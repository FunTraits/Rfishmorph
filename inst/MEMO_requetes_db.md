# Mémo — interroger la base FISHMORPH avec `DBI::dbGetQuery()`

Toutes les requêtes de ce mémo ont été exécutées sur DuckDB 1.5.5 contre une base
construite par `fishmorph_build_db()` ; les sorties montrées sont réelles.

---

## 1. Les trois gestes

```r
library(DBI)
source("Other_scripts/fishmorph_landmark_store.R")
source("Other_scripts/fishmorph_db.R")

con <- fishmorph_db_connect("FishMORPH/fishmorph.duckdb")   # read_only = TRUE par défaut
res <- DBI::dbGetQuery(con, "SELECT * FROM v_ratios LIMIT 5")
DBI::dbDisconnect(con, shutdown = TRUE)
```

| Fonction | Usage |
|---|---|
| `dbGetQuery()` | `SELECT` — renvoie un `data.frame` |
| `dbExecute()` | `CREATE` / `INSERT` / `UPDATE` — renvoie un nombre de lignes |
| `dbListTables()` | inventaire des tables et vues |
| `dbGetQuery(con, "DESCRIBE v_ratios")` | colonnes et types d'une vue |

Trois règles d'hygiène :

- **Ouvrir en lecture seule.** C'est le défaut, et ce n'est pas une précaution
  cosmétique : `read_only = TRUE` autorise plusieurs processus R à ouvrir le même
  fichier simultanément, alors qu'une connexion en écriture le verrouille.
- **Ne jamais écrire dans la base.** Elle est dérivée des journaux ; la prochaine
  exécution de `fishmorph_build_db()` effacera toute modification manuelle. Une
  correction se fait dans l'app, donc dans le journal.
- **Toujours `shutdown = TRUE`** à la déconnexion, sinon l'instance DuckDB reste
  en mémoire et le fichier reste verrouillé pour la session R.

---

## 1 bis. Le premier réflexe : qu'y a-t-il dans ma base ?

**Une requête qui renvoie 0 ligne est presque toujours une clé qui n'existe pas**,
pas une requête fausse. Commencer par lister ce qui est réellement là :

```r
DBI::dbGetQuery(con, "SELECT specimen_id, species, mode FROM v_specimen_qc ORDER BY specimen_id")
```

```
specimen_id              species                  mode
Aaptosyax.grypus         Aaptosyax.grypus         correct
Abbottina.obtusirostris  Abbottina.obtusirostris  correct
Abbottina.rivularis      Abbottina.rivularis      correct
Coilia.borneensis        Coilia.borneensis        reconstruct
...
```

### `specimen_id` n'a pas la même forme selon le mode

C'est la source d'erreur numéro un :

| Mode de saisie | `specimen_id` | Exemple |
|---|---|---|
| `reconstruct`, `correct` | **nom d'espèce du classeur**, avec un point | `Coilia.nasus` |
| `new` | **nom du fichier photo**, extension comprise | `Abramis_brama_2.JPG` |

En modes `reconstruct` et `correct`, la ligne du classeur est identifiée par
l'espèce ; il n'y a pas de fichier photo qui fasse office de clé. Chercher
`WHERE specimen_id = 'Coilia nasus'` (espace au lieu du point) renvoie 0 ligne
sans le moindre message d'erreur — SQL n'a aucun moyen de deviner l'intention.

En cas de doute, ne pas deviner : chercher.

```r
DBI::dbGetQuery(con, "SELECT DISTINCT specimen_id FROM specimen WHERE specimen_id ILIKE ?",
                params = list("%coilia%"))
```

---

## 2. Carte du schéma

**Tables** (le stockage)

| Table | Grain | Colonnes clés |
|---|---|---|
| `record` | un clic sur « Enregistrer » | `record_id` (PK), `ts`, `operator`, `mode` |
| `specimen` | un individu digitalisé | `specimen_id` (PK), `species`, `photo_file`, `img_w/h`, `mm_per_px` |
| `landmark_obs` | **un point** | `(specimen_id, landmark)` (PK), `x`, `y`, `status` |

**Vues** (les calculs, recalculés à chaque requête donc jamais périmés)

| Vue | Contenu |
|---|---|
| `v_landmarks_wide` | une ligne par spécimen, colonnes `"1_X"`, `"1_Y"` … `"25_Y"` |
| `v_ratios` | `Bl_px`, `Bl_mm`, et les 10 rapports segment/Bl (`Bd`, `Hd`, `Ed`…) |
| `v_specimen_qc` | `n_placed`, `n_seeded`, `n_derived`, `n_na`, `a_echelle` |

`status` prend quatre valeurs : `placed` (posé à la main), `seeded` (**resté à la
position de graine, donc jamais vérifié**), `derived` (calculé : 8, 9, 11, 15, 23),
`na` (déclaré non mesurable).

---

## 3. Anatomie de la requête d'exemple

```sql
SELECT r.specimen_id, r.species, r.Ed, q.n_seeded   -- (1) colonnes voulues
FROM v_ratios r                                     -- (2) source, alias « r »
JOIN v_specimen_qc q USING (specimen_id)            -- (3) jointure sur la clé commune
WHERE r.Ed > 0.1386 OR q.n_seeded > 3               -- (4) filtre
ORDER BY q.n_seeded DESC                            -- (5) tri
```

1. `r.Ed` = le diamètre oculaire rapporté à la longueur du corps.
2. L'alias `r` évite de réécrire `v_ratios` partout et lève l'ambiguïté :
   `specimen_id` et `species` existent dans les **deux** vues.
3. `USING (specimen_id)` est l'écriture courte de
   `ON r.specimen_id = q.specimen_id`, possible parce que la colonne porte le même
   nom des deux côtés.
4. `0.1386` est le quantile 99,9 % de `Ed/Bl` sur les 9556 espèces du référentiel
   (`.FM_RATIO_BOUNDS`). Au-delà, l'œil est plus grand que chez 999 espèces sur
   1000 — suspect, pas impossible.
5. `DESC` = décroissant.

### Le piège de cette requête

`v_ratios` se termine par `WHERE Bl_px > 0`. Un spécimen dont **LM1 ou LM2
manque** n'a pas de `Bl_px`, donc **n'apparaît pas dans `v_ratios`** — et la
jointure interne (`JOIN`) l'élimine de la requête. Les spécimens les plus abîmés
sont donc précisément ceux que ce contrôle ne voit pas.

Vérifié sur un jeu de quatre spécimens dont un sans LM1/LM2 :

```
requête d'origine              →  rutilus_1.jpg (n_seeded=6), brama_2.jpg (Ed=0.150)
                                  perca_1.jpg  ABSENT
```

Version corrigée — on part de `v_specimen_qc`, qui contient *tous* les spécimens,
et on ajoute les motifs du signalement :

```r
sql <- "
SELECT q.specimen_id, q.species, r.Ed, q.n_seeded,
       CASE WHEN r.Ed > 0.1386      THEN 'oeil hors enveloppe' END       AS motif_ratio,
       CASE WHEN q.n_seeded > 3     THEN 'points non verifies' END       AS motif_saisie,
       CASE WHEN r.specimen_id IS NULL THEN 'axe degenere (LM1/LM2)' END AS motif_axe
FROM v_specimen_qc q
LEFT JOIN v_ratios r USING (specimen_id)
WHERE r.Ed > 0.1386 OR q.n_seeded > 3 OR r.specimen_id IS NULL
ORDER BY q.n_seeded DESC, q.specimen_id"
DBI::dbGetQuery(con, sql)
```

```
specimen_id    species            Ed      n_seeded  motif_ratio          motif_saisie         motif_axe
rutilus_1.jpg  Rutilus rutilus    0.0590  6         NULL                 points non verifies  NULL
brama_2.jpg    Abramis brama      0.1500  0         oeil hors enveloppe  NULL                 NULL
perca_1.jpg    Perca fluviatilis  NULL    0         NULL                 NULL                 axe degenere (LM1/LM2)
```

Les colonnes `motif_*` valent leur libellé ou `NULL` : on lit d'un coup d'œil
quel critère a déclenché chaque ligne, ce que le `OR` seul ne dit pas.

---

## 4. Recettes

### Accéder à une espèce

**Toujours passer la valeur en paramètre**, jamais par `paste0()`. Le `?` est
remplacé par le moteur, ce qui gère les apostrophes et les caractères spéciaux
sans que tu aies à y penser.

```r
DBI::dbGetQuery(con,
  "SELECT specimen_id, round(Bl_px,1) AS Bl_px, round(Bd,3) AS Bd, round(Ed,3) AS Ed
     FROM v_ratios WHERE species = ?",
  params = list("Coilia.nasus"))
```

```
specimen_id   Bl_px   Bd     Ed
Coilia.nasus  551.1   0.195  0.021
```

Recherche partielle, insensible à la casse (`ILIKE`, `%` = n'importe quelle suite).
Indispensable quand on ne se rappelle pas la forme exacte de la clé :

```r
DBI::dbGetQuery(con,
  "SELECT specimen_id, round(Bl_px,1) AS Bl_px, round(Bd,3) AS Bd, round(Ed,3) AS Ed
     FROM v_ratios WHERE species ILIKE ? ORDER BY specimen_id",
  params = list("coilia%"))
```

```
specimen_id          Bl_px   Bd     Ed
Coilia.borneensis    539.1   0.233  0.045
Coilia.dussumieri    465.3   0.268  0.036
Coilia.lindmani      519.0   0.243  0.044
Coilia.macrognathos  436.7   0.238  0.027
Coilia.mystus        534.2   0.176  0.038
Coilia.nasus         551.1   0.195  0.021
```

Plusieurs espèces à la fois — on fabrique autant de `?` que de valeurs :

```r
sp  <- c("Coilia.nasus", "Congothrissa.gossei")
sql <- sprintf("SELECT * FROM v_ratios WHERE species IN (%s)",
               paste(rep("?", length(sp)), collapse = ", "))
DBI::dbGetQuery(con, sql, params = as.list(sp))
```

### Un spécimen précis, coordonnées brutes

Les colonnes commençant par un chiffre **exigent des guillemets doubles** —
`"13_X"`, pas `'13_X'` ni `13_X`.

```r
DBI::dbGetQuery(con,
  'SELECT specimen_id, "13_X", "13_Y", "14_X", "14_Y"
     FROM v_landmarks_wide WHERE specimen_id = ?',
  params = list("Coilia.nasus"))
```

Utiliser des apostrophes autour de la chaîne R permet d'écrire les guillemets
doubles SQL sans échappement.

### Format long, pour `geomorph` / `intraitR`

```r
lg <- DBI::dbGetQuery(con,
  "SELECT specimen_id, landmark, x, y, status FROM landmark_obs
    WHERE specimen_id = ? ORDER BY landmark", params = list("Coilia.nasus"))
```

```
specimen_id   landmark  x        y        status
Coilia.nasus  1          19.964  142.389  placed
Coilia.nasus  2         570.980  152.656  placed
Coilia.nasus  3         148.213  100.631  placed
Coilia.nasus  4         146.025  207.986  placed
...
```

### Quels points n'ont jamais été vérifiés ?

`list()` est l'agrégat DuckDB qui rassemble des valeurs en vecteur — pratique
pour obtenir une ligne par spécimen plutôt qu'une par point :

```r
DBI::dbGetQuery(con, "
  SELECT specimen_id, list(landmark ORDER BY landmark) AS points_seeded
  FROM landmark_obs WHERE status = 'seeded' GROUP BY 1 ORDER BY 1")
```

```
specimen_id          points_seeded
Coilia.borneensis    [13, 14]
Coilia.dussumieri    [13, 14]
...
```

### Agrégation par espèce

```r
DBI::dbGetQuery(con, "
  SELECT species, count(*) AS n,
         round(avg(Bd), 4) AS Bd_moy, round(stddev_samp(Bd), 4) AS Bd_et
  FROM v_ratios GROUP BY ALL ORDER BY n DESC")
```

`GROUP BY ALL` est un raccourci DuckDB : il groupe sur toutes les colonnes non
agrégées, ici `species`. Plus court et impossible à désynchroniser du `SELECT`.

### Spécimens exclus d'une vue (anti-jointure)

```r
DBI::dbGetQuery(con, "
  SELECT specimen_id, species FROM specimen
  WHERE specimen_id NOT IN (SELECT specimen_id FROM v_ratios)")
```

---

## 5. Les quatre pièges

### Guillemets

| | Rôle | Exemple |
|---|---|---|
| `'...'` | une **valeur** texte | `WHERE species = 'Abramis brama'` |
| `"..."` | un **identifiant** (colonne, table) | `SELECT "13_X"` |

Inverser les deux donne des erreurs déroutantes : `WHERE species = "Abramis brama"`
cherche une *colonne* nommée `Abramis brama`.

### `NULL` n'est pas une valeur

`NULL` signifie « inconnu ». Toute comparaison avec `NULL` vaut `NULL`, jamais
`TRUE` — donc la ligne est écartée.

```r
# 3 spécimens ont mm_per_px ≠ 0.0125... mais un seul est NULL et disparaît :
"WHERE mm_per_px <> 0.0125"                        # → 2 lignes
"WHERE mm_per_px <> 0.0125 OR mm_per_px IS NULL"   # → 3 lignes
```

Toujours `IS NULL` / `IS NOT NULL`, jamais `= NULL`. Côté R, ces `NULL` arrivent
en `NA`, ce qui est le comportement attendu.

### `AND` l'emporte sur `OR`

`WHERE a OR b AND c` se lit `a OR (b AND c)`. Parenthèse systématiquement dès que
tu mélanges les deux — c'est le même piège que `%in%` combiné à l'arithmétique en R.

### Jointure interne silencieuse

Vu plus haut : un `JOIN` élimine sans prévenir les lignes absentes d'un côté.
Quand la question est « qu'est-ce qui cloche ? », partir de la vue **la plus
complète** (`v_specimen_qc` ou `specimen`) et joindre le reste en `LEFT JOIN`.

---

## 6. Raccourcis DuckDB utiles

```sql
SELECT * EXCLUDE (Bl_px, Bd, Hd, Eh2, Mo2, PFi2, PFl, Jl, CPd, CFd) FROM v_ratios;
SELECT * REPLACE (round(Ed, 3) AS Ed) FROM v_ratios;
SUMMARIZE v_ratios;      -- min, max, quartiles, % de NULL, par colonne
DESCRIBE v_ratios;       -- noms et types
FROM v_ratios LIMIT 5;   -- le SELECT * est facultatif
```

`SUMMARIZE` est le premier réflexe à avoir sur une base fraîchement reconstruite :
il donne la distribution de chaque ratio et le taux de valeurs manquantes.

---

## 7. Sans écrire de SQL : `dbplyr`

Le même filtrage en syntaxe `dplyr`, exécuté **dans DuckDB** — rien n'est chargé
en mémoire avant `collect()` :

```r
library(dplyr)
tbl(con, "v_specimen_qc") |>
  left_join(tbl(con, "v_ratios"), by = "specimen_id") |>
  filter(Ed > 0.1386 | n_seeded > 3) |>
  arrange(desc(n_seeded)) |>
  collect()
```

`show_query()` à la place de `collect()` affiche le SQL généré — utile pour
apprendre la traduction.

---

## 8. Sans base du tout

Les exports Parquet sont interrogeables directement, sans construire ni ouvrir la
base — pratique pour un collaborateur qui n'a que les fichiers :

```r
con <- DBI::dbConnect(duckdb::duckdb())          # base en mémoire, vide
DBI::dbGetQuery(con, "SELECT specimen_id, Bd FROM 'FishMORPH/exports/ratios.parquet'
                       WHERE species = ?", params = list("Abramis brama"))
```

---

## 9. Une mise en garde d'interprétation

En mode `reconstruct`, les points sont pré-placés **à partir des segments mesurés
du classeur**. Tant qu'un point reste au statut `seeded`, sa position n'a pas été
corrigée : le ratio calculé par `v_ratios` ne fait alors que **reproduire le
segment d'entrée**. Le confronter à l'enveloppe FISHMORPH ne valide rien — c'est
circulaire.

Constaté sur les données réelles, en comparant `v_ratios` aux segments de
`FishMORPH_seg.csv` :

| Espèce | Segment | Cible (classeur) | Mesuré | Écart | Statut des points |
|---|---|---|---|---|---|
| *Coilia.nasus* | Ed | 0.0214 | 0.0210 | −1.9 % | `seeded` |
| *Coilia.dussumieri* | Ed | 0.0361 | 0.0360 | −0.3 % | `seeded` |
| *Coilia.dussumieri* | Hd | 0.0650 | 0.1060 | **+63 %** | `placed` |
| *Coilia.dussumieri* | Bd | 0.2096 | 0.2680 | **+28 %** | `placed` |
| *Aaptosyax.grypus* | Bd | 0.2184 | 0.2280 | +4.4 % | `placed` (mode `correct`) |

Les écarts quasi nuls sur `Ed` ne signalent pas une excellente digitalisation :
ils signalent que LM13 et LM14 n'ont **pas été touchés**. Inversement, les écarts
de +28 % et +63 % sur *C. dussumieri* sont de vrais désaccords entre ta
digitalisation et les segments de référence — c'est là qu'il faut regarder.

**Conséquence pratique :** filtrer sur `status` avant toute analyse de validation.

```r
DBI::dbGetQuery(con, "
  SELECT r.specimen_id, round(r.Ed,4) AS Ed, q.n_seeded
  FROM v_ratios r JOIN v_specimen_qc q USING (specimen_id)
  WHERE q.n_seeded = 0")     -- seulement les spécimens intégralement vérifiés
```

---

## Aide-mémoire

```r
con <- fishmorph_db_connect("FishMORPH/fishmorph.duckdb")
DBI::dbListTables(con)
DBI::dbGetQuery(con, "DESCRIBE v_ratios")
DBI::dbGetQuery(con, "SUMMARIZE v_ratios")
DBI::dbGetQuery(con, "SELECT * FROM v_ratios WHERE species = ?", params = list("..."))
DBI::dbDisconnect(con, shutdown = TRUE)
```
