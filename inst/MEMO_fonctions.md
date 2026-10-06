# Mémo — fonctions du package `Rfishmorph`

Version 0.2.0 · **119 fonctions** : 60 exportées, 59 internes (préfixe `.`), plus
8 méthodes S3. Les signatures ci-dessous sont extraites du code source, pas
recopiées à la main.

---

## Le flux complet, dans l'ordre

```
    photos (locales)
         │
         ▼
 launch_fishmorph_digitizer()          ← saisie des 21 landmarks
         │  écrit à chaque « Enregistrer »
         ▼
    journal TSV append-only            ← SOURCE DE VÉRITÉ, jamais réécrite
         │
         ├─► fm_journal_status()       ← diagnostic : qu'y a-t-il dedans ?
         ├─► fishmorph_consolidate()   ← tableau analysable
         ├─► fishmorph_journal_qc()    ← points jamais vérifiés, manquants
         │
         ▼
 fishmorph_build_db()                  ← base DuckDB dérivée + exports
         │
         ├─► fishmorph_validate()      ← contrôle structurel ET morphométrique
         ├─► fishmorph_db_connect()    ← requêtes SQL / dbplyr
         │
         ▼
    Parquet / CSV  ──►  launch_fishmorph_space()   ← exploration
```

Règle qui gouverne toute l'architecture : **le journal est la seule couche
durable**. Le classeur, la base et les exports sont des artefacts *dérivés*,
reconstructibles en quelques secondes. Ne jamais écrire directement dans la base.

---

## 1. Applications

| Fonction | Signature abrégée |
|---|---|
| `launch_fishmorph_digitizer()` | `(xlsx_path, photo_dir, out_path, seg_sheet, lm_sheet, new_sheet, new_photo_dir, ruler_mm, journal_dir, operator, xlsx_flush_every, mode)` |
| `launch_fishmorph_space()` | `(data = NULL, launch.browser = TRUE, ...)` |
| `fishmorph_space_data()` | `()` — chemin du tableau embarqué |
| `launch_fishmorph_reconstructor()` | `(segments_csv = NULL, ...)` — **déprécié**, redirige |

`mode` du digitizer : `"reconstruct"` (espèces sans landmarks, amorcées depuis
les segments mesurés) · `"correct"` (déjà landmarkées, rechargées du classeur) ·
`"new"` (photos absentes du classeur, amorcées depuis les **proportions médianes**
des 9556 espèces — aucune longueur n'est verrouillée dans ce mode).

Les photographies restent **en local** : seuls leur nom de fichier et leurs
dimensions en pixels entrent dans le journal.

---

## 2. Journal — capture

| Fonction | Signature |
|---|---|
| `fm_journal_open()` | `(journal_dir, operator = NULL, app_version = NA_character_)` |
| `fm_journal_append()` | `(jr, row_key, coords, points, status = NULL, species, photo_file, mode, target_sheet, img_w, img_h, ruler_mm, mm_per_px)` |
| `fm_journal_read()` | `(journal_dir)` |
| `fm_journal_status()` | `(journal_dir)` — **le premier réflexe** si une consolidation est vide |
| `fm_journal_history()` | `(journal_dir, row_key = NULL)` |
| `fishmorph_consolidate()` | `(journal_dir, long = FALSE, drop_na_points = TRUE, out_csv = NULL, out_xlsx = NULL)` |
| `fishmorph_journal_qc()` | `(journal_dir, expect = c(1:19, 22L, 23L))` |
| `fm_save_workbook_atomic()` | `(wb, path, keep_prev = TRUE)` |

**Le `status` de chaque point** — l'information qu'un tableau large de
coordonnées ne peut pas porter :

| Statut | Sens | Conséquence |
|---|---|---|
| `placed` | posé ou déplacé à la main | mesure |
| `seeded` | **encore à sa position de graine** | jamais vérifié à l'œil |
| `derived` | calculé (8, 9, 11, 15, 23) | dépend d'autres points |
| `na` | déclaré non mesurable | absent, volontairement |

**Piège d'interprétation.** En mode `reconstruct`, un point `seeded` est resté
exactement là où les segments d'entrée l'ont mis : son ratio *reproduit* la
donnée d'entrée. Le confronter à l'enveloppe FISHMORPH est circulaire. Filtrer
sur `status` avant toute analyse de validation.

---

## 3. Base de données — dérivée

| Fonction | Signature |
|---|---|
| `fishmorph_build_db()` | `(journal_dir, db_path = NULL, export_dir = NULL, validate = TRUE, stop_on_error = FALSE)` |
| `fishmorph_db_connect()` | `(db_path, read_only = TRUE)` |
| `fishmorph_validate()` | `(x, expect = c(1:19, 22L, 23L), bounds = .FM_RATIO_BOUNDS)` |

Tables : `record` · `specimen` · `landmark_obs`.
Vues : `v_landmarks_wide` · `v_ratios` · `v_specimen_qc`.

Sévérités de `fishmorph_validate()` : `erreur` (incohérence certaine — point hors
de l'image, points confondus, axe dégénéré) · `avertissement` (proportion hors de
l'enveloppe des 9556 espèces : spécimen **à regarder**, pas à rejeter) · `info`
(traçabilité).

Détail des requêtes SQL : voir `inst/MEMO_requetes_db.md`.

---

## 4. Schéma et calcul des traits

| Fonction | Signature |
|---|---|
| `fishmorph_schema()` | `()` |
| `fishmorph_segment_names()` | `()` — les 11 segments |
| `fishmorph_ratio_names()` | `()` — les 9 ratios |
| `fishmorph_landmark_pairs()` | `()` — segment → paire de landmarks |
| `fishmorph_segments()` | `(x, scale_cm = NULL, na.rm = TRUE)` |
| `fishmorph_ratios()` | `(x, scale_cm = NULL)` |
| `fishmorph_traits()` | `(x, scale_cm = NULL)` — segments + ratios |

---

## 5. Landmarks — conteneur et entrée/sortie

| Fonction | Signature |
|---|---|
| `fishmorph_landmarks()` | `(coords, metadata = NULL, scale = NULL, specimen = NULL, pad_to = 21L)` |
| `is_fishmorph_landmarks()` | `(x)` |
| `as_fishmorph_landmarks()` | `(x, ...)` |
| `as_intrait_landmarks()` | `(x)` |
| `read_landmarks_csv()` | `(file, sep = ",", dec = ".", id_col = NULL)` |
| `write_landmarks_csv()` | `(x, file, cartesian = TRUE, image_height = NULL)` |

---

## 6. Géométrie et reconstruction

| Fonction | Signature |
|---|---|
| `standardize_geometry()` | `(x, orient = TRUE, rescale = FALSE, dorsal_up = FALSE)` |
| `correct_geometry_conventions()` | `(x, tolerance_coord = 1e-6, straighten = TRUE)` |
| `correct_zero_ratio_landmarks()` | `(landmarks, reference, id_col = NULL, ratios = c("OGp","PFv"), tol = 0)` |
| `correct_landmark_order()` | `(landmarks, chains, report = TRUE)` |
| `reconstruct_fishmorph_landmarks()` | `(segments, anchor_snout, anchor_caudal, px_per_cm = 50, params = list(), scale_cm = 1, ...)` |
| `fishmorph_reconstruction_defaults()` | `(preset = c("template", "app"))` |
| `check_reconstruction_roundtrip()` | `(segments = NULL, tol = 1e-6, ...)` |

**Garantie d'aller-retour** : `reconstruct_fishmorph_landmarks()` est l'inverse de
`fishmorph_segments()`. Les paramètres libres changent *où* les landmarks se
placent, jamais les *longueurs* des segments.

---

## 7. Contrôle qualité

| Fonction | Signature |
|---|---|
| `agreement_metrics()` | `(x, y)` |
| `compare_segments_landmarks()` | `(landmarks, published, id_col, variant, space, group_col, ...)` |
| `plot_segment_landmark_agreement()` | `(x, type = c("scatter","bar","space"), style, ...)` |
| `check_infinite_ratios()` | `(data, z_thresh = 8)` |
| `check_geometry_conventions()` | `(x, tolerance = 0)` |
| `check_landmark_order()` | `(landmarks, chains)` |

---

## 8. Espace fonctionnel

| Fonction | Signature |
|---|---|
| `load_fishmorph_reference()` | `(file = NULL, sheet = NULL)` — `NULL`/`"full"` = 8970 esp., `"sample"` = 400 |
| `fishmorph_trait_space()` | `(data, traits, groups = NULL, log = FALSE, scale = TRUE, na_action, ...)` |
| `project_fishmorph()` | `(specimens, reference = NULL, traits, groups, select_species, ...)` |
| `group_colors()` · `reset_group_colors()` | `(x)` · `()` |

**Échelle des traits.** Les tables embarquées sont **déjà** en `log10(x + 1)`.
Leur appliquer `fishmorph_trait_space(log = TRUE)` logarithmerait deux fois. Les
ratios recalculés par `fishmorph_ratios()` sont sur échelle brute et, eux, ont
besoin de la transformation.

---

## 9. Imputation et phylogénie

| Fonction | Signature |
|---|---|
| `impute_traits()` | `(data, cols, method = c("missforest_phylo","missforest","impute_group_mean","impute_mean"), groups, tree, missforest_phylo_k = 10, phylo_axes = NULL, ...)` |
| `impute_landmarks()` | `(landmarks, method = c("tps","regression","impute_mean","impute_group_mean","missforest","missforest_phylo"), groups, tree, missforest_phylo_k, phylo_axes, ...)` |
| `load_fishmorph_phylo_axes()` | `(file = NULL, k = NULL, refresh = FALSE)` — **axes précalculés** |
| `phylo_pcoa()` | `(tree, species, k, correction, ultrametric = TRUE, ...)` — recalcul depuis un arbre |
| `load_fishmorph_phylogeny()` | `()` |

**Les axes phylogénétiques sont précalculés.** `"missforest_phylo"` lit
`pcoaPhylogenyFish.rds` (8970 espèces, 10 axes, 540 Ko), mis en cache une fois
par session ; le format texte reste accepté, l'aiguillage se fait sur
l'extension. La raison n'est pas seulement le coût — l'eigendécomposition d'une
matrice patristique 8970 × 8970 est cubique — mais la **comparabilité** : des
axes recalculés sur le sous-ensemble d'espèces présent définiraient un repère
différent à chaque analyse, et deux imputations ne vivraient pas dans le même
espace phylogénétique.

Ordre de priorité des sources : `phylo_axes` fourni → `tree` fourni (recalcul) →
table précalculée (défaut) → arbre embarqué (repli). Le message d'imputation
nomme la source retenue.

---

## 10. Taxonomie (FishBase)

| Fonction | Signature |
|---|---|
| `validate_species_names()` | `(species, verbose = TRUE)` |
| `update_fishmorph_taxonomy()` | `(data, species_col = NULL, add_classification = TRUE)` |
| `freshwater_fish_list()` | `(include_brackish = FALSE, fields = NULL)` |
| `new_fishmorph_species()` | `(species, landmarks, segments, ratios, scale_cm = 1, family, order, genus, ...)` |
| `add_fishmorph_species()` | `(reference, new_records, species_col = "Species", overwrite = FALSE)` |

---

## 11. Graphiques

| Fonction | Signature |
|---|---|
| `plot_landmarks()` | `(x, specimen = 1, label_points = TRUE, segments = TRUE, engine, ...)` |
| `plot_trait_space()` | `(x, style = c("hull","spider","density","none"), axes = c(1,2), ...)` |
| `plot_trait_distributions()` | `(data, traits, highlight = NULL, log = FALSE, engine, ...)` |
| `plot_functional_space()` | `(x, source = c("segment","landmark"), groups, reference, axes, hull, density, ...)` |

**Méthodes S3** : `print()` et `plot()` pour `fishmorph_landmarks`,
`fishmorph_trait_space`, `fishmorph_projection` ; `print()` pour
`fishmorph_phylopcoa` ; `summary()` pour `fishmorph_comparison`.

---

## Fonctions internes, par fichier

Non exportées, préfixées par un point. Répertoriées ici parce que c'est là que
vivent les règles les plus délicates.

| Fichier | Nombre | Principales |
|---|---|---|
| `digitizer-app.R` | 14 | `.fm_place`, `.fm_constrain`, `.fm_point23`, `.fm_apply_conventions`, `.fm_correct_via_package`, `.fm_axis_chain` |
| `plot-ordination.R` | 11 | `.plot_ordination`, `.kde2d`, `.density_field`, `.covariance_ellipse` |
| `impute.R` | 5 | `.apply_na_action`, `.phylo_axes_for_groups` |
| `journal.R` | 3 | `.fm_iso_now`, `.fm_tsv_safe`, `.fm_journal_empty` |
| `database.R` | 4 | `.fm_create_views`, `.fm_export`, `.fm_sql_str` |
| `class-landmarks.R` | 3 | `.landmarks_from_long`, `.fm_coords` |
| autres | 19 | — |

---

## Deux dettes techniques identifiées

**1. La géométrie du digitizer n'est ni testée ni documentée.** `digitizer-app.R`
concentre 14 fonctions internes qui portent les règles géométriques les plus
délicates du package — placement depuis les segments, édition contrainte, axe
brisé, point dérivé 23. Elles gagneraient à migrer vers `geometry.R` avec des
tests unitaires.

**2. Une duplication assumée mais non vérifiée.** `.fm_apply_conventions()`
réimplémente ce que `correct_geometry_conventions()` fait déjà. C'est délibéré —
la version locale est plus rapide pour l'édition interactive — et
`.fm_correct_via_package()` existe pour arbitrer entre les deux. Mais aucun test
ne vérifie qu'elles coïncident, alors que c'est exactement le type de duplication
qui finit par diverger silencieusement.

---

## Constantes internes utiles

| Constante | Contenu |
|---|---|
| `.FM_LM_PTS` | les 21 landmarks enregistrés : `1:19`, `22`, `23` |
| `.FM_DERIVED` | points calculés : `8, 9, 11, 15, 23` |
| `.FM_HINGES` | charnières de l'axe brisé : `22, 24, 25` |
| `.FM_SCALE_PTS` | barre d'échelle : `20, 21` |
| `.FM_NEW_RATIOS` | proportions médianes segment/Bl (mode `"new"`) |
| `.FM_RATIO_BOUNDS` | enveloppe empirique, quantiles 0.001 et 0.999 |
| `.FM_JOURNAL_COLS` | les 17 colonnes du journal |
