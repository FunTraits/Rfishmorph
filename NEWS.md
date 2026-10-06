# Rfishmorph 0.13.1

## La taxonomie n'est plus soumise a la priorite des magasins

* `build_fishmorph_landmark_table()` prenait `Family` et `Order` sur la LIGNE
  gagnante du dedoublonnage. Or le duckdb, bati sur un journal qui n'enregistre
  qu'un nom d'espece, n'en porte aucune (`.fm_lm_from_duckdb()` les met a `NA`
  par construction) : une espece que la jointure de metadonnees ne retrouvait
  pas sortait donc SANS famille ni ordre des qu'elle avait ete re-digitalisee.
  Plus le travail etait recent, plus la perte etait certaine.
* La priorite arbitre une MESURE : de deux configurations d'une meme espece, la
  plus recente gagne. Une famille n'est pas une mesure -- elle est la meme quel
  que soit le magasin lu. La geometrie vient donc de la ligne gagnante, et la
  taxonomie du magasin le mieux classe qui en porte une (`tax_fill`).
* Effet mesure sur la table du 3 aout : **160 especes sans `Family` ni `Order`,
  dont 159 recuperees** par le correctif (la 160e, *Abramis sapa*, n'a de
  famille dans aucun magasin). Toutes portaient `store = duckdb`.
* `IUCN` suit la meme regle et est desormais LU dans le classeur quand la
  feuille porte la colonne. Le digitizer cree cette colonne sur la feuille des
  nouveaux specimens : aucun landmark ne donne un statut de menace, il ne peut
  qu'etre SAISI, et cette feuille est le seul endroit ou il puisse l'etre pour
  une espece absente de la table segments.

# Rfishmorph 0.13.0

## Un niveau GENRE dans l'explorateur de bassins

* La cascade taxonomique de la barre laterale passe de trois niveaux a quatre :
  ordre > famille > **genre** > espece, couples dans les deux sens comme les
  autres. C'est le niveau auquel une question morphologique se pose le plus
  souvent -- « ce genre occupe-t-il une region de l'espace ou s'y disperse-t-il »
  -- et il fallait jusqu'ici selectionner la famille puis lire les noms un a un
  dans le tableau de composition.
* Le genre n'est pas un attribut externe comme l'ordre ou la famille : c'est le
  premier mot du binome. La colonne `genus` du cache est utilisee quand elle
  est renseignee, et le NOM repond quand elle ne l'est pas -- le cas de toute
  espece ajoutee depuis la publication, dont la jointure de metadonnees n'a
  rien trouve. Un niveau troue serait pire que pas de niveau : la boite
  laisserait tomber en silence exactement les especes qu'on l'ouvre pour
  chercher. Sur la table landmark actuelle les deux sources donnent 1 312
  genres et ne se contredisent sur aucune espece.
* Un genre a cheval sur deux familles (5 dans la table actuelle : *Alestes*,
  *Brycinus*, *Epiplatys*, *Fundulopanchax*, *Myleus*) est rattache a sa
  famille MODALE, par la meme fonction et la meme regle qu'une famille a cheval
  sur deux ordres. La hierarchie repond donc pareil dans la barre laterale et
  sur la carte.
* Le genre entre dans le niveau operatoire (`taxon_rows`) : selectionne, c'est
  lui qui commande les bassins allumes, la restriction de l'assemblage et
  l'etiquette des panneaux -- le plus precis l'emporte, comme avant. Le chemin
  affiche sous les boites, le lien « vider » et le bouton de reinitialisation
  suivent. Le panneau specimen affiche desormais ordre · famille · genre · IUCN.
* La boite est envoyee au navigateur en entier, comme celles des ordres et des
  familles : le couplage ascendant depuis l'espece SELECTIONNE un genre, et un
  selectize cote serveur tiendrait alors une valeur qu'il n'a jamais recue.
  Moins de deux mille options, quelques dizaines de kilo-octets ; c'est le
  registre des especes, cinq fois plus grand, qui doit rester cote serveur.

## MBl / MBw depuis FishBase, dans le constructeur du tableau

* `fishmorph_fishbase_size()` (nouveau, exporte) rend la longueur STANDARD
  maximale en cm et la masse maximale en g -- les deux quantites brutes
  derriere `MBl` et `MBw`, que les tables stockent en `log10(x + 1)`.
* `build_fishmorph_landmark_table(fishbase_size = TRUE)` s'en sert pour combler
  ce que la table segments ne peut pas fournir. Une espece digitalisee depuis
  la publication en est absente par construction : elle sortait sans taille, et
  le `complete.cases()` de `prepare_fishmorph_basins()` sur les ratios ET
  `size_traits` la faisait alors sortir de l'espace fonctionnel. Forme mesuree,
  aucun point sur la carte.
* Nouvelle colonne `size_source` (`"fishmorph_publi"` / `"fishbase"`). Sans
  elle, une taille derivee devient indiscernable d'une taille publiee des la
  lecture suivante, et l'origine d'un point de l'ACP n'est plus retracable.
  Une valeur existante n'est jamais ecrasee.

### Le type de longueur, et pourquoi il n'est pas ignore

* `species()$Length` est une longueur maximale dans le type que donne
  `LTypeMaxM`, le plus souvent TL. La colonne publiee de FISHMORPH est une
  longueur STANDARD. Y verser une longueur totale la gonflerait de 10 a 25 %,
  biais systematique qu'aucune lecture du fichier ne revelerait.
* Les types non SL sont donc CONVERTIS par la table LENGTH-LENGTH plutot
  qu'ecartes. Une conversion dont le rapport SL/L sort de `[0,5 ; 1]` est
  refusee : une espece revient avec `NA` plutot qu'avec un nombre que personne
  ne peut controler.
* L'ORIENTATION de cette table est MESUREE, pas supposee. Le manuel FishBase
  pose `Length2 = a + b * Length1` ; rfishbase porte depuis 2017 un
  signalement ouvert d'inversion des deux champs
  (`ropensci/rfishbase#119`), et fishbase.org nomme les memes colonnes
  "unknown" et "known". Comme une longueur standard est plus courte qu'une
  longueur totale, l'orientation dont les pentes sortent inferieures a 1 est la
  bonne : `.fm_fb_sl_direction()` la determine sur les donnees telechargees et
  l'annonce. Si une version future de rfishbase remet les colonnes a
  l'endroit, le calibrage suit tout seul. Orientation ambigue = aucune
  conversion.
* Coefficients `a`/`b` : `length_weight()` et l'etude au plus fort
  `CoeffDetermination`, comme la table publiee ; a cela pres qu'une regression
  de type SL est preferee quand la colonne `Type` existe, pour que la longueur
  entree dans `a * L^b` soit celle pour laquelle les coefficients ont ete
  ajustes. `lw_type` dit laquelle a servi.
* Masse : le poids maximal publie quand FishBase en a un, `a * L^b` sinon --
  l'ordre de la table publiee. `weight_source` distingue les deux : un maximum
  observe et un maximum derive ne sont pas la meme quantite.

### Et ce que FishBase ne sait pas non plus : `impute_size`

* `build_fishmorph_landmark_table(impute_size = TRUE)`, actif par defaut,
  impute les `MBl`/`MBw` encore manquants apres la jointure et apres FishBase,
  avec le meme `na_action` que les ratios.
* C'est une SECONDE passe, et non les neuf ratios et les deux tailles dans une
  seule matrice. Une passe conjointe laisserait `MBl` et `MBw` predire les
  ratios, ce qui changerait la valeur imputee de toute espece partiellement
  mesuree deja presente : une revision silencieuse de nombres publies. Ici les
  ratios -- complets a ce stade -- et les axes phylogenetiques predisent la
  taille, et rien ne predit les ratios en retour. Strictement additif, et
  l'information allometrique circule quand meme dans le sens demande.
* `"keep"` et `"omit"` ne sont pas honores pour la taille : le premier veut
  dire laisser les trous, et le second retirerait une espece faute de taille --
  une decision qui appartient a l'analyse, pas a la lecture, et que
  `prepare_fishmorph_basins()` prend deja explicitement.
* `size_source` prend une troisieme valeur, `"imputed"`. Elle est necessaire :
  une longueur inventee pese sur PC1 exactement comme une longueur mesuree, et
  aucune autre colonne ne les distingue. `n_imputed` continue de ne compter que
  les RATIOS.
* Le resume de fin annonce desormais la provenance des tailles a cote de celle
  des ratios : les deux entrent dans l'ordination avec le meme poids, en
  rapporter une seule donnerait une image fausse du tableau.

* `00_Scripts/fill_landmark_size.R` est neutralise. Il rustinait le CSV apres
  coup, ce que la regeneration suivante effacait, et il prenait `Length` sans
  filtrer le type -- il versait donc des TL dans une colonne de SL.

# Rfishmorph 0.12.0

## Les especes ajoutees depuis la publication cessent d'etre invisibles

* La file `"correct"` du digitizer est desormais batie sur les lignes de
  `lm_sheet` ET sur celles de `new_sheet`. Une espece entree par la file
  `"new"`, ou par l'onglet *Absent from FISHMORPH* de FishInTrait, n'etait
  jusqu'ici visible dans AUCUNE file : `reconstruct` et `correct` se lisaient
  sur `Global_Landmark`, ou elle ne figure pas, et `new` liste les FICHIERS de
  `new_photo_dir`, ou sa photographie n'est pas -- FishInTrait la depose avec
  les autres, sous `photo_dir`. Un specimen portant deja ses vingt-et-un points
  n'a pas besoin de la file d'entree : il a besoin d'etre reouvrable.
* La destination d'un enregistrement voyage donc AVEC la ligne (`.sheet` /
  `.srow`) au lieu d'etre implicite : une correction repart dans la feuille
  d'ou la ligne vient, a la position qu'elle y occupe, et les colonnes sont
  cherchees dans l'en-tete de CETTE feuille -- les deux ne rangent pas leurs
  colonnes dans le meme ordre.
* La photographie d'une ligne est resolue une fois pour toutes au demarrage
  (`.photo`) : `photo_file` d'abord, cle d'espece ensuite. La cle d'espece est
  le bon lien pour `Global_Landmark`, ou une ligne est une espece ; c'est le
  MAUVAIS pour `new_sheet`, ou une ligne est une PHOTO -- le mode planche pose
  plusieurs specimens d'une meme espece, et la cle les aurait tous montres
  sous la meme image.
* Une espece deja portee par `lm_sheet` n'est pas reprise de `new_sheet` : la
  ligne d'attente a ete promue, et c'est la ligne publiee qu'on corrige.
* `new_sheet` doit porter le nom EXACT de la feuille du classeur. Le defaut
  `"new_specimens"` ouvert sur un classeur qui porte `New_specimen` cree une
  SECONDE feuille, vide, au lieu d'utiliser celle qui existe.

## `build_fishmorph_landmark_table()` lit aussi les nouveaux specimens

* Nouvel argument `new_sheet`, par defaut `c("New_specimen", "new_specimens")`
  -- les deux orthographes en circulation, celles qui manquent au classeur
  etant simplement ignorees. Les especes digitalisees depuis la publication
  entraient dans le classeur sans jamais entrer dans le tableau de traits.
* Trois magasins et non plus deux, par autorite decroissante :
  `duckdb` > `xlsx` > `xlsx_new`. La colonne `store` du tableau retourne le
  nomme, de sorte qu'un espace fonctionnel construit sur des specimens non
  encore valides reste tracable.
* Les points 20 et 21 -- les deux bouts de la MIRE, que le digitizer enregistre
  sur les feuilles de nouveaux specimens pour convertir les pixels en
  millimetres -- ne sont plus lus comme des landmarks. Une regle n'est pas une
  partie du corps.
* Une espece absente de la table de metadonnees garde la taxonomie lue dans le
  magasin de landmarks, mais sort avec `MBl`, `MBw` et `IUCN` a `NA` : elle
  n'est pas dans `Global_segments`, ou ces colonnes vivent. Consequence directe
  pour `prepare_fishmorph_basins(size_traits = c("MBl", "MBw"))`.

# Rfishmorph 0.11.0

## Faire entrer une photo dans la file, depuis l'application (digitizer)

* Un deuxieme onglet du panneau principal, `New species`, amene une
  photographie dans la file des nouveaux specimens sans passer par le
  gestionnaire de fichiers ni relancer la session. Quatre gestes, dans le seul
  ordre qui soit sur : choisir le fichier, nommer le specimen, le CADRER,
  valider -- et la photo s'ouvre aussitot dans la file `"new"`, prete a etre
  mesuree.
* Ce n'etait pas une commodite d'intendance. Le nom de fichier est la SEULE
  identite qu'une image possede avant d'etre mesuree : `.fm_name_from_file()` y
  lit l'espece et `.fm_photo_index()` y apparie les lignes du classeur. Cette
  etape appartient donc au protocole, et elle appartient a l'application.
* Le cadrage vient AVANT le premier clic et ne doit jamais venir apres : le
  digitizer enregistre des coordonnees dans les pixels DU FICHIER, si bien que
  recadrer ou tourner une photo portant deja des landmarks les deplacerait tous
  sans toucher a un seul nombre enregistre. Cette page est le seul endroit ou la
  geometrie d'une image peut encore changer, et elle est en amont du premier
  clic par construction.
* Quarts de tour (`.fm_rot90()`), miroirs (`.fm_mirror()`) et recadrage a la
  souris (`.fm_crop()`) sont des operations exactes sur le tableau, sans
  reechantillonnage. Le format de sortie suit les VRAIS octets de la source
  (`.fm_is_png_file()`) : un PNG reste un PNG plutot que d'acquerir des
  artefacts JPEG sur les pixels memes ou se lisent les landmarks. L'ecriture est
  atomique (`.fm_write_img_as()`).
* La copie est nommee `Genus_species.<ext>`, suffixee `_2`, `_3`... quand le
  dossier porte deja l'espece (`.fm_new_photo_path()`). C'est un compteur de
  SPECIMEN et non un marqueur de doublon : la file `"new"` est une entree par
  PHOTO, plusieurs specimens d'une meme espece sont legitimes, et les deux
  lecteurs du nom de fichier retirent un nombre final -- tous reviennent donc au
  meme binome.
* Le fichier choisi par l'operateur n'est pas touche la ou il se trouve, et une
  copie en est gardee sous `_originaux/`, a cote de la file : ce qui entre dans
  la file a ete recadre et tourne, et ces pixels-la sont perdus.
* RIEN n'est ecrit dans le classeur a ce moment. Une ligne de `new_sheet` est le
  releve d'une MESURE, et une photo que personne n'a digitalisee n'a aucune
  mesure a declarer ; la ligne est creee par `Save & next`, indexee sur
  `photo_file`, exactement comme pour une photo deposee a la main dans le
  dossier. Une ligne vide ecrite a l'entree serait indiscernable d'un specimen
  dont tous les landmarks seraient sortis `NA`.
* Le nom est verifie sur sa FORME (deux mots, genre capitalise) avant validation,
  et facultativement contre FishBase (`validate_species_names()`) par un bouton
  qui SIGNALE sans jamais refuser : une espece absente de FishBase peut tres bien
  meriter d'etre digitalisee. Appliquer le nom accepte est un second bouton, et
  non une reecriture automatique -- un synonyme corrige en silence est une
  decision que personne n'a enregistree.
* La file `"new"` est reconstruite sur place (`rv$photostamp` invalide `qrows()`
  et la liste d'acces direct) : l'alternative etait de fermer l'application et de
  perdre la position dans le journal pour une seule photo.
* Le panneau lateral -- file, specimen, conventions -- est masque sur la page
  d'entree : un controle qui ne peut pas agir sur ce qui est a l'ecran est une
  invitation a l'erreur, pas un rappel.

# Rfishmorph 0.10.0

## Les points confondus sont enfin ENREGISTRES (correction)

* Les coincidences declarees -- `Mo = 0`, `6 = 8`, `PFi = 0`, `5 = 13`,
  `4 sur 22-24` -- sont ecrites avec le specimen, dans la colonne
  `collapse_rules` de la feuille visee et dans le journal, sous la forme de la
  liste des identifiants de regles (`"Mo;Hd6"`). Rouvrir le specimen replace les
  cases.
* C'etait un vrai defaut, et pas seulement d'affichage : la declaration ne
  vivait que dans la geometrie qu'elle produisait, et une regle de COPIE ne
  laisse rien qui la distingue d'une coincidence fortuite. Un specimen rouvert
  revenait donc avec ses points confondus mais ses cases vides -- et le premier
  clic, la regle n'etant plus appliquee, defaisait le zero en silence.
* Une case decochee et une case jamais cochee ne sont pas la meme chose non
  plus : la colonne est reecrite EN ENTIER a chaque enregistrement, chaine vide
  comprise, ce qui est la facon de RETIRER une regle d'un specimen.
* Pour tout ce qui a ete saisi avant cette colonne, et pour toute table venue
  d'ailleurs, les regles restent lues sur les coordonnees : `.fm_collapse_detect()`
  reconnait desormais les quatre regles de copie (une paire de points confondus)
  en plus de la projection (un point sur l'axe median). Les deux lectures sont
  reunies au rechargement, la declaration enregistree l'emportant.
* Seule la PREMIERE paire d'une regle de copie est testee -- elle est l'enonce
  lui-meme (`9 sur 1` = la bouche s'ouvre sur le profil ventral), les autres n'en
  sont que les consequences : une consequence laissee NA par une saisie ancienne
  masquerait sinon l'enonce qui l'a produite.

## Qualite et relecture d'une saisie (digitizer)

* Un onglet `Quality` dans le panneau lateral, a cote de `Specimen` et
  `Display` : une NOTE de 1 (inutilisable, points largement devines) a 5
  (excellent -- poisson entier, strictement lateral, chaque landmark non
  ambigu) et une CASE declarant l'espece verifiee. Deux champs parce que ce
  sont deux questions distinctes -- ce que vaut la saisie, et si quelqu'un l'a
  effectivement regardee : un specimen peut etre relu ET mauvais, et c'est de
  cet etat-la qu'on tire une liste de rephotographies.
* `Not scored` et une cellule vide disent la meme chose, et ni l'un ni l'autre
  n'est une note de zero. Les deux champs sont RECHARGES avec le specimen et
  reecrits a chaque enregistrement : revenir sur une espece et la sauver a
  nouveau conserve la relecture qu'elle porte deja, et effacer une note devient
  un acte explicite de l'operateur plutot qu'un accident de navigation.
* Ecrits dans quatre colonnes de la feuille visee -- `quality_score`,
  `reviewed`, `reviewed_by`, `review_date` -- creees a la volee comme les
  colonnes des charnieres. L'auteur et la date ne sont estampilles que si
  quelque chose est declare : sans cela, "personne n'a regarde" serait
  indiscernable de "quelqu'un a regarde et n'a rien dit".
* Le journal porte les deux memes champs au niveau de l'ENREGISTREMENT
  (`quality_score`, `reviewed`, repetes sur chaque ligne de point, faute d'un
  en-tete par enregistrement) et `fishmorph_consolidate()` les remonte dans le
  tableau large. Le journal disait comment chaque POINT avait ete obtenu
  (`placed`, `seeded`, `derived`...) ; il dit maintenant ce que vaut la saisie
  entiere, jugement que seul l'operateur devant la photographie peut porter.
* Les journaux ecrits par les versions anterieures restent lisibles : la
  lecture est faite sur les en-tetes et les colonnes absentes sont comblees par
  NA -- une cellule vide signifie donc "la version qui a ecrit cet
  enregistrement n'avait pas ce champ".

## Files de travail par ordre alphabetique

* Les files `reconstruct` et `correct` sont ordonnees ALPHABETIQUEMENT par
  espece, et non plus dans l'ordre des lignes du classeur -- lequel n'est qu'un
  accident de la facon dont la feuille a ete assemblee. L'ordre de la file EST
  l'ordre du travail : `Save & next` donne le NOM suivant, donc une session
  parcourt la classification au lieu de sauter d'un poisson a un autre sans
  rapport. Les congeneres arrivent alors ensemble -- meme oeil, meme nageoire,
  memes ambiguites -- et corriger un *Barbus* met tout le genre sous la main
  pendant que les criteres sont encore frais.
* La liste du champ de saut de la barre d'outils EST la file : une seule et
  meme sequence, sans second tri qui pourrait les faire diverger. Les
  resultats de recherche gardent eux aussi cet ordre au lieu d'etre classes par
  score de correspondance (`sortField` sur le texte).
* Le tri est fait en `method = "radix"`, c'est-a-dire dans la locale C : l'ordre
  est alors identique sur tous les postes, la ou l'ordre de la locale placerait
  `Barbus` avant ou apres `barbus` selon la machine.
* La file `new` garde l'ordre alphabetique des NOMS DE FICHIERS des
  photographies, seule identite de ces images avant d'etre nommees.

# Rfishmorph 0.9.0

## Selection par famille et par ordre dans l'explorateur de bassins

* Deux selecteurs supplementaires dans la barre laterale, `Ordre(s)` et
  `Famille(s)`, a cote du selecteur d'especes. Chaque libelle porte l'effectif
  du taxon -- `Cyprinidae (1 253 esp.)` -- parce que le poids d'une famille
  n'est pas devinable avant de la choisir, et qu'il commande tout ce qui suit.
* Le critere taxonomique allume les bassins ou le taxon est PRESENT, sous le
  filtre de statut courant : une famille uniquement exotique la ou elle occurre
  n'allume rien en mode "natives seules". Il se combine aux autres criteres par
  le meme selecteur ET/OU que les especes, bassins, pays et ecoregions.
* Les trois niveaux sont EMBOITES, ordre > famille > espece, et couples dans les
  deux sens. Vers le bas, un niveau n'offre que ce que le niveau au-dessus
  autorise : un ordre choisi ne laisse que ses familles, une famille que ses
  especes. Vers le haut, choisir une famille selectionne son ordre, et choisir
  une espece selectionne sa famille -- donc son ordre, par le meme chemin. Les
  trois boites se lisent alors comme un seul trajet dans la classification.
* Le cycle ainsi forme ne converge que parce que chaque observateur compare
  l'etat vise a l'etat courant et ne met a jour que sur une difference ; une
  mise a jour inconditionnelle ferait osciller la paire indefiniment. Ordre et
  famille sont pour cette raison passes au navigateur EN ENTIER (quelques
  centaines de choix) : un selectize alimente cote serveur ne connait que les
  options qu'on lui a demandees, et se verrait imposer une valeur qu'il ne peut
  pas afficher.
* Le critere taxonomique est le niveau le PLUS PRECIS selectionne, jamais
  l'union des niveaux : l'emboitement garantit que la famille est dans l'ordre,
  et unir les deux annulerait le resserrement demande -- choisir les
  Loricariidae allumerait tous les bassins a Siluriformes.
* Le lien famille -> ordre est etabli une fois au demarrage sur l'ordre MODAL de
  la famille : quelques familles apparaissent sous deux ordres dans le registre,
  et trancher a un seul endroit evite que la hierarchie reponde autrement dans
  la barre laterale et sur la carte.
* Le chemin courant est ecrit sous les trois boites -- `Ordre : Siluriformes ›
  Famille : 42 au choix › Espece : 1 253 au choix` -- parce qu'une liste
  silencieusement ramenee de 500 a 42 entrees ressemble a une liste, pas a une
  consequence. Un lien `vider les trois niveaux` elargit d'un clic, sans
  toucher aux bassins, pays et ecoregions en cours.
* Une case `Restreindre l'assemblage au taxon`, cochee par defaut, decide du
  sens de la selection. Cochee, la composition specifique, l'espace fonctionnel,
  l'enveloppe convexe, le panneau specimen et les indices FD recalcules ne
  portent que sur les especes du taxon presentes dans les bassins allumes ; la
  restriction est appliquee en un seul endroit, sur `compo()`, de sorte qu'un
  polygone ne puisse pas etre trace sur un ensemble et un FRic sur un autre.
  Decochee, l'assemblage complet reste affiche et le taxon y est surligne par
  une couche propre, avec son enveloppe en tirets.
* Les indices d'un assemblage restreint sont signales comme tels dans le
  panneau lateral : un FRic calcule sur un sous-ensemble d'especes est borne par
  celui de l'assemblage entier, et ne se compare ni au tableau des bassins ni a
  une autre selection taxonomique.
* Sans aucun filtre spatial, le taxon se dessine sur le POOL FISHMORPH complet,
  ce qui est la lecture "montre-moi cette famille dans l'espace" du selecteur.

# Rfishmorph 0.8.0

## FRic en part de l'espace fonctionnel mondial

* Nouvelle variable cartographiable et nouvelle colonne du tableau : `FRic %`,
  la richesse fonctionnelle du bassin rapportee a celle du POOL ENTIER de la
  campagne, mesuree dans le meme plan. Une aire d'enveloppe convexe est un
  nombre en unites de score au carre, sans signification propre et incomparable
  d'une campagne a l'autre ; une part de l'espace occupe par les poissons d'eau
  douce du monde se lit.
* La borne 0-100 est de CONSTRUCTION, non de convention : l'enveloppe d'un
  sous-ensemble est contenue dans celle de l'ensemble, donc aucun bassin ne peut
  depasser le pool dont il est tire. C'est ce qui autorise une echelle continue
  FIXE de 0 a 100 %, jamais ajustee sur ce qui est affiche -- une echelle
  etiree sur l'etendue observee redefinirait son propre maximum a chaque filtre,
  et deux cartes de la meme variable cesseraient d'etre comparables.
* Le denominateur est calcule PAR CAMPAGNE, chacune dans son ordination : un
  pourcentage segments et un pourcentage landmarks se rapportent a deux mondes
  differents, comme le reste de l'application.
* La distribution des parts est fortement dissymetrique -- la plupart des
  bassins occupent quelques pour cent, une poignee en occupent des dizaines --
  si bien qu'une rampe lineaire peint presque toute la carte d'une seule
  couleur. Un selecteur de rampe (racine par defaut, lineaire, log) courbe les
  COULEURS sans toucher ni aux valeurs ni aux graduations : la legende continue
  d'aller de 0 a 100 %, et un bassin garde sa valeur. Le pied de carte affiche
  la mediane, le q90 et le maximum observes, pour que le choix de rampe se fasse
  sur des chiffres.

## Une correction arrivait dans l'ordination mais pas dans le panneau specimen

* SYMPTOME : on corrige un point au digitizer, on refait toute la chaine, le
  cache porte bien la nouvelle valeur -- et le panneau specimen continue
  d'afficher l'ancienne.
* CAUSE : deux sources pour la meme chose. L'ordination vient de la table de
  traits (journal -> DuckDB -> CSV, donc a jour), tandis que le panneau
  specimen et l'onglet 4 lisaient les COORDONNEES du classeur `.xlsx`. Or le
  digitizer ne reecrit le classeur que tous les `xlsx_flush_every`
  enregistrements, sur clic explicite, ou a la fermeture propre de la session,
  alors qu'il journalise chacun d'eux : un classeur en retard de plusieurs
  jours sur son journal est l'etat NORMAL d'une campagne active, pas un
  accident. Une correction, deux reponses sur un meme ecran.
* `prepare_fishmorph_basins(landmarks_db = )` prend le magasin DuckDB et ses
  coordonnees ECRASENT celles du classeur, espece par espece -- la meme
  preseance que `build_fishmorph_landmark_table()` applique deja. Tout ce que
  l'application montre vient desormais d'un seul etat des mesures.
* Quand le classeur est plus vieux que le magasin, la preparation le DIT, avec
  les deux dates. C'est la premiere cause de « je l'ai corrige et l'app montre
  encore l'ancienne valeur », et elle ne se decouvrait qu'en comparant des
  horodatages a la main.
* Le magasin peut porter des especes absentes du classeur : elles sont
  ajoutees, pas ignorees.

## build_fishmorph_landmark_table() mourait des que les deux magasins etaient donnes

* SYMPTOME : `numbers of columns of arguments do not match`, dans `rbind()`, sans
  nommer ni les magasins ni ce qui differait.
* CAUSE : les deux magasins ne portent pas les memes points, et rien ne dit
  qu'ils le devraient. Le classeur de publication tient 1-19, 22 et le point
  derive 23 ; le journal du digitizer -- donc la base DuckDB qui en est
  construite -- tient en plus les charnieres d'axe 24 et 25, qui n'existaient pas
  quand le classeur a ete ecrit. 46 colonnes contre 42, et `rbind()` refuse.
* Les magasins sont desormais empiles sur l'UNION de leurs points, les absents
  valant `NA` -- ce qui est exactement leur sens : un point qu'un magasin ne
  porte pas est un point qui n'y a pas ete place.
* L'intersection aurait ete une CORRUPTION SILENCIEUSE plutot qu'une erreur :
  24 et 25 sont ce que `.fm_axis_chain()` suit pour mesurer `Bl` le long de
  l'axe BRISE, et s'en passer fait retomber sur la corde droite, raccourcissant
  de 8,5 % en median tout poisson courbe (voir `.FM_AXIS_HINGES` dans
  schema.R). Aucun message n'aurait annonce ce changement de resultats.
* Quand les magasins divergent, `verbose = TRUE` NOMME les points manquants de
  chaque cote. Une incompatibilite entre deux magasins doit se lire, pas se
  deviner.
* `00_Scripts/remesure_workflow.R` documente la chaine complete correction ->
  journal -> base -> table de traits -> cache, avec un controle a chaque saut.
  Deux pieges y sont expliques : `build_fishmorph_landmark_table()` doit etre
  RELANCE (journaliser des points ne recalcule aucun trait), et
  `prepare_fishmorph_basins(traits_landmark = NULL)` lit la copie INSTALLEE du
  CSV via `system.file()`, non celle de l'arbre source ou l'on vient d'ecrire.

## Onglet 4 : les deux campagnes confrontees

* Nouvel onglet « 4. Comparaison » : les deux nuages dans une ordination
  PARTAGEE, le test de Procrustes, un biplot methode contre methode par rapport
  et par segment, le tableau d'accord par variable et la liste des especes que
  le changement de methode deplace le plus.
* C'est le SEUL endroit de l'application ou les deux campagnes partagent un
  repere, et il le faut : comparer deux configurations, c'est les superposer, et
  deux ACP independantes ne se superposent pas. `compare_segments_landmarks()`
  fige une ordination sur les rapports de la campagne segments et y projette les
  deux jeux de mesures, si bien qu'un deplacement entre les nuages est un
  deplacement du POISSON et non un changement de base. Ailleurs l'application
  garde ses deux espaces separes, pour la raison inverse : chaque campagne doit
  pouvoir se lire seule.
* Rien n'est calcule par l'onglet. Tout vient de
  `Rfishmorph::compare_segments_landmarks(space = TRUE)`, appele une fois par
  `prepare_fishmorph_basins()` : un Procrustes a 999 permutations sur trois
  mille especes n'a pas sa place derriere un clic d'onglet. L'app dessine.
* Les deux panneaux partagent les MEMES echelles. Sur des echelles ajustees
  separement, deux nuages quelconques se ressemblent.
* Sur les biplots, la droite est la premiere bissectrice et non une regression :
  la question posee est l'ACCORD, pas la correlation. Un nuage bien aligne mais
  decale de la bissectrice signale un biais systematique entre les deux
  methodes, qu'un `r` eleve ne revelerait pas -- c'est pourquoi le tableau
  d'accord donne aussi le biais et le RMSE, et pourquoi `r` est imprime sur
  chaque panneau. Les segments y sont rapportes a `Bl`, ce qui elimine l'echelle
  pixel de chaque cliche.
* Le pied de l'onglet dit ce que le test de Procrustes ne dit PAS : une
  correlation elevee etablit que les deux campagnes rangent les especes de la
  meme facon les unes par rapport aux autres, non qu'un poisson est a la meme
  place ni que les indices d'un bassin sont les memes. Sa p-value ne mesure que
  l'improbabilite d'un accord nul, ce qui n'est pas l'hypothese interessante
  quand on compare deux mesures du meme objet.
* Table nominative sous les biplots : une ligne par espece ET par variable,
  filtrable et triable. Un biplot montre QUE `REs` ne concorde pas ; il ne dit
  pas qui fait le point a 21, et un panneau qui affiche un desaccord sans
  nommer sa cause renvoie le lecteur au classeur a la main. Les colonnes
  `Segments` et `Landmarks` sont triables separement de l'ecart, parce que les
  deux questions sont distinctes : un `REs` de 21 est IMPOSSIBLE -- le diametre
  de l'oeil ne fait pas vingt fois la hauteur de la tete -- et signale une
  erreur de digitalisation, tandis qu'un desaccord de 30 % entre deux valeurs
  plausibles est une divergence de methode.
* Le seuil d'ecart passe dans la BARRE LATERALE. Il pilotait les tables de
  l'onglet 2 depuis l'en-tete de l'onglet 2, et desormais aussi la table de
  l'onglet 4 : un reglage qui commande une page depuis l'en-tete d'une autre est
  un reglage que personne ne trouve.
* Un clic sur un point, sur une ligne du tableau des deplacements ou sur une
  ligne de la table nominative ouvre l'espece dans le panneau specimen de
  l'onglet 2, ou le desaccord se regarde sur le poisson.

## Le panneau specimen compare les deux campagnes

* Les deux tables affichent desormais SEGMENTS et LANDMARKS cote a cote, quel
  que soit le choix general de l'application : c'est leur seul objet, et une
  comparaison qui ne montrerait que la campagne active n'en serait pas une. La
  colonne Definition disparait, les libelles restent dans l'infobulle du trait.
* Les valeurs sont RECALCULEES depuis les mesures brutes de chaque campagne,
  jamais lues dans les tables de traits publiees : celles-ci ont subi
  l'imputation, et les comparer reviendrait a comparer deux imputations plutot
  que deux methodes. La ou une campagne ne produit pas la quantite, la cellule
  est un tiret -- y mettre une valeur imputee fermerait discretement l'ecart
  que l'on cherche a voir.
* Le statut mesure / impute / FishBase reste, mais PAR CAMPAGNE : deux colonnes
  `Orig. seg` et `Orig. lm`. « Impute » y signifie que cette campagne-la n'a pas
  la mesure brute, donc que la valeur employee par l'ordination a ete completee ;
  la cellule de comparaison, elle, reste un tiret, aucun nombre invente n'etant
  imprime.
* Un selecteur « Colorer par » choisit laquelle des deux lectures porte la
  couleur, ecart ou origine. Elles ne peuvent pas teindre les memes cellules :
  un ecart est une propriete de la PAIRE, une origine une propriete de CHAQUE
  cote, et les superposer ferait passer l'une pour l'autre. En mode origine,
  chaque colonne suit sa propre campagne plutot qu'une couleur unique qui
  affirmerait d'une mesure ce qui n'est vrai que de l'autre.
* L'ecart est SYMETRIQUE, `200|a-b|/(|a|+|b|)`, et non `|a-b|/b`. Un segment
  FISHMORPH vaut legitimement zero -- `Mo = 0` est une bouche terminale,
  `PFi = 0` une pectorale inseree sur l'axe median -- et la forme asymetrique
  n'y est pas definie, precisement sur les 954 et 737 especes ou les deux
  methodes divergent le plus. Bornee a 200 %, ce que vaut « une methode dit
  zero, l'autre non ».
* Deux paliers de couleur, regles par un curseur. Le defaut de 15 % n'est pas
  arbitraire : sur les 3 326 especes mesurees dans les deux campagnes, l'ecart
  median vaut 4,9 % pour les rapports (q75 = 12,3 %, q90 = 26,5 %) et 2,9 %
  pour les segments, si bien que 15 % signale a peu pres le quart le plus
  divergent.
* Verification prealable, sans laquelle la comparaison en PIXELS n'aurait eu
  aucun sens : les deux campagnes ont bien digitalise les MEMES photographies a
  la meme resolution -- `Bl` ne diverge que de 1,2 % en median (q90 = 3,9 %).
  Les ecarts par trait sont tres inegaux : 0,0 % en median pour `CPt`, 1,2-1,5 %
  pour `Bl`, `Ed`, `CPd`, `CFd`, mais 14,7 % pour `OGp` et 11,2 % pour `PFv`.
  Un seuil unique reste donc un outil de tri, pas un test.

## La taille peut entrer dans l'espace, et le dit

* `prepare_fishmorph_basins(size_traits = )` fait entrer `MBl` (longueur
  maximale) et `MBw` (masse maximale) dans l'ACP, dans les indices et dans le
  compte des NA, aux cotes des neuf rapports. Les deux par defaut.
* Ce n'est pas un reglage technique mais un choix sur ce que l'espace SIGNIFIE,
  et le code le traite comme tel : argument explicite, enregistre dans le cache,
  affiche par l'application. Sur les 8 970 especes de la table publiee, les
  charges de `MBl` et `MBw` sur PC1 valent -0,51 et -0,51, devant tous les
  rapports de forme (`RMl` -0,38, `BLs` +0,37) : PC1 devient largement un axe de
  TAILLE, et `FRic` cesse d'etre comparable a l'espace FISHMORPH de Brosse et
  al. (2021), bati sur les rapports seuls. Les deux variables sont de plus
  correlees a r = 0,925 -- log-masse vaut sensiblement log a + b log-longueur --
  si bien que les demander toutes les deux fait entrer la taille DEUX FOIS dans
  une ACP centree-reduite. Variance PC1+PC2 : 47,2 % sur les neuf rapports,
  43,9 % avec les deux tailles.
* La taille compte dans les NA : une espece sans longueur quitte l'espace au
  lieu d'y sieger a une taille inventee.
* Elles ne sont JAMAIS vertes. Le panneau leur donne un statut propre, bleu,
  « attribut d'espece (FishBase) » : ce ne sont pas des mesures sur le cliche,
  et les peindre comme un segment qui, lui, a ete mesure dessus mettrait deux
  natures de preuve sous une meme couleur. Elles apparaissent dans les deux
  tables du panneau, en unites brutes (cm et g) pour les segments, en log10 et
  en valeur retro-transformee pour les rapports.
* Une colonne de taille demandee mais absente de la table d'une campagne est une
  ERREUR, pas un abandon silencieux : la campagne serait sinon ordonnee sur un
  jeu de variables different de sa voisine sans que rien ne le dise.
* `00_Scripts/fill_landmark_size.R` comble les 531 especes de
  `fishmorph_data_landmarks.csv` qui n'ont ni `MBl` ni `MBw`. AUCUNE n'est
  recuperable dans `fishmorph_data.csv` -- ce sont precisement les especes
  absentes de la table segments, d'ou le NA laisse par la jointure. La masse est
  derivee de la relation longueur-poids `a * L^b` de FishBase, comme les 4 582
  valeurs deja presentes : un poids maximal observe et un poids derive dans une
  meme colonne y mettraient deux quantites differentes sans moyen de les
  distinguer. Une colonne `size_source` garde la provenance, le fichier
  d'origine est copie et date avant remplacement, et le script termine en
  comparant les medianes ancienne et nouvelle -- une erreur d'unite (metres,
  kilogrammes) ne se voit pas autrement.

## Segments ou landmarks : deux campagnes, deux espaces, une bascule

* `prepare_fishmorph_basins(traits_landmark = )` construit une ordination et un
  jeu d'indices PAR CAMPAGNE DE MESURE, et l'explorateur bascule de l'une a
  l'autre : carte, espace fonctionnel, tableau des bassins et panneau specimen
  suivent tous le meme selecteur.
* Les deux campagnes sont tenues separees de bout en bout -- table de traits,
  ACP, indices. Les melanger donnerait une ordination dont les axes sont pour
  partie un effet de methode, et un bassin dont la richesse fonctionnelle change
  parce que ses poissons ont ete mesures deux fois plutot que parce que quoi que
  ce soit d'ecologique differe.
* En contrepartie, LES DEUX ESPACES NE SONT PAS COMPARABLES POINT PAR POINT :
  deux ACP independantes sur des pools d'especes differents donnent des axes
  differents, donc une espece ne conserve pas ses coordonnees quand on bascule,
  et un `FRic` de 12 en segments n'est pas un `FRic` de 12 en landmarks. Ce qui
  se compare est le CLASSEMENT des bassins et le SIGNE d'un contraste. La barre
  laterale et le pied de l'onglet 2 le disent, et l'etiquette de chaque axe
  porte la part de variance de SA campagne -- « PC1 (34 %) » n'est pas une
  propriete du nombre 1.
* Le registre des especes ne porte plus que l'IDENTITE (nom, ordre, famille,
  genre, IUCN). Ce qui varit d'une campagne a l'autre -- ce qui a ete mesure,
  ou l'espece se situe -- vit dans des matrices alignees sur ce registre. Une
  espece ne peut donc plus porter les coordonnees d'une campagne dans laquelle
  elle n'a pas ete mesuree.
* Les segments bruts du panneau specimen suivent eux aussi la campagne : la
  feuille `Global_segments` publiee pour les segments, et les onze memes
  distances RECALCULEES entre les landmarks du specimen pour la campagne
  landmark. Comparer une table a une table n'aurait pas compare deux methodes ;
  deriver les deux de la meme facon, si. `Bl` suit l'axe brise 1-22-2 quand la
  charniere de courbure existe : la corde sous-estime la longueur d'un poisson
  courbe de 8,5 % en median, et l'utiliser aurait fait paraitre la methode
  landmark systematiquement « plus courte » pour une raison de formule.
* Le statut mesure / impute est lui aussi calcule par campagne : depuis les NA
  de `Global_segments` pour l'une, depuis les NA des coordonnees pour l'autre.
* Un cache anterieur ne s'ouvre PLUS : il porte une ordination unique dont rien
  ne dit de quelle campagne elle vient, et deviner serait exactement
  l'ambiguite que cette separation supprime. L'application s'arrete avec la
  commande de reconstruction plutot que d'afficher des nombres dont elle ne
  peut pas nommer la provenance.
* Le cache porte desormais un NUMERO DE FORMAT, verifie au demarrage avant que
  quoi que ce soit ne soit lu. Le cache et l'application sont ecrits et lus par
  des processus differents, installes a des moments differents, et supposer
  qu'ils concordent est la seule hypothese qui soit regulierement fausse : un
  decalage se manifestait quarante reactifs plus loin par
  `undefined columns selected`, qui ne nomme ni le cache ni la version. Le
  message dit maintenant LEQUEL des deux est en retard -- reconstruire le
  cache, ou reinstaller le package. Les colonnes attendues du registre des
  especes sont verifiees dans la foulee, pour la meme raison.

## Le digitizer peut inscrire le retournement dans le fichier

* Nouveau bouton « Write the flip into the file » (onglet Display de
  `launch_fishmorph_digitizer()`). Il ecrit la photographie TELLE QU'AFFICHEE,
  parce que le fichier etait jusqu'ici le seul objet a ne pas savoir qu'on
  l'avait retourne : le retournement vivait dans la session, les coordonnees
  etaient enregistrees dans le repere retourne, et tout autre lecteur --
  `launch_fishmorph_basins()` au premier chef, qui dessine les points sur le
  fichier et rien d'autre -- voyait des landmarks en miroir de leur poisson.
* La regle « ecrire ce qu'on voit » vaut pour les DEUX selecteurs, et c'est
  pourquoi ils sont composes plutot que traites separement. « photo +
  landmarks » a deplace les points avec l'image : ils sont enregistres dans le
  repere retourne, et sauver l'image retournee met le fichier d'accord avec
  l'enregistrement. « photo ONLY » existe pour le cas inverse -- des points
  charges en miroir de leur photographie -- et le retournement de l'affichage
  est ce qui les remet sur le poisson : les points enregistres etaient donc
  deja dans le repere retourne, et sauver l'image les rejoint aussi. Dans les
  deux cas, la composition affichee est celle a laquelle les coordonnees
  appartiennent.
* Ce qui n'est PAS inscrit : le sous-echantillonnage de l'affichage rapide.
  C'est un raccourci de rendu, et l'ecrire retrecirait la photographie sous des
  coordonnees exprimees en pixels pleine resolution.
* Trois precautions, aucune facultative. L'original est copie dans
  `_originaux/` AVANT toute ecriture, et une seule fois -- une seconde
  application ne peut donc pas remplacer le fichier intact par un fichier deja
  retourne. L'ecriture passe par un temporaire DANS LE MEME REPERTOIRE puis un
  rename, atomique sur un meme systeme de fichiers : une interruption laisse
  l'ancienne photographie entiere plutot qu'un fichier tronque (`tempdir()` ne
  conviendrait pas, souvent sur un autre peripherique, ou le rename redevient
  une copie). Le format de sortie suit les OCTETS reels de l'original et non son
  extension : environ 7 % des « .jpg » de cette collection sont en fait PNG,
  GIF ou BMP, et les re-encoder en JPEG a cause de leur nom ajouterait une
  compression destructive a un fichier qui n'en avait pas.
* Apres ecriture, les deux selecteurs reviennent a None et le tableau en memoire
  devient l'image retournee : le retournement est desormais DANS l'image, et le
  laisser arme le defferait au redessin suivant. La notification rappelle que ce
  bouton ecrit l'IMAGE, pas l'enregistrement.
* CORRECTIF dans l'explorateur de bassins : il choisissait son lecteur d'image
  d'apres l'EXTENSION du fichier, donc echouait sur un fichier sur quatorze --
  et echouait en silence, le panneau montrant des landmarks sur fond blanc,
  c'est-a-dire exactement ce qu'il affiche pour une espece sans photographie.
  Le format est desormais lu dans les octets d'en-tete, comme le fait le
  digitizer depuis toujours, avec repli par 'magick' pour les formats exotiques.

## Troisieme application : les bassins versants du monde

* `launch_fishmorph_basins()` ouvre une carte mondiale des 3 364 bassins de
  drainage, l'espace morphologique FISHMORPH qui suit le bassin clique, et un
  tableau d'un bassin par ligne avec pays, ecoregion, richesse specifique et
  indices de diversite fonctionnelle. Bati sur le patron de
  `FishInTrait::launch_fishintrait_explorer()` -- meme separation lanceur /
  `inst/shiny/`, meme communication par option, meme regle de degradation d'une
  dependance Suggests absente.
* `prepare_fishmorph_basins()` fait TOUT le calcul, une fois, et ecrit un `.rds`.
  L'application ne calcule rien qui depende d'un clic : le shapefile fait 50 Mo,
  les deux tables d'occurrence 210 000 enregistrements, et les indices de
  3 364 bassins quelques minutes d'enveloppes convexes. Rien de cela ne change
  quand l'utilisateur bouge la souris, donc rien de cela n'a sa place dans un
  reactive.
* Les indices sont precalcules pour CHAQUE sous-ensemble de statut (`all`,
  `native`, `exotic`) plutot que pour l'assemblage global seul. L'alternative
  mettait trois mille enveloppes convexes derriere un bouton radio ; le
  sous-ensemble exotique est petit, le cout est de l'ordre du double, et chaque
  nombre affiche reste un nombre calcule de la meme facon.
* Deux sources d'occurrence, commutables dans l'interface : la table de type
  Tedesco (statut natif / exotique, 3 119 bassins nommes dont 2 972 retrouves
  dans le shapefile) et l'export Catalogue of Fishes (3 364 bassins, aucun
  statut). Les noms de bassins absents du shapefile sont signales et ecartes
  au moment de la preparation, pas silencieusement.
* Un critere actif SOUSTRAIT de la carte : seuls les bassins retenus sont
  dessines, les autres disparaissent. Garder 3 364 polygones sous une selection
  de douze rend les douze plus difficiles a voir, pas plus faciles, et coute un
  redessin complet pour aucune information. L'echelle de couleur, elle, reste
  ajustee sur TOUS les bassins de la source : une echelle qui se recalerait sur
  la selection ferait changer un bassin de couleur selon ce qui l'entoure, et
  deux filtres successifs ne seraient plus comparables. Le trait de contour
  s'epaissit quand le filtre est actif, la mosaique continue ne servant plus de
  repere. Recadrage automatique sur l'emprise de la selection, desactivable --
  une espece presente sur quatre continents a pour emprise le monde entier.
* Deux modes de rendu de la carte : remplissage (choroplethe) et CONTOURS SEULS,
  la variable etant alors portee par la couleur du trait -- une carte de
  contours qui abandonnerait la variable serait une autre carte, pas une carte
  allegee. Le mode contours garde `fill = TRUE` avec `fillOpacity = 0` plutot
  que `fill = FALSE` : les deux se ressemblent et se comportent differemment,
  un trace non rempli n'etant cliquable que sur son trait, ce qui reviendrait a
  viser une ligne d'un pixel pour selectionner un bassin.
* Fonds de carte elargis a ceux qui dessinent le reseau hydrographique et le
  relief qui l'a produit (`Esri.NatGeoWorldMap`, `Esri.WorldTopoMap`,
  `OpenTopoMap`, `Esri.WorldShadedRelief`), plus un fond blanc, qui est ce qui
  rend 3 364 limites emboitees lisibles en mode contours.
* Panneau SPECIMEN sous l'espace fonctionnel : la photographie de l'espece
  selectionnee, ses landmarks, ses segments et ses reperes, dessines avec les
  conventions de `launch_fishmorph_digitizer()` -- et rien d'autre. Aucun
  gestionnaire de clic, aucune ecriture : c'est l'image du digitizer sans ses
  verbes.
  - `prepare_fishmorph_basins(landmarks = )` prend le classeur de publication
    (feuilles `Global_Landmark` et `Global_segments`) et stocke les
    COORDONNEES. Les photographies restent sur le disque et sont trouvees au
    lancement par `launch_fishmorph_basins(photos = )` : 7 588 cliches font
    ~1 Go, vingt-trois points par espece font 3 Mo, et le panneau n'affiche
    jamais qu'une image a la fois. L'application connait donc toujours la
    FORME d'un poisson et seulement parfois son image, ce qui est le bon sens :
    le dessin degrade vers une silhouette, pas vers rien.
  - Les matrices de landmarks sont indexees PAR NUMERO de point, avec des
    trous : la table publiee porte 1-19, 22 et le point derive 23, et un
    reindexage dense renumeroterait silencieusement l'anatomie. `P[6, ]` est
    le landmark 6 ici comme dans le digitizer.
  - Les neuf rapports voyagent desormais avec le registre des especes, pas
    seulement leur ordination : un panneau qui montre un specimen doit pouvoir
    dire ce qui a ete MESURE dessus, et un score d'axe principal est une
    position dans un nuage, pas une mesure. Ils sont stockes tels que lus
    (log10(x + 1)) et retro-transformes a l'affichage, qui montre les deux --
    le rapport brut est ce qu'on a mesure sur le poisson, la valeur
    transformee est ce que l'ordination a utilise.
  - Les segments sont affiches en PIXELS, pas convertis en millimetres.
    `MaxLength` est une longueur maximale d'espece (FishBase), pas la taille du
    poisson photographie : s'en servir comme etalon fabriquerait une mesure que
    la donnee ne porte pas. Les rapports, sans dimension, restent comparables.
  - Le poisson affiche est UN etat, ecrit par trois gestes : un clic dans
    l'ordination, une espece choisie dans le panneau lateral, le selecteur du
    panneau lui-meme. Faire de la liste deroulante cet etat aurait fait du clic
    un cas particulier obligé d'aller ecrire dans un widget, et les deux
    auraient derive.
  - Chaque point de l'ordination porte sa cle d'espece dans `customdata`. Un
    clic revient donc en NOMMANT le poisson, au lieu d'un couple
    (curveNumber, pointNumber) qu'il faudrait decoder contre une disposition de
    traces qui change avec la coloration, les filtres et l'enveloppe convexe --
    et qui designerait silencieusement la mauvaise espece a la premiere trace
    ajoutee.
  - Une espece cliquee hors de l'assemblage courant est AJOUTEE a la liste
    plutot que refusee : cliquer un point et ne rien voir serait l'application
    contredisant son propre dessin. Une espece que le cache ne peut pas
    illustrer declenche un avertissement au lieu d'un repli silencieux sur une
    autre -- repondre a un clic par la photo d'un autre poisson est pire que ne
    rien montrer.
  - Les deux tables de mesures n'ont plus de hauteur fixe. Un `DTOutput` a
    hauteur imposee rogne son conteneur sans reduire la table, si bien qu'une
    table de neuf lignes dans 260 px se dessinait par-dessus la suivante ; le
    defilement est delegue a DT (`scrollY`), qui dimensionne le conteneur.
  - Les deux tables colorent leurs valeurs : VERT mesure sur la photographie,
    ROUGE complete par imputation, GRIS statut inconnu. Ce statut est DEDUIT et
    non lu, et le pied de panneau le dit : la table de traits publiee ne porte
    aucun drapeau par rapport, et le `n_imputed` de la table landmark est un
    compte par espece, qui ne dit pas LEQUEL des neuf a ete estime. La regle
    est prise la ou l'information est sans ambiguite -- les segments bruts : un
    rapport est mesure exactement quand les DEUX segments dont il est fait sont
    presents dans `Global_segments`. Sur les donnees courantes cela donne
    59 408 rapports mesures pour 14 023 imputes.
  - Trois etats, pas deux. Les 811 especes de la table publiee absentes de la
    feuille de segments restent en GRIS : rien dans la donnee ne dit si leurs
    rapports ont ete mesures ou estimes, et les peindre en vert affirmerait une
    mesure que la source n'a jamais faite.
  - Un segment NUL est mesure, pas manquant : `Mo = 0` est une bouche
    terminale, `Bbl = 0` un poisson sans barbillons, deux observations. Le test
    porte donc sur `is.na()` et non sur la verite de la valeur -- l'inverse
    aurait classe 5 078 poissons sans barbillons comme non mesures.
  - Un cache anterieur reste ouvrable : l'absence des colonnes de rapports ou
    de landmarks est annoncee dans le panneau au lieu de faire echouer
    l'application.
* `prepare_fishmorph_basins(rivers = )` porte HydroRIVERS v1.0 (Lehner & Grill
  2013) DANS le cache : le reseau hydrographique devient une donnee locale
  plutot qu'un service tiers avec une date de retrait. Trois decisions y sont
  prises :
  - Le filtre sur `ORD_FLOW` est POUSSE DANS GDAL, en OGR SQL, avant que quoi
    que ce soit n'arrive dans R. HydroRIVERS compte 8 477 883 troncons ; les
    lire tous pour en jeter 98 % demande plusieurs gigaoctets pour rien. Si le
    pilote refuse la requete, on retombe sur une lecture complete, en le disant.
  - `ORD_FLOW` est une classe LOGARITHMIQUE DE DEBIT qui se lit a l'envers de
    l'intuition : 1 vaut au moins 100 000 m3/s, 10 moins de 0,001. Garder
    `ORD_FLOW <= n` garde les GRANDS fleuves, et inverser l'inegalite
    conserverait silencieusement huit millions de ruisseaux de tete de bassin.
    Le seuil est donc traduit en debit (10^(6 - n) m3/s) dans le message de
    progression, plutot que laisse sous forme de nombre nu.
  - Chaque troncon est rattache a UN bassin, via un point porte par le troncon
    et non en intersectant les lignes avec les polygones : l'intersection
    couperait en deux tout troncon franchissant une ligne de partage et
    multiplierait la couche. Le rattachement se fait contre la geometrie des
    bassins EN PLEINE RESOLUTION, pas la version simplifiee -- la
    simplification est une decision d'affichage et ne doit pas atteindre la
    donnee. Un troncon hors de tout bassin garde `basin_id = NA` et est
    conserve : la delimitation de Tedesco ne couvre pas toute la surface
    continentale, et une riviere qui en sort renseigne sur la delimitation.
* Dans l'application, le reseau local suit la selection : filtre actif, seuls
  les troncons des bassins affiches partent vers le navigateur. Un plafond de
  25 000 troncons protege leaflet, et quand il mord ce sont les PLUS GROS cours
  d'eau qui sont conserves -- un elagage aleatoire amincirait le reseau partout
  au lieu de le degrossir, ce qui se lirait comme des donnees manquantes. Le
  pied de carte annonce le plafond quand il s'applique.
* Calque optionnel « reseau hydro. » : Esri Hydro Reference Overlay, tuiles
  raster libres sans cle, largeur de trait proportionnelle au debit, compilees
  depuis HydroSHEDS (WWF), GTOPO30, SRTM, GLWD et GRDC. Il est dessine dans son
  propre pane leaflet (`zIndex` 450) : les tuiles atterrissent sinon sous les
  polygones, donc sous une choroplethe a 85 % d'opacite, invisibles exactement
  quand on les demande. La delimitation des rivieres est INDEPENDANTE de celle
  des bassins de Tedesco : les deux forment un controle croise visuel, pas une
  paire emboitee, et une riviere longeant une limite signale un desaccord entre
  deux jeux plutot qu'une erreur dans l'un des deux. Esri a place cette couche
  en support de maturite en juin 2025 et annonce son retrait pour decembre
  2026 ; le pied de carte le dit, et la reponse durable est de porter
  HydroRIVERS dans le cache comme donnee plutot que comme fond.
* La simplification des polygones desactive `s2` le temps du calcul. `s2` valide
  chaque anneau et refuse la couche entiere au premier polygone
  auto-intersectant (`Loop 5 is not valid: Edge 12 crosses edge 14`) ; les
  couches hydrologiques publiees en portent regulierement quelques-uns, et un
  seul suffisait a faire echouer un cache construit sur les 3 363 autres
  bassins. Trois filets successifs -- `st_make_valid()` puis nouvel essai, puis
  conservation de la geometrie complete avec avertissement -- et un bassin
  effondre a vide est restaure en pleine resolution : un trou dans la carte se
  lit comme une absence de donnees.
* `fishmorph_basin_indices()` est exporte separement : les cinq indices
  (`FRic`, `FDiv`, `FDis`, `FEve`, `Rao`) doivent pouvoir etre reproduits -- et
  contredits -- depuis la console. Poids specifiques egaux, parce qu'une table
  d'occurrence enregistre une presence et non une abondance, et que ponderer
  des presences reviendrait a inventer une quantite absente des donnees.
* Les deux familles d'indice ne sont pas mesurees sur le meme nombre d'axes, et
  la coupure est assumee. `FRic` et `FDiv` reposent sur une enveloppe convexe,
  dont le volume perd tout sens des que le nombre d'especes approche le nombre
  d'axes : ils sont calcules dans le PLAN que l'application dessine, pour que le
  nombre du tableau soit l'aire que l'oeil voit. `FDis`, `FEve` et `Rao`
  reposent sur des distances, stables en dimension superieure : ils utilisent
  quatre axes. Le pied de page de chaque panneau le dit, et l'onglet 2 avertit
  explicitement quand on change de plan que le polygone dessine n'est plus
  celui dont l'aire est reportee.
* Un indice qu'un assemblage est trop petit ou trop degenere pour supporter
  vaut `NA`, jamais `0`. Un volume non mesurable n'est pas un volume nul, et
  renvoyer 0 placerait un bassin dont les especes sont colineaires en bas d'un
  classement plutot qu'en dehors.
* La colonne `Couverture` (part des especes du bassin ayant une morphologie
  FISHMORPH) est affichee partout ou un indice l'est. Les indices ne portent
  que sur les especes mesurees ; un bassin de 80 especes dont 20 mesurees n'est
  pas un bassin de 20 especes, et une interface qui masquerait l'ecart
  cacherait exactement le biais que cette colonne existe pour exposer.
* Verrouille par `tests/testthat/test-basin-indices.R` : les cinq indices sur
  des configurations dont la valeur se calcule a la main (carre unite, points
  colineaires, assemblages de 1 et 2 especes), et l'invariance de `Rao` et
  `FDis` a la rotation.

# Rfishmorph 0.7.1

## La base refusait le statut « adjusted », que le journal ecrit depuis 0.6.0

* SYMPTOME : `fishmorph_build_db()` echouait sur
  `CHECK constraint failed on table landmark_obs`, apres avoir pourtant lu et
  valide tout le journal.
* CAUSE : le vocabulaire des statuts est defini une fois, dans
  `.FM_JOURNAL_STATUS` (R/journal.R), et vaut cinq valeurs -- `placed`,
  `seeded`, `adjusted`, `derived`, `na`. Le DDL de la base en recopiait quatre,
  a la main. Tant qu'aucune convention FISHMORPH n'avait rabattu un point,
  personne ne voyait la difference ; depuis que 3 et 4 sont rabattus sur la
  profondeur maximale et que 4 se projette sur l'axe median (0.6.0), la moitie
  des specimens portent un point `adjusted`. Une contrainte SQL rejette le LOT
  entier, donc un seul point suffisait a faire echouer une base de plusieurs
  centaines de specimens, avec un message nommant une contrainte plutot qu'un
  poisson.
* Le CHECK est desormais CONSTRUIT depuis `.FM_JOURNAL_STATUS`, et le DDL
  devient une fonction (`.fm_ddl()`) pour cela : R source les fichiers dans
  l'ordre alphabetique, `database.R` precede `journal.R`, et une constante
  aurait ete evaluee avant l'existence du vocabulaire. Les deux ne peuvent plus
  diverger sans que le meme edit les touche tous les deux.
* `v_specimen_qc` gagne `n_adjusted`. Une vue qui stocke un statut sans le
  compter fait mentir ses propres totaux : la somme des colonnes ne valait plus
  le nombre de points, ce qui est la meme derive un etage plus haut.
* Verrouille par `tests/testthat/test-db-status.R` : un journal portant les
  cinq statuts doit se construire, faire l'aller-retour et s'additionner.

# Rfishmorph 0.7.0

## Les applications s'ouvrent dans le navigateur, pas dans le Viewer

* `shiny::runApp(launch.browser = TRUE)` finit dans `utils::browseURL()`, qui
  passe par `options("browser")` -- que RStudio REMPLACE par un gestionnaire
  gardant les URL localhost dans l'IDE. L'application atterrissait donc dans le
  panneau Viewer : quelques centaines de pixels de large, sans barre
  d'adresse, sans second onglet, et avec un moteur JavaScript qui n'est pas
  celui pour lequel l'interface a ete ecrite. Ce n'est pas une affaire de gout :
  une carte leaflet ou une figure plotly y sont inutilisables.
* `launch.browser` accepte desormais quatre formes, avec le meme sens dans les
  trois packages : `TRUE` (defaut) ou `"browser"` force le navigateur du
  systeme ; `"viewer"` restitue le panneau a qui le prefere ; `FALSE` n'ouvre
  rien et imprime l'URL ; une fonction est utilisee telle quelle.
* Le gestionnaire « fenetre externe » de RStudio est cherche PAR NOM dans
  `tools:rstudio`, jamais suppose : hors RStudio, nom disparu dans une version
  future, environnement non attache -- chaque echec retombe sur `browseURL()`.
  Un lanceur ne doit pas s'interrompre parce qu'un nom interne d'un autre
  programme a bouge.

# Rfishmorph 0.6.0

## A coincidence with a LINE: point 4 on the mid axis

* New rule in the *Coincident points* bar of `launch_fishmorph_digitizer()`:
  **`4 on 22-24`**. Point 4, the ventral end of the body depth, is projected
  perpendicularly onto the mid axis 22 -> 24. It keeps the abscissa that was
  clicked along the axis and its height becomes zero; the line is not bounded by
  the two hinges, so the foot of the perpendicular may fall on their
  prolongation.
* This is a second KIND of rule. The four existing ones state that two POINTS
  coincide and are expressed as a copy of coordinates; this one states that a
  point lies on a LINE and is expressed as a projection. `.FM_COLLAPSE` entries
  therefore carry `moves` (copies) and/or `project` (projections), and
  `.fm_apply_collapse()` gains a `kinds` argument to apply one family without
  the other.
* The order in the reconstruction follows from the geometry. Point 4 is the
  master of the belly line, so the projection is applied BEFORE the constrained
  editing -- 11, then 8 and 9, are re-derived from the projected 4 -- and
  replayed at the end, where it is idempotent. Applied only at the end, as the
  copies are, it would have left the belly line no longer passing through its
  own pivot.
* Declaring the rule SUSPENDS the ventral half of the extreme-point check on
  save (`.fm_extreme_violations(skip = )`): 4 no longer claims to be the most
  ventral point, so reporting 6, 10 or 14 below it would flag the rule itself.
  The dorsal half, on 3, is untouched.
* 4 stays in `edited` -- only its height is imposed, its position along the body
  remains a measurement -- but is reported `"adjusted"` in the journal, since a
  rule placed it. A copied point, which owes its partner everything, still has
  its override taken over by the rule.
* A projection leaves no pair of coincident points to be recognized by, so
  reopening a specimen reads it back off the geometry (`.fm_collapse_detect()`,
  0.5 px of the axis). A real belly sits at half the body depth from the
  midline, some 12 % of the standard length, so the band is empty.

# Rfishmorph 0.5.0

## The broken body axis, stated explicitly

* `fishmorph_landmarks()` now pads to **25** points instead of 21
  (`.FM_N_POINTS`): 19 anatomical landmarks, the scale bar 20-21, the curvature
  hinge 22, the derived point 23 and the extra axis hinges 24-25. The digitizer
  has been recording 25 points for a while, so the object was one shape and the
  stored data another, and a configuration carrying hinges lost them on the way
  in. `NA` means "not placed" and every routine skips it, so the wider frame
  costs nothing.
* `fishmorph_schema()` gains the labels of points 23-25 and an `axis_hinges`
  element. It described a 22-point scheme while the data carried 25, which is
  an odd thing for a single source of truth to do. `.fm_bl_broken()` now reads
  `.FM_AXIS_HINGES` rather than a hard-coded vector, so scheme and computation
  can no longer drift apart.
* Confirmed and locked by tests: `Bl` is the arc length
  `1 -> (hinges placed) -> 2`, hinges 22, 24 and 25 being treated identically
  and **sorted along the 1-2 chord**, so the order in which they were clicked is
  irrelevant. With no hinge the chain collapses to the straight 1-2 distance.
* **Point 23 is not a hinge and never enters the chain.** It lies on the line
  (1, 9), the ventral line running back from the snout, not on the body axis;
  its purpose is the segment 23-6, the axial snout-to-head-base distance.
  Inserting it into the chain sends the polyline down to the belly and back,
  inflating `Bl` by a median 8.5% and up to 27% on the 650 species that carry
  22, 23 and 24 — an error that would propagate to `BEl` and `PFs`, and
  unevenly, since deep-bellied fishes suffer most. Measured on the current
  data, not assumed, and now covered by `test-bl-axis.R`.

## Two measurement campaigns, one API

* The FISHMORPH traits now come in two flavours, and every function that reads
  the global table lets you say which: `source = "segment"` is the published
  table (`fishmorph_data.csv`, 8,970 species, ratios from the eleven segments
  measured on the plates), `source = "landmark"` is `fishmorph_data_landmarks.csv`,
  whose ratios are recomputed from the landmark re-digitization through
  `fishmorph_segments()`. Added to `load_fishmorph_reference()`,
  `fishmorph_space_data()`, `fishmorph_trait_space()`, `project_fishmorph()` and
  `launch_fishmorph_space()`.
* `set_fishmorph_source()` / `get_fishmorph_source()` set the session default
  (`options(fishmorph.source = )`) consulted whenever `source` is left `NULL`.
  The argument always wins over the option, so a script can pin one call
  without disturbing the rest, and a shared script never depends on the state
  of the session that runs it.
* `load_fishmorph_reference()` now says out loud which campaign it loaded and
  how many species it holds, and tags the result with
  `attr(, "fishmorph_source")`. The landmark table covers **only the digitized
  species**, so an analysis run on it describes a smaller pool than one run on
  the segment table; that must not be discoverable only after the fact. Pass
  `quiet = TRUE` to silence it.

## Building the landmark table

* `build_fishmorph_landmark_table()` assembles the landmark trait table from
  the publication workbook (`Global_Landmark` sheet) and/or the digitizer
  DuckDB store, following one homogeneous geometric path: landmarks ->
  `fishmorph_segments()` -> `fishmorph_ratios()` -> imputation ->
  `log10(x + 1)`. Species held by both stores are taken from DuckDB, the more
  recent digitization. Taxonomy, `MBl`, `MBw` and `IUCN`, which no landmark can
  yield, are joined from the segment table.
* Ratios missing because a structure is absent or a specimen only partly
  digitized are filled with `na_action = "missforest_phylo"` by default, on the
  raw scale and on the precomputed phylogenetic PCoA axes, i.e. the same
  coordinate system as every other imputation in the package. The per-species
  `n_imputed` column keeps the count, so imputed species stay excludable.
* `data-raw/build_fishmorph_landmark_table.R` regenerates the shipped snapshot
  with a pinned seed and prints a segment-vs-landmark correlation table. Read
  that table with care: the reconstruction imposes the segment *lengths*, so a
  high correlation on the size ratios is a property of the construction and not
  evidence of agreement. The position ratios `OGp`, `VEp` and `PFv` are where
  the landmark campaign carries independent information.

## Note on comparability

* A "landmark" space is a **refitted** PCA, not a reprojection into the segment
  space. Axis order and sign may differ between campaigns, and scores are not
  comparable term by term without an explicit alignment.

# Rfishmorph 0.4.0

## Coincident points: a measurement of zero

* A bar under the photograph declares the segments that are ZERO on the species
  in view. A zero is a measurement like any other -- neither a missing value nor
  a placement error -- and the FISHMORPH ratios are defined to take it:
  `OGp = 0` for a mouth opening on the ventral profile, `PFv = 0` for a pectoral
  fin inserted on the belly. Four rules: **`Mo = 0`** (9, and 23, take the
  coordinates of 1), **`6 = 8`** (the bottom of the head is the body underside,
  and 23 follows 9), **`PFi = 0`** (10 takes the coordinates of 11) and
  **`5 = 13`** (an eye reaching the top of the head).
* That 23 follows 9 under `6 = 8` is a **consequence, not an extra convention**:
  23 is the intersection of the line (1, 9) with the line through 6 parallel to
  the head axis, and the belly line {9, 8, 11} is itself parallel to that axis,
  so once 6 sits on the belly line that parallel IS the belly line and the
  intersection is 9. Checked numerically, tilted photograph included: the
  derivation lands on 9 to machine precision. The rule states it explicitly so
  that it holds even when the derivation is degenerate.
* **Nothing is deleted.** Both points keep a position, both are drawn on the
  photograph and both are written to the workbook; one simply takes the
  coordinates of the other, so the segment between them measures zero. A
  coincidence is a measurement, an absence is `NA`, and the two must not be
  confused downstream.
* In `"reconstruct"` mode a zero already comes from the workbook -- the points
  are laid out from the measured segments. The rules are for `"new"`, where the
  points are seeded from medians, and `"correct"`, where a specimen is being
  repaired. They are applied at the END of `recon()`, after `.fm_constrain()`
  and after point 23 is rebuilt, because the constrained editing re-derives the
  ventral points on the belly line at every click.
* Which point moves is a protocol decision and is not the same for every rule.
  For the mouth the fixed point is 1, the snout -- and 23, built on the line
  (1, 9), is undefined once 9 sits on 1, so it follows 1 rather than becoming
  `NA`. For the two ventral rules the belly line holds: 8 and 11 are its
  intersections with the eye and the pectoral verticals, so the head bottom and
  the fin insertion come onto them. For the eye at the top of the head, 5 comes
  onto 13, since moving 13 would change `Ed`, a measurement in its own right.
* Points moved by a rule take the `"adjusted"` status in the journal, whose
  meaning widens accordingly: placed by a rule the operator invoked, neither
  pointed at nor left at a seed. Declarations are reset for every species.

## The eye vertical is checked, in order

* The save-time check now also verifies the ORDER of the six points that the
  FISHMORPH conventions place on one vertical -- 5, 13, 7, 14, 6, 8, from the
  back downwards: top of the head, top of the eye, centre of the eye, bottom of
  the eye, bottom of the head, body underside. Two things are tested and they
  are not the same statement: that **5 tops the group** (the `Hd` analogue of
  the 3/4 rule for `Bd`), and that **every consecutive pair is in order**, which
  catches a local swap the first test cannot see.
* This is the failure no other check catches, because each pair stays
  internally consistent: with 13 and 14 exchanged -- the eye clicked
  bottom-first -- `Ed` (13-14) keeps its exact length while `Eh` (7-8) silently
  refers to the wrong edge of the eye. Nothing in a coordinate table shows it.
* Settled on the data, as the `Bd` rule was. Over the 4,151 species already
  digitized in the workbook, the expected order holds for **99.5 %** of them:
  8 above 6 in 22 specimens (0.53 %), 13 above 5 in 10 (0.24 %, the same ten as
  "5 does not top the group"), 6 above 14 in 2, 14 above 7 in 1. A convention
  that a hand-digitized corpus already satisfies to that degree is a convention,
  not a preference, and the residue is worth looking at one specimen at a time.
* An inversion is reported but **never corrected automatically**: moving a point
  to satisfy the order would invent a measurement rather than repair one. The
  dialog therefore offers *Measure again* (which selects and zooms on the point
  found on the wrong side, not the reference it was compared with) and *Save
  without correcting*; the *Correct automatically* button only appears when
  there is an extreme-point violation, which is the only kind that can be
  repaired by moving a landmark.
* Same tolerance as the extremes, `max(5 px, 0.003 * Bl)`, and the same
  invariance: heights are read perpendicular to the body axis and the dorsal
  side from the relative position of 3 and 4, so the test holds head left or
  right, photograph flipped, or mirrored. Missing points are stepped over rather
  than breaking the chain.
* New internals `.fm_eye_order_violations()` and `.fm_convention_violations()`;
  the violation tables gain a `kind` column (`"extreme"` / `"order"`).

## A tabbed, themed digitizer

* The side panel of `launch_fishmorph_digitizer()` is a **tabset** — `Specimen`,
  `Display`, `Checks`, `Seed` — instead of one long scroll. The controls
  fall into groups touched at different rhythms (once per specimen, once per
  photograph, once per session), and stacking them in one column put the ones
  used constantly below the ones used never.
* With `bslib` (already in `Suggests`) the page uses a Bootstrap 5 theme, cards
  and a wider sidebar; without it, the same content falls back to the standard
  Shiny layout. No feature depends on `bslib`, only the appearance does. The
  page is deliberately **not** `fillable`: it is a document that scrolls, and in
  a filling page the 620 px photograph is squeezed by the bars above and the
  panels below.
* The **queue selector** (`To reconstruct` / `Correct existing` / `New
  photographs`) moves from the action bar to the head of the side panel: it decides
  what the whole session is doing, and it sat one button away from
  "Save & next". `Mark NA` moves the other way, next to the landmark bar,
  with the zoom controls: those act on the point under the cursor.
* The **landmark bar is bare and on one line** — the buttons share the width
  rather than wrapping, so a given point keeps its place on screen whatever the
  window size. The entry order, the broken-axis conventions and the colour code
  move to a card at the foot of the page: they are read on the first specimen
  and never again, but above the photograph they cost three lines of scroll on
  each of the following thousands.
* A header strip shows what the session IS — workbook, photographs, journal,
  operator — since those are arguments of the launcher and are not editable from
  the app. The `Checks` tab shows the state of both write layers.

## Every point is written, and a lost one is now loud

* Verified end to end: `save_pts = 1..19, 22, 23, 24, 25` for the landmark sheet
  (and `+ 20, 21` for new specimens), the columns `24_X`..`25_Y` being created at
  start-up by `ensure_cols()`. Over the 201 records of the existing journals,
  every record carries its 23 points: 22 `placed` 201/201, 23 `derived` 165 (36
  `na`, the derivation needing 6 and 9), 24 `placed` 196, 25 `placed` 9 — the
  hinges being optional by design.
* The on-screen legend claimed **"24/25 are NOT recorded"**, which had
  been untrue since the hinge columns were added. An operator reading it had
  every reason not to bother placing them. Corrected, and the legend now states
  what is written and why: the hinges are not landmarks and belong in no shape
  analysis, but they define the frames each convention was applied in — without
  them a species reopened for correction comes back with a straight axis.
* Writing a point whose column is missing from the sheet used to `next` in
  silence. It now raises a persistent error naming the points, since a
  hand-edited sheet is the one case where a placed point could vanish without
  trace (the journal, as always, still has it).

# Rfishmorph 0.3.0

## Extreme-point convention checked on save

* `launch_fishmorph_digitizer()` now verifies, when "Save & next" is
  pressed, that landmark 3 is the most **dorsal** and landmark 4 the most
  **ventral** point of the body outline — the definition of `Bd` as the maximum
  body depth. A specimen whose 5 (head top) sits above 3, or whose 6 (head
  bottom) sits below 4, silently under-estimates `Bd`; this is now caught before
  anything reaches the journal or the workbook.
* On violation a dialog offers three routes: **remeasure** (the offending point
  becomes active and the view centres on it), **auto-correct** (3, resp. 4, takes
  the height of the point overshooting it while keeping its position along the
  axis, so `Bd` grows and the 3-4 perpendicularity convention is preserved), or
  **save as is**.
* Heights are measured perpendicular to the body axis 1-2, so a tilted
  photograph does not bias the test, and the dorsal side is inferred from the
  relative position of 3 and 4 — the check therefore holds whatever the
  orientation (head left or right, flipped photograph, "Flip dorsal/ventral"
  ticked). Caudal peduncle and fin (16-19) and appendage tips (12 pectoral,
  15 jaw) are excluded, as are the scale bar (20, 21), the derived point (23)
  and the hinges (24, 25). Tolerance: 0.003 of body length.
* The **derived ventral points 8, 9 and 11 are excluded too**: they are computed
  from landmark 4 (belly line), so testing whether 4 is the lowest point against
  them is circular. Settled on the data rather than by argument -- over the 1,036
  digitized T-26 specimens of the intraitR corpus, including 8/9/11 flags 20.6%
  of the batch (198 of 213 flags are those three points, median overshoot 0.5% of
  `Bl`, i.e. belly-line noise), whereas excluding them flags 1.5% at a median
  overshoot of 6.8% of `Bl`, with a flag rate flat from 0.003 to 0.02 `Bl`.
  The comparison set is therefore 1, 2, 5, 6, 7, 10, 13, 14, 22 -- the landmarks
  that are independent measurements on the body outline.
* Tolerance is `max(5 px, 0.003 * Bl)`. The 5 px absolute floor matters on small
  photographs, where the relative term falls below click noise. It is free:
  compliant T-26 specimens top out at -0.4 px of overshoot (p98) while the
  smallest real breach is 11.8 px, so any floor from 1 to 8 px flags the same
  16 specimens.
* The check is toggled by the new "Check 3/4 (extremes) on save"
  box (on by default).
* New journal status **`"adjusted"`** for points relocated by that automatic
  correction, distinct from `"placed"` (operator-pointed) and `"seeded"`
  (never verified). `fishmorph_journal_qc()` reports them, so an auto-corrected
  `Bd` remains traceable specimen by specimen.

# Rfishmorph 0.2.0

## Package renamed

* `FishMORPHR` is now **`Rfishmorph`**. Update `library()` calls accordingly.

## Digitizing application

* New `launch_fishmorph_digitizer()`: works directly on the FISHMORPH workbook
  with three switchable queues — `"reconstruct"` (species without landmarks),
  `"correct"` (already landmarked, reloaded from the workbook) and `"new"`
  (photographs absent from the workbook, appended to a `new_specimens` sheet).
  Adds a broken body axis with hinge points 22/24/25 for curved specimens, an
  optional 20/21 scale bar giving `mm_per_px`, and constrained editing that
  enforces the FISHMORPH geometric conventions live.
* In `"new"` mode there are no measured segments: points are seeded from the
  **median segment/Bl proportions of the 9556 reference species**, then corrected
  by hand. No length is locked in that mode.
* `launch_fishmorph_reconstructor()` is **deprecated** and redirects to the new
  tool. The old single-photo prototype was removed: two implementations of the
  same geometry were bound to diverge.

## Data layer: append-only journal

* New `fm_journal_open()`, `fm_journal_append()`, `fm_journal_read()`,
  `fm_journal_status()`, `fm_journal_history()`, `fishmorph_consolidate()`,
  `fishmorph_journal_qc()`.
* Every save is appended to a per-session TSV in long format (one row per point)
  before anything is written to the workbook. A crash can at worst truncate the
  last line, which is detected and discarded on read.
* Each point records a `status`: `placed`, `seeded` (never verified by the
  operator), `derived`, `na`. This distinction is invisible in a wide
  coordinate table and is what makes quality control possible.
* Workbook writes are now **atomic** (`fm_save_workbook_atomic()`: temporary file
  then rename, previous generation kept as `.prev.xlsx`) and batched via
  `xlsx_flush_every`, taking the multi-megabyte rewrite out of the input loop.

## Data layer: derived DuckDB database

* New `fishmorph_build_db()`, `fishmorph_db_connect()`, `fishmorph_validate()`.
* Three constrained tables (`record`, `specimen`, `landmark_obs`) and three
  views (`v_landmarks_wide`, `v_ratios`, `v_specimen_qc`), plus Parquet and CSV
  exports for archiving.
* `fishmorph_validate()` adds morphometric plausibility on top of SQL
  constraints, comparing each specimen to the empirical envelope (quantiles
  0.001 and 0.999) of the 9556 reference species.

## Exploration application

* New `launch_fishmorph_space()`: the former stand-alone *FishMorphSpace* app,
  now shipped in `inst/shiny/fishmorph_space/`, with the 9556-species trait table
  embedded in `inst/extdata/fishmorph_data.csv` (`fishmorph_space_data()`).
  A `data =` argument points it at a more recent table.

## Phylogenetic imputation

* New `load_fishmorph_phylo_axes()`: reads `inst/extdata/Phylogeny/
  pcoaPhylogenyFish.rds`, the **precomputed** PCoA axes of the global fish
  phylogeny (8,970 species, 10 axes), cached once per session. Shipped as a
  compressed `.rds` (540 kB instead of 1.8 MB of text); the loader still accepts
  the whitespace-separated text format, dispatching on the file extension.
* `"missforest_phylo"` now uses that table by default instead of
  eigendecomposing the patristic distance matrix on every call. Beyond the cost,
  this fixes a **comparability** problem: axes recomputed on whichever species
  happened to be present defined a different coordinate system for each
  analysis, so two imputations on two subsets did not live in the same
  phylogenetic space.
* `impute_traits()`, `impute_landmarks()` and `fishmorph_trait_space()` gain a
  `phylo_axes` argument to supply an alternative table. Passing `tree` still
  recomputes from that tree, as before; the bundled tree remains the fallback if
  the table is unreachable.
* The imputation message now names the **source** of the axes, so two runs can be
  told apart.
* **`species` is now a separate argument** from `groups` in `impute_traits()`,
  `impute_landmarks()`, `fishmorph_trait_space()` and
  `compare_segments_landmarks()`. The two were conflated, so
  `"missforest_phylo"` refused to work without a `groups` vector -- yet the
  phylogeny only needs to know which species each row belongs to, not a
  categorical predictor for the forest. `species` is auto-detected from a
  `Genus.species` / `Species` / `species` column (or, for landmarks, from the
  metadata or the specimen names); `groups` is no longer auto-filled with
  species. In `compare_segments_landmarks()` the key is taken from `id_col`,
  not from `group_col`.
* A `groups` factor with more than 53 levels is now dropped from the missForest
  predictors with a warning instead of failing: `randomForest` cannot handle more
  than 53 categories, so auto-filling `groups` with several thousand species
  names would have made the imputation error out.

## Bug fixes

* `correct_geometry_conventions()` failed with `'vec' must be sorted
  non-decreasingly and not contain NAs` when a hinge landmark (22, 24 or 25)
  projected outside the snout-to-caudal segment. Such a hinge is now discarded
  from the axis polyline, with an aggregated warning naming the specimens.

## Dependencies

* All application and database packages are in `Suggests`; each launcher checks
  its own dependencies and prints a ready-to-paste `install.packages()` call.

## Earlier in this cycle

* `project_fishmorph()` is now a faithful port of `intraitR::project_fishmorph()`
  (specimens projected into the frozen FISHMORPH space built from a reference
  database), with `print()` and `plot()` methods. `plot(proj, style = "hull")`
  reproduces the intraitR figure: reference kernel-density heatmap + per-species
  convex hulls / spider / density, species legend, optional loading arrows and
  `itv_reference` points. Added `group_colors()` / `reset_group_colors()`.
* Imputation aligned with intraitR: `impute_landmarks()`, `phylo_pcoa()`,
  `load_fishmorph_phylogeny()`, and `fishmorph_trait_space(na_action = ...)`
  including `"missforest_phylo"`.

# Rfishmorph 0.1.0

First release. FISHMORPH routines split out of `intraitR` into a stand-alone
package.

* Schema and core measurements: `fishmorph_schema()`, `fishmorph_segments()`,
  `fishmorph_ratios()`, `fishmorph_traits()`.
* Landmark container and I/O interoperable with `intrait_landmarks`:
  `fishmorph_landmarks()`, `read_landmarks_csv()`, `write_landmarks_csv()`.
* Geometry: `standardize_geometry()`, `correct_geometry_conventions()`.
* Reconstruction (inverse of the segments) with an exact round-trip guarantee:
  `reconstruct_fishmorph_landmarks()`, `check_reconstruction_roundtrip()`,
  `launch_fishmorph_reconstructor()`.
* Functional trait space (frozen PCA + projection): `fishmorph_trait_space()`,
  `project_fishmorph()`, `load_fishmorph_reference()`.
* Visualization (ggplot2 + base fallback): `plot_landmarks()`,
  `plot_trait_space()`, `plot_trait_distributions()`,
  `plot_segment_landmark_agreement()`.
* Quality control: `compare_segments_landmarks()`, `agreement_metrics()`,
  `check_infinite_ratios()`, `check_geometry_conventions()`.
* Species management via `rfishbase`: `validate_species_names()`,
  `update_fishmorph_taxonomy()`, `freshwater_fish_list()`,
  `new_fishmorph_species()`, `add_fishmorph_species()`.
