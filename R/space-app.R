# =============================================================================
# space-app.R -- lanceur de l'explorateur de l'espace morphologique.
#
# L'application elle-meme (anciennement le projet autonome "FishMorphSpace") vit
# dans inst/shiny/fishmorph_space/app.R. Elle n'est PAS reecrite ici : ce fichier
# ne fait que verifier ses dependances, resoudre le jeu de donnees, et la lancer.
#
# Le jeu de donnees par defaut est embarque dans inst/extdata/fishmorph_data.csv
# (9556 especes). ATTENTION : ses colonnes de traits sont DEJA en log10(x + 1) --
# ne pas les re-transformer avant de projeter de nouveaux individus.
# =============================================================================

# Dependances de l'app de visualisation. Elles sont en Suggests : qui veut
# seulement calculer des ratios n'a aucune raison d'installer plotly ou DT.
.FMS_DEPS <- c("shiny", "bslib", "ggplot2", "dplyr", "tidyr", "DT",
               "RColorBrewer", "scales", "ggrepel", "plotly", "MASS", "magrittr")

# Verifie un jeu de dependances et produit un message d'installation ACTIONNABLE
# (la ligne install.packages() prete a copier) plutot qu'un echec au chargement
# du premier library() manquant, a l'interieur de l'app.
.fm_require <- function(pkgs, what) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (!length(miss)) return(invisible(TRUE))
  stop(what, " requiert ", length(miss), " package(s) non installe(s) : ",
       paste(miss, collapse = ", "), "\n  install.packages(c(",
       paste0('"', miss, '"', collapse = ", "), "))", call. = FALSE)
}

#' Explorateur de l'espace morphologique des poissons d'eau douce
#'
#' Lance l'application 'shiny' qui explore l'espace morphologique global des
#' poissons d'eau douce a partir de la base FISHMORPH (Brosse et al. 2021) :
#' analyse en composantes principales des neuf traits sans dimension, densite de
#' l'espace fonctionnel, coloration par ordre ou par statut IUCN, et projection
#' d'un jeu d'especes fourni par l'utilisateur.
#'
#' @param data Chemin d'un CSV de traits (separateur point-virgule) remplacant le
#'   jeu embarque. Utile pour travailler sur une version plus recente, ou sur le
#'   resultat de [fishmorph_build_db()] exporte en CSV. NULL utilise le jeu
#'   embarque dans le package.
#' @param launch.browser Ouvrir dans le navigateur par defaut.
#' @param ... Passe a [shiny::runApp()] (par exemple `port`).
#' @return Invisiblement `NULL` ; appelee pour son effet de bord.
#' @seealso [launch_fishmorph_digitizer()] pour produire les landmarks,
#'   [project_fishmorph()] pour projeter des specimens par le calcul plutot
#'   qu'interactivement.
#' @examples
#' \dontrun{
#' launch_fishmorph_space()
#' launch_fishmorph_space(data = "mes_traits.csv")
#' }
#' @export
launch_fishmorph_space <- function(data = NULL, launch.browser = TRUE, ...) {
  .fm_require(.FMS_DEPS, "L'explorateur de l'espace morphologique")

  appdir <- system.file("shiny", "fishmorph_space", package = "Rfishmorph")
  if (!nzchar(appdir) || !file.exists(file.path(appdir, "app.R")))
    stop("Application introuvable dans le package. Reinstallez 'Rfishmorph'.",
         call. = FALSE)

  if (!is.null(data)) {
    if (!file.exists(data))
      stop("Jeu de donnees introuvable : ", data, call. = FALSE)
    old <- options(Rfishmorph.space_data = normalizePath(data))
    on.exit(options(old), add = TRUE)
  }
  shiny::runApp(appdir, launch.browser = launch.browser, ...)
  invisible(NULL)
}

#' Chemin du jeu de traits FISHMORPH embarque
#'
#' Renvoie le chemin du CSV utilise par defaut par [launch_fishmorph_space()],
#' pour l'inspecter ou le lire directement.
#'
#' Rappel : les colonnes de traits y sont DEJA en log10(x + 1). Les relire pour
#' projeter de nouveaux individus ne demande donc aucune transformation
#' supplementaire, et en appliquer une fausserait la projection.
#'
#' @return Chemin de fichier (chaine de caracteres).
#' @examples
#' p <- fishmorph_space_data()
#' if (nzchar(p)) utils::head(utils::read.csv(p, sep = ";"), 3)
#' @export
fishmorph_space_data <- function()
  system.file("extdata", "fishmorph_data.csv", package = "Rfishmorph")
