# =============================================================================
# shiny-reconstructor.R -- compatibilite ascendante.
#
# L'outil de digitalisation vit desormais dans R/digitizer-app.R sous le nom
# launch_fishmorph_digitizer(). L'ancien prototype (une photo isolee chargee par
# upload, parametres au curseur, export CSV) a ete retire : maintenir deux
# implementations de la MEME geometrie garantissait qu'elles finiraient par
# diverger, et c'est exactement le genre de divergence silencieuse qui produit
# deux jeux de landmarks incomparables dans une meme base.
#
# Ce fichier ne conserve qu'un alias deprecie.
# =============================================================================

#' Lance l'outil de reconstruction (deprecie)
#'
#' Depuis la version 0.2.0, l'outil de digitalisation est
#' [launch_fishmorph_digitizer()], qui travaille directement sur le classeur
#' FISHMORPH, gere trois files de travail (reconstruire, corriger, nouvelles
#' photos) et securise chaque enregistrement par un journal append-only.
#'
#' L'ancien prototype ne prenait qu'une photo a la fois et exportait un CSV
#' isole : il n'existe pas de correspondance exacte entre ses arguments et ceux
#' du nouvel outil, qui exige un classeur. Cette fonction avertit puis redirige.
#'
#' @param segments_csv Ignore. Conserve pour ne pas casser les appels existants.
#' @param ... Passe a [launch_fishmorph_digitizer()].
#' @return Invisiblement `NULL`.
#' @seealso [launch_fishmorph_digitizer()]
#' @export
launch_fishmorph_reconstructor <- function(segments_csv = NULL, ...) {
  if (!is.null(segments_csv))
    warning("`segments_csv` n'est plus utilise : le nouvel outil lit les ",
            "segments directement dans le classeur FISHMORPH.", call. = FALSE)
  .Deprecated(
    new = "launch_fishmorph_digitizer",
    package = "Rfishmorph",
    msg = paste0(
      "launch_fishmorph_reconstructor() est deprecie depuis Rfishmorph 0.2.0.\n",
      "  Utilisez launch_fishmorph_digitizer(xlsx_path =, photo_dir =, mode =) :\n",
      "  l'outil travaille sur le classeur FISHMORPH et journalise chaque ",
      "enregistrement."))
  launch_fishmorph_digitizer(...)
}
