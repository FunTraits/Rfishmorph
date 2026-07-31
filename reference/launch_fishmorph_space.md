# Morphological space explorer for freshwater fishes

Launches the 'shiny' application that explores the global morphological
space of freshwater fishes from the FISHMORPH database (Brosse et al.
2021): principal component analysis of the nine dimensionless traits,
density of the functional space, colouring by order or by IUCN status,
and projection of a user-supplied set of species.

## Usage

``` r
launch_fishmorph_space(data = NULL, source = NULL, launch.browser = TRUE, ...)
```

## Arguments

- data:

  Path to a trait CSV (semicolon-separated) replacing the bundled data
  set. Useful to work on a more recent version, or on the result of
  [`fishmorph_build_db()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_build_db.md)
  exported to CSV. `NULL` uses the data set bundled with the package.

- source:

  Which measurement campaign to explore: `"segment"` or `"landmark"`.
  `NULL` (default) follows `getOption("fishmorph.source", "segment")`.
  Ignored when `data` is given.

- launch.browser:

  Where the application opens. `TRUE` (default) or `"browser"` forces
  the system browser, past the RStudio Viewer pane; `"viewer"` restores
  the pane; `FALSE` opens nothing and prints the URL; a function is used
  as given.

- ...:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) (for
  example `port`).

## Value

Invisibly `NULL`; called for its side effect.

## See also

[`launch_fishmorph_digitizer()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_digitizer.md)
to produce the landmarks,
[`set_fishmorph_source()`](https://funtraits.github.io/Rfishmorph/reference/set_fishmorph_source.md),
[`project_fishmorph()`](https://funtraits.github.io/Rfishmorph/reference/project_fishmorph.md)
to project specimens by computation rather than interactively.

## Examples

``` r
if (FALSE) { # \dontrun{
launch_fishmorph_space()
launch_fishmorph_space(source = "landmark")
launch_fishmorph_space(data = "my_traits.csv")
} # }
```
