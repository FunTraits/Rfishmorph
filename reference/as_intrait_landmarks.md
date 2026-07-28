# Coerce to an intraitR `intrait_landmarks` object

Returns the same landmark configuration relabelled as
`intrait_landmarks`, so Rfishmorph outputs can be passed to `intraitR`
functions such as `plot_fishmorph_points()`, `gpa_fish()` or
[`fishmorph_segments()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_segments.md).
The internal layout (`coords`, `scale`, `metadata`) is identical between
the two classes; only the class attribute changes.

## Usage

``` r
as_intrait_landmarks(x)
```

## Arguments

- x:

  A `fishmorph_landmarks` (or interoperable) object.

## Value

An object of class `intrait_landmarks`.

## Examples

``` r
lm <- reconstruct_fishmorph_landmarks(
  list(Bl = 10, Bd = 3.2, Hd = 2.4, Eh = 1.1, Mo = 1.6, PFi = 1.9,
       PFl = 2.1, Ed = 0.6, Jl = 1.3, CPd = 1.0, CFd = 3.0))
x <- as_intrait_landmarks(lm)
class(x)
#> [1] "intrait_landmarks"
# intraitR::plot_fishmorph_points(x, specimen = "reconstructed")
```
