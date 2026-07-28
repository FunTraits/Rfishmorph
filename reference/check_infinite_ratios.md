# Diagnose non-finite or extreme FISHMORPH ratios

Flags specimens whose ratios are infinite, `NaN` or extreme outliers,
usually caused by a zero/near-zero denominator segment (a digitizing
error). Ported from diagnostic_infinite_ratios.R.

## Usage

``` r
check_infinite_ratios(data, z_thresh = 8)
```

## Arguments

- data:

  A data frame of ratios or segments, or a `fishmorph_landmarks` object.

- z_thresh:

  Absolute robust z-score (median/MAD) above which a finite value is
  also reported as an outlier. Default 8.

## Value

A data frame of flagged `id`, `trait`, `value` and `reason`
(`"infinite"`, `"nan"`, `"outlier"`). Zero rows means the data are
clean.
