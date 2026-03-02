# Get available version names for a standardization method

Get available version names for a standardization method

## Usage

``` r
list_method_versions(scores, method)
```

## Arguments

- scores:

  A `npsych_scores` object, such as
  [`MOCATOTS()`](https://rmtrane.github.io/ntrs/reference/MOCATOTS.md).

- method:

  Character string identifying the standardization method (e.g.,
  `"norms"`, `"regression"`).

## Value

A character vector of registered version names.
