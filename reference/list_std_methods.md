# List available standardization methods for `npsych_scores`

Get methods that are available for a test class, or all available
methods.

## Usage

``` r
list_std_methods(scores)
```

## Arguments

- scores:

  A `npsych_scores` object, such as
  [`MOCATOTS()`](https://rmtrane.github.io/ntrs/reference/MOCATOTS.md).
  When the argument is supplied, only methods with registered versions
  for that specific test class are returned. When called via the
  `.npsych_scores` method with a missing argument, all registered
  methods are returned.

## Value

A character vector of standardization method names.
