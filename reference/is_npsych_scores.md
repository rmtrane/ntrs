# Is `x` an npsych_scores object?

Tests whether `x` is an `npsych_scores` object.

## Usage

``` r
is_npsych_scores(x)
```

## Arguments

- x:

  An R object.

## Value

A single logical value: `TRUE` if `x` is an `npsych_scores` object,
`FALSE` otherwise.

## Examples

``` r
moca <- MOCATOTS(c(25, 28))
is_npsych_scores(moca) # TRUE
#> [1] TRUE

traila <- c(53, 75)
is_npsych_scores(traila) # FALSE
#> [1] FALSE

```
