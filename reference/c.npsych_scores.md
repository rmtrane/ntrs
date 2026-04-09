# Combine `npsych_scores` objects

Combine `npsych_scores` objects

## Usage

``` r
# S3 method for class 'npsych_scores'
c(x, ...)
```

## Arguments

- x:

  An `npsych_scores` object.

- ...:

  Additional `npsych_scores` objects to combine.

## Value

An `npsych_scores` object containing the combined data from `x` and
`...`. The function will error if different subclasses of
`npsych_scores` are provided.
