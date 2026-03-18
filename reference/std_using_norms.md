# Standardize `npsych_scores` using norms

Computes z-scores by looking up the mean and standard deviation from a
norms table, adjusting for covariates (e.g., age, sex, education).

## Usage

``` r
std_using_norms(scores, ...)
```

## Arguments

- scores:

  An `npsych_scores` object.

- ...:

  Named covariates required by the norms version (e.g., `age = 72`,
  `sex = 1`, `educ = 16`).

## Value

An
[std_npsych_scores](https://rmtrane.github.io/ntrs/reference/std_npsych_scores.md)
object containing z-scores.
