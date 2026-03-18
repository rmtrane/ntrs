# Standardize `npsych_scores` using regression

Computes z-scores using regression-based norms: predicts the expected
score from covariates via a linear model, then returns the scaled
residual `(score - predicted) / RMSE`.

## Usage

``` r
std_using_regression(scores, ...)
```

## Arguments

- scores:

  An `npsych_scores` object.

- ...:

  Named covariates required by the regression version (e.g., `age = 72`,
  `sex = 1`, `educ = 16`).

## Value

An
[std_npsych_scores](https://rmtrane.github.io/ntrs/reference/std_npsych_scores.md)
object containing z-scores.
