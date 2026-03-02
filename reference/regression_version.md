# Regression-Based Standardization Version

Create a `regression_version` object that stores regression coefficients
and covariate functions for regression-based standardization. Inherits
from
[std_version](https://rmtrane.github.io/ntrs/reference/std_version.md).

## Usage

``` r
regression_version(
  scores_class,
  version_id,
  coefs,
  covar_fns,
  description = ""
)
```

## Arguments

- scores_class:

  A single non-empty string giving the `npsych_scores` subclass this
  version applies to (e.g., `"MOCATOTS"`).

- version_id:

  A single non-empty string uniquely identifying this version within its
  method and scores class.

- coefs:

  A named numeric vector of regression coefficients. Must include
  `"intercept"` and `"rmse"` entries.

- covar_fns:

  A named list of functions. May include entries for all coefficient
  names other than `"intercept"` and `"rmse"`.

- description:

  An optional single string describing the version.

## Value

An S7 object of class `regression_version` with properties inherited
from `std_version` plus `coefs` and `covar_fns`.
