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
  raw_scores_fn = function(x) x,
  post_proc_fn = function(x) x,
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

  A named list of functions. Names must match the non-statistic columns
  in `lookup_table`.

- raw_scores_fn:

  An optional function that is applied to the raw scores before
  standardization. Example: the model fitted to get the
  `updated_2025.06` regression coefficients for `TRAILA` was fitted to
  the negative `TRAILA` values to ensure higher values are better.
  Therefore `raw_scores_fn = \(x) -x` for this version.

- post_proc_fn:

  An option post processing function that is applied to standardized
  scores after the fact. For example, for norms based standardization of
  `TRAILA`, the sign of the z-scores are flipped to that larger z-scores
  are correlated with better performance. Hence,
  `post_proc_fn = \(x) -x`.

- description:

  An optional single string describing the version.

## Value

An S7 object of class `regression_version` with properties inherited
from `std_version` plus `coefs` and `covar_fns`.
