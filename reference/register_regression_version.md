# Register a regression-based standardization version

Creates a
[regression_version](https://rmtrane.github.io/ntrs/reference/regression_version.md)
object and registers it in the `.std_versions` registry via
[`.register_std_version()`](https://rmtrane.github.io/ntrs/reference/dot-register_std_version.md).

## Usage

``` r
register_regression_version(
  scores,
  version,
  coefs,
  raw_scores_fn = function(x) x,
  covar_fns = NULL,
  post_proc_fn = function(x) x,
  description = "",
  overwrite = FALSE
)
```

## Arguments

- scores:

  A `npsych_scores` object, such as
  [`MOCATOTS()`](https://rmtrane.github.io/ntrs/reference/MOCATOTS.md).
  Used to determine the scores class for registration.

- version:

  Character string identifying this version (e.g., `"nacc"`,
  `"updated"`).

- coefs:

  A named numeric vector of regression coefficients. Must include
  `"intercept"` and `"rmse"` entries.

- raw_scores_fn:

  An optional function that is applied to the raw scores before
  standardization. Example: the model fitted to get the
  `updated_2025.06` regression coefficients for `TRAILA` was fitted to
  the negative `TRAILA` values to ensure higher values are better.
  Therefore `raw_scores_fn = \(x) -x` for this version.

- covar_fns:

  A named list of functions. Names must match the non-statistic columns
  in `lookup_table`.

- post_proc_fn:

  An option post processing function that is applied to standardized
  scores after the fact. For example, for norms based standardization of
  `TRAILA`, the sign of the z-scores are flipped to that larger z-scores
  are correlated with better performance. Hence,
  `post_proc_fn = \(x) -x`.

- description:

  An optional single string describing the version.

- overwrite:

  Logical. If `FALSE` (the default), an error is thrown if a version
  with the same `version` and scores class already exists in the
  registry. If `TRUE`, the existing version is overwritten with the new
  one.

## Value

Invisible `NULL`. Called for its side effect of registering the version
in the internal `.std_versions` registry.
