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
  covar_fns = NULL,
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

  A named numeric vector or a data frame of regression coefficients.
  Must include `"intercept"` and `"rmse"` entries. Additional names are
  treated as covariate coefficients. See
  [regression_version](https://rmtrane.github.io/ntrs/reference/regression_version.md)
  for validation details.

- covar_fns:

  A named list of functions that transform raw covariate inputs. Must
  include entries for all coefficient names other than `"intercept"` and
  `"rmse"`.

- description:

  Optional character string describing this version.

- overwrite:

  Logical. If `TRUE`, allows overwriting an existing version with a
  warning. Defaults to `FALSE`.

## Value

Invisible `NULL`. Called for its side effect of registering the version
in the internal `.std_versions` registry.
