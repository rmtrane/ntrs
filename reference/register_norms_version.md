# Register a norms-based standardization version

Creates a
[norms_version](https://rmtrane.github.io/ntrs/reference/norms_version.md)
object and registers it in the `.std_versions` registry via
[`.register_std_version()`](https://rmtrane.github.io/ntrs/reference/dot-register_std_version.md).

## Usage

``` r
register_norms_version(
  scores,
  version,
  lookup_table,
  raw_scores_fn = function(x) x,
  covar_fns,
  post_proc_fn = function(x) x,
  overwrite = FALSE,
  description = ""
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

- lookup_table:

  Data frame containing norms. Must include columns `m` (mean) and `sd`
  (standard deviation), may include `n`(sample size). Additional columns
  are treated as covariate grouping variables and must have matching
  entries in `covar_fns`. See
  [norms_version](https://rmtrane.github.io/ntrs/reference/norms_version.md)
  for validation details.

- covar_fns:

  A named list of functions that transform raw covariate inputs to match
  the levels in `lookup_table`. Names must match the non-statistic
  columns (everything but `m`, `sd`, `n`) in `lookup_table`.

- overwrite:

  Logical. If `FALSE` (the default), an error is thrown if a version
  with the same `version` and scores class already exists in the
  registry. If `TRUE`, the existing version is overwritten with the new
  one.

- description:

  Optional character string describing this version.

## Value

Invisible `NULL`. Called for its side effect of registering the version
in the internal `.std_versions` registry.
