# Register a standardization version

Stores a
[std_version](https://rmtrane.github.io/ntrs/reference/std_version.md)
object (e.g.,
[norms_version](https://rmtrane.github.io/ntrs/reference/norms_version.md),
[regression_version](https://rmtrane.github.io/ntrs/reference/regression_version.md))
in the `.std_versions` registry, organized by method, scores class, and
version ID.

## Usage

``` r
.register_std_version(version_obj, overwrite = FALSE)
```

## Arguments

- version_obj:

  An S7 object inheriting from
  [std_version](https://rmtrane.github.io/ntrs/reference/std_version.md).
  Contains all version metadata (`scores_class`, `method_name`,
  `version_id`) and the method-specific data (e.g., `lookup_table` or
  `coefs`).

- overwrite:

  Logical. If `TRUE`, allows overwriting an existing version with a
  warning. If `FALSE` (default), attempting to register an existing
  version throws an error.

## Value

Invisible `NULL`. Called for side effects (storing data in
`.std_versions` environment).
