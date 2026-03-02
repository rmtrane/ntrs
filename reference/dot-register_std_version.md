# Register a standardization version

Stores a std_version object (e.g., norms_version, regression_version) in
the `.std_versions` registry, organized by method, scores class, and
version ID.

## Usage

``` r
.register_std_version(version_obj, overwrite = FALSE)
```

## Arguments

- version_obj:

  An S7 object inheriting from std_version. Contains all version
  metadata (`scores_class`, `method_name`, `version_id`) and the
  method-specific data (e.g., `lookup_table` or `coefs`).

- overwrite:

  Logical. If `TRUE`, allows overwriting an existing version with a
  warning. If `FALSE` (default), attempting to register an existing
  version throws an error.

## Value

Invisible `NULL`. Called for side effects (storing data in
`.std_versions` environment).
