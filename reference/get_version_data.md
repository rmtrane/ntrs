# Get version data

Retrieve the standardization data for a specific method and version of a
test score class from the `.std_versions` registry. When `method` and/or
`version` are `NULL`, defaults are resolved via
[`get_std_defaults()`](https://rmtrane.github.io/ntrs/reference/get_std_defaults.md).

## Usage

``` r
get_version_data(scores, method = NULL, version = NULL)
```

## Arguments

- scores:

  An `npsych_scores` object (e.g.,
  [`MOCATOTS()`](https://rmtrane.github.io/ntrs/reference/MOCATOTS.md)).

- method:

  A single string specifying the standardization method (e.g.,
  `"norms"`, `"regression"`). If `NULL` (default), the default method
  for the scores class is used.

- version:

  A single string specifying the version name. If `NULL` (default), the
  default version for the resolved method is used. Must be `NULL` when
  `method` is `NULL`.

## Value

The registered standardization data for the requested method/version
combination. Errors if the specified (or resolved default) version does
not exist.
