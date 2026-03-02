# Standardize neuropsychological test scores

User-facing wrapper that dispatches to the appropriate `std_using_*()`
method for an `npsych_scores` object. When `method` and `version` are
omitted, the registered defaults (see
[`set_std_defaults()`](https://rmtrane.github.io/ntrs/reference/set_std_defaults.md))
are used.

## Usage

``` r
std(scores, ..., method = NULL, version = NULL)
```

## Arguments

- scores:

  An `npsych_scores` object, such as `MOCATOTS(c(25, 28))`.

- ...:

  Named covariates passed through to the underlying `std_using_*()`
  method (e.g., `age`, `sex`, `educ`).

- method:

  A single string naming the standardization method (e.g., `"norms"`,
  `"regression"`). When `NULL` (the default), the default method
  registered via
  [`set_std_defaults()`](https://rmtrane.github.io/ntrs/reference/set_std_defaults.md)
  is used.

- version:

  A single string identifying the version of the method (e.g., `"nacc"`,
  `"updated_2025.06"`). When `NULL` (the default), the default version
  registered via
  [`set_std_defaults()`](https://rmtrane.github.io/ntrs/reference/set_std_defaults.md)
  is used. Ignored when the resolved method does not use versioned data.

## Value

A numeric vector of standardized scores.

## See also

[`std_data()`](https://rmtrane.github.io/ntrs/reference/std_data.md) for
batch-standardizing every `npsych_scores` column in a data frame,
[`list_std_methods()`](https://rmtrane.github.io/ntrs/reference/list_std_methods.md),
[`list_method_versions()`](https://rmtrane.github.io/ntrs/reference/list_method_versions.md),
[`get_std_defaults()`](https://rmtrane.github.io/ntrs/reference/get_std_defaults.md),
[`set_std_defaults()`](https://rmtrane.github.io/ntrs/reference/set_std_defaults.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Using registered defaults
std(MOCATOTS(c(25, 28)), age = 72, sex = 1, educ = 16)

# Specifying method and version explicitly
std(MOCATOTS(c(25, 28)), method = "norms", version = "nacc",
    age = 72, sex = "m", educ = 16)
} # }
```
