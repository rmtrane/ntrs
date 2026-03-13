# Standardize all `npsych_scores` columns in a data frame

Finds every column in `data` that inherits from `npsych_scores`,
standardizes it via
[`std()`](https://rmtrane.github.io/ntrs/reference/std.md), and adds the
result as a new column with a `"z_"` prefix (e.g., `MOCATOTS` →
`z_MOCATOTS`).

Each score class can use a different method/version combination. Classes
not mentioned in `methods` fall back to their registered defaults (see
[`set_std_defaults()`](https://rmtrane.github.io/ntrs/reference/set_std_defaults.md)).

## Usage

``` r
std_data(
  data,
  ...,
  methods = list(),
  prefix_std = "z_",
  prefix_raw = NULL,
  .cols = NULL
)
```

## Arguments

- data:

  A `data.frame` or `data.table` containing one or more `npsych_scores`
  columns.

- ...:

  Named covariates shared across all score columns (e.g., `age`, `sex`,
  `educ`). These are passed to every
  [`std()`](https://rmtrane.github.io/ntrs/reference/std.md) call and
  must be supplied explicitly. Values can be bare column names
  referencing columns in `data` (e.g., `age = age`) or explicit vectors
  (e.g., `age = c(72, 65, 80)`).

- methods:

  An optional named list keyed by `npsych_scores` **class name** (not
  column name). Each element is a named character vector of the form
  `c(method = "...", version = "...")`. Both elements are optional
  within each entry; omitted elements resolve to the registered default.
  Classes not listed use their defaults.

      methods = list(
        MOCATOTS = list(method = "regression", version = "nacc"),
        ANIMALS  = list(method = "norms")
      )

- prefix_std:

  A single string prepended to each score column name to form the new
  column name. Defaults to `"z_"`.

- prefix_raw:

  Optional; if character string, used as prefix for scores that are
  standardized. This enables you to obtain pairs of scores, for example
  `raw_MOCATOTS` and `z_MOCATOTS` for bookkeeping

- .cols:

  An optional character vector of `npsych_scores` **column names** to
  standardize. When `NULL` (the default), all `npsych_scores` columns
  are processed.

## Value

The input `data` with additional standardized score columns appended.
The original columns are left unchanged. The return type matches the
input type (`data.frame` or `data.table`).

## See also

[`std()`](https://rmtrane.github.io/ntrs/reference/std.md) for
standardizing a single `npsych_scores` vector.

## Examples

``` r
if (FALSE) { # \dontrun{
# Bare column names — evaluated against data
std_data(my_data, age = age, sex = sex, educ = educ)

# Explicit vectors work too
std_data(my_data, age = my_data$age, sex = my_data$sex, educ = my_data$educ)

# Override method/version for specific test classes
std_data(
  my_data,
  methods = list(
    MOCATOTS = list(method = "norms", version = "nacc"),
    ANIMALS  = list(method = "regression", version = "updated_2025.06")
  ),
  age = age, sex = sex, educ = educ
)
} # }
```
