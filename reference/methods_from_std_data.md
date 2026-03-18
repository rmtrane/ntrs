# Extract standardization method and version from `std_data()` output

Reads the `method` and `version` properties from the
[std_npsych_scores](https://rmtrane.github.io/ntrs/reference/std_npsych_scores.md)
columns produced by
[`std_data()`](https://rmtrane.github.io/ntrs/reference/std_data.md),
and uses the `prefix_std` attribute on `dat` to strip the prefix from
column names.

## Usage

``` r
methods_from_std_data(dat, std_cols = NULL)
```

## Arguments

- dat:

  A `data.frame` or `data.table` returned by
  [`std_data()`](https://rmtrane.github.io/ntrs/reference/std_data.md).
  Must carry a `prefix_std` attribute (set automatically by
  [`std_data()`](https://rmtrane.github.io/ntrs/reference/std_data.md)).

- std_cols:

  Optional character vector of standardized column names to inspect. If
  omitted, all
  [std_npsych_scores](https://rmtrane.github.io/ntrs/reference/std_npsych_scores.md)
  columns are used.

## Value

A named list, one element per standardized column. Each element is a
character vector with `method` and `version` entries. Names correspond
to score column names with the `prefix_std` stripped.

## See also

[`std_data()`](https://rmtrane.github.io/ntrs/reference/std_data.md),
[`std()`](https://rmtrane.github.io/ntrs/reference/std.md),
[std_npsych_scores](https://rmtrane.github.io/ntrs/reference/std_npsych_scores.md)
