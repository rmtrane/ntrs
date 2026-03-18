# Standardized neuropsychological test scores

An S7 class representing the result of standardizing an `npsych_scores`
vector via [`std()`](https://rmtrane.github.io/ntrs/reference/std.md),
[`std_using_norms()`](https://rmtrane.github.io/ntrs/reference/std_using_norms.md),
or
[`std_using_regression()`](https://rmtrane.github.io/ntrs/reference/std_using_regression.md).
Inherits from
[`S7::class_double`](https://rconsortium.github.io/S7/reference/base_classes.html),
so it behaves as a numeric vector while carrying standardization
metadata as S7 properties.

## Usage

``` r
std_npsych_scores(
  .data = numeric(0),
  scores_subclass = character(0),
  description = character(0),
  method = character(0),
  version = character(0)
)
```

## Arguments

- .data:

  A numeric vector of standardized (z-score) values.

- scores_subclass:

  A string naming the originating `npsych_scores` subclass (e.g.,
  `"MOCATOTS"`), or `NULL`.

- description:

  A non-empty string summarizing the standardization applied, including
  the method, version, and covariates.

- method:

  A non-empty string indicating the standardization method (e.g.,
  `"norms"`, `"regression"`).

- version:

  A string identifying the version used (e.g., `"nacc"`,
  `"updated_2025.06"`), or `NULL`.

## Value

An S7 object of class `std_npsych_scores`, which behaves as a numeric
vector with additional properties: `scores_subclass`, `description`,
`method`, and `version`.

## Properties

- `scores_subclass`:

  (`character(1)` or `NULL`) The name of the originating `npsych_scores`
  subclass (e.g., `"MOCATOTS"`).

- `description`:

  (`character(1)`) A human-readable summary of the standardization
  applied, including the method, version, and covariates.

- `method`:

  (`character(1)`) The standardization method used (e.g., `"norms"`,
  `"regression"`).

- `version`:

  (`character(1)` or `NULL`) The version identifier (e.g., `"nacc"`,
  `"updated_2025.06"`).

## See also

[`std()`](https://rmtrane.github.io/ntrs/reference/std.md),
[`std_using_norms()`](https://rmtrane.github.io/ntrs/reference/std_using_norms.md),
[`std_using_regression()`](https://rmtrane.github.io/ntrs/reference/std_using_regression.md),
[`std_data()`](https://rmtrane.github.io/ntrs/reference/std_data.md)
