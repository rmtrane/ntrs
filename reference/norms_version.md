# Norms-Based Standardization Version

Create a `norms_version` object that stores a lookup table and covariate
functions for norms-based standardization. Inherits from
[std_version](https://rmtrane.github.io/ntrs/reference/std_version.md).

## Usage

``` r
norms_version(
  scores_class,
  version_id,
  lookup_table,
  covar_fns,
  description = ""
)
```

## Arguments

- scores_class:

  A single non-empty string giving the `npsych_scores` subclass this
  version applies to (e.g., `"MOCATOTS"`).

- version_id:

  A single non-empty string uniquely identifying this version within its
  method and scores class.

- lookup_table:

  A data frame containing columns `m` (mean) and `sd` (standard
  deviation), plus covariate columns.

- covar_fns:

  A named list of functions. Names must match the non-statistic columns
  in `lookup_table`.

- description:

  An optional single string describing the version.

## Value

An S7 object of class `norms_version` with properties inherited from
`std_version` plus `lookup_table` and `covar_fns`.
