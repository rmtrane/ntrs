# Standardization Version

Create a `std_version` object that identifies a specific standardization
version for a neuropsychological test. This is the base class for
method-specific versions (e.g., norms-based or regression-based).

## Usage

``` r
std_version(
  scores_class = character(0),
  method_name = character(0),
  version_id = character(0),
  description = character(0)
)
```

## Arguments

- scores_class:

  A single non-empty string giving the `npsych_scores` subclass this
  version applies to (e.g., `"MOCATOTS"`).

- method_name:

  A single non-empty string identifying the standardization method
  (e.g., `"norms"`, `"regression"`).

- version_id:

  A single non-empty string uniquely identifying this version within its
  method and scores class.

- description:

  An optional single string describing the version.

## Value

An S7 object of class `std_version` with properties `scores_class`,
`method_name`, `version_id`, and `description`.
