# Create an npsych_scores subclass

Factory function that creates a new S7 subclass of
[npsych_scores](https://rmtrane.github.io/ntrs/reference/npsych_scores.md)
with fixed `label`, `range`, and `codes` properties. The returned class
object is also registered in the `.npsych_classes` environment for
discovery by
[`list_npsych_scores()`](https://rmtrane.github.io/ntrs/reference/list_npsych_scores.md).

## Usage

``` r
new_npsych_scores(name, label, range, codes = numeric())
```

## Arguments

- name:

  A non-empty single string giving the subclass name (e.g.,
  `"MOCATOTS"`). Used as both the S7 class name and registry key.

- label:

  A single string identifying the test (e.g., `"MoCA"`,
  `"Trail Making Test Part A"`).

- range:

  A numeric vector of length 2 giving the minimum and maximum valid
  scores.

- codes:

  A named numeric vector of error/special codes (e.g.,
  `c("Not administered" = 88)`). Defaults to an empty numeric vector.

## Value

An S7 class object (subclass of
[npsych_scores](https://rmtrane.github.io/ntrs/reference/npsych_scores.md))
whose constructor accepts a single `.data` argument.
