# Neuropsychological Test Scores

Create an `npsych_scores` object, a validated numeric vector
representing neuropsychological test scores. This is the parent class
for all test-specific subclasses (e.g., `MOCATOTS`, `TRAILA`).

## Usage

``` r
npsych_scores(
  .data = numeric(0),
  label = character(0),
  domain = character(0),
  short_descriptor = character(0),
  range = numeric(0),
  codes = numeric(0)
)
```

## Arguments

- .data:

  Numeric vector of test scores. Values must fall within `range` or
  match one of the `codes`, or be `NA`.

- label:

  A single string identifying the test (e.g., `"MoCA"`,
  `"Trail Making Test Part A"`).

- domain:

  A single string identifying the domain the test belongs to (e.g.,
  `"General Cognition"`, `"Language"`). Defaults to an empty character
  vector.

- short_descriptor:

  A short descriptor giving a little bit more information than the
  label. When available, the entry from the Researchers Data Dictionary;
  see [rdd](https://rmtrane.github.io/ntrs/reference/rdd.md) for more.

- range:

  A numeric vector of length 2 giving the minimum and maximum valid
  scores.

- codes:

  A named numeric vector of error/special codes (e.g.,
  `c("Not administered" = 88)`). Defaults to an empty numeric vector.

## Value

An S7 object of class `npsych_scores`, inheriting from `class_double`,
with properties `label`, `range`, and `codes`.
