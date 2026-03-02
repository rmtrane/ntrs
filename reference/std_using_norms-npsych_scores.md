# Standardize test scores using norms

A short description...

## Arguments

- scores:

  A numeric vector of raw test scores.

- ...:

  Additional numeric vectors representing covariates, for example `age`,
  `educ`, `sex`. These must be of length one or the same length as
  `npsych_scores`, and named as columns in the `lookup_table` for the
  specified `version`; use
  `get_version_data({npsych_scores}, "norms", {version})$lookup_table`
  to inspect the table.

- version:

  A single string specifying the version of the norms to use.

## Value

A numeric vector of standardized scores. The function will error if
`version` is not registered, if provided covariates are not numeric, if
required covariates are missing based on the `lookup_table` for the
specified `version`, or if covariate lengths are mismatched.
