# Standardize test scores using regression

A short description...

## Arguments

- scores:

  A vector coercible to numeric, representing raw test scores.

- ...:

  Additional numeric covariates to be included in the regression model.
  Each covariate must be a numeric vector of length 1 or the same length
  as `npsych_scores`, and must be named after a coefficient present in
  the `version` data; use
  `get_version_data({npsych_scores}, "regression", {version})$coefs` to
  inspect the coefficients.

- version:

  A single string specifying the version of the regression model to use
  for standardization.

## Value

A numeric vector of standardized test scores. The function will error if
any supplied covariates are not numeric, if required covariates for the
specified `version` are missing, or if covariate lengths are mismatched.
