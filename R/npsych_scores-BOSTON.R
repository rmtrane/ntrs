#' @include new_npsych_scores.R
NULL

#' BOSTON Test Scores
#'
#' @description Create a `BOSTON` object to hold BOSTON scores.
#'
#' @param x Numeric scores.
#'
#' @returns
#' An object of class `BOSTON`.
#'
#' @export
BOSTON <- new_npsych_scores(
  "BOSTON",
  label = "Boston Naming Test",
  domain = "Language",
  short_descriptor = "Boston Naming Test (30) - Total score",
  range = c(0, 30),
  codes = c(
    "Physical problem" = 95,
    "Cognitive/behavior problem" = 96,
    "Other problem" = 97,
    "Verbal refusal" = 98,
    "Not available: UDS form submitted" = -4
  )
)

#' Setup BOSTON method versions
#'
#' @description
#' Registers versions for the %s test class,
#' and sets a default method. Meant to be called in .onLoad
#'
#' @returns
#' `NULL`, invisibly. This function is called for its side effects of
#' registering test versions and setting defaults.
#'
#' @keywords internal
.setup_BOSTON_versions <- function() {
  var_name <- NULL
  # Register regression versions for BOSTON
  coefs <- subset(
    reg_coefs[["updated_2024.06"]],
    var_name == "BOSTON"
  )[, setdiff(
    names(reg_coefs[["updated_2024.06"]]),
    "var_name"
  )]

  register_regression_version(
    scores = BOSTON(),
    version = "updated_2024.06",
    coefs = coefs[names(which(unlist(lapply(coefs, \(x) any(!is.na(x))))))],
    covar_fns = list(
      age = \(x) age_covar_fn(x),
      sex = \(x) sex_covar_fn(x),
      educ = \(x) educ_covar_fn(x),
      race = \(x) race_covar_fn(x)
    )
  )

  coefs <- subset(
    reg_coefs[["updated_2025.06"]],
    var_name == "BOSTON"
  )[, setdiff(
    names(reg_coefs[["updated_2025.06"]]),
    "var_name"
  )]

  register_regression_version(
    scores = BOSTON(),
    version = "updated_2025.06",
    coefs = coefs[names(which(unlist(lapply(coefs, \(x) any(!is.na(x))))))],
    covar_fns = list(
      age = \(x) age_covar_fn(x),
      sex = \(x) sex_covar_fn(x),
      educ = \(x) educ_covar_fn(x),
      race = \(x) race_covar_fn(x)
    )
  )
  coefs <- subset(
    reg_coefs[["nacc_legacy"]],
    var_name == "BOSTON"
  )[, setdiff(
    names(reg_coefs[["nacc_legacy"]]),
    "var_name"
  )]

  coefs <- stats::setNames(
    as.numeric(coefs),
    names(coefs)
  )

  register_regression_version(
    scores = BOSTON(),
    version = "nacc_legacy",
    coefs = c(stats::na.omit(coefs)),
    covar_fns = list(
      age = \(x) age_covar_fn(x),
      sex = \(x) sex_covar_fn(x),
      educ = \(x) educ_covar_fn(x)
    )
  )
  ## Set the default for %s
  set_std_defaults(
    scores = BOSTON(),
    method = "regression",
    version = "nacc_legacy"
  )
}
