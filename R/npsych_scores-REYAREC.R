#' @include new_npsych_scores.R
NULL

#' REYAREC Test Scores
#'
#' @description Create a `REYAREC` object to hold REYAREC scores.
#'
#' @param x Numeric scores.
#'
#' @returns
#' An object of class `REYAREC`.
#'
#' @export
REYAREC <- new_npsych_scores(
  "REYAREC",
  label = "RAVLT Recognition",
  domain = "Memory",
  short_descriptor = "RAVLT Recognition Percentage (not in NACC data)",
  range = c(0, 100)
)

#' Setup REYAREC method versions
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
.setup_REYAREC_versions <- function() {
  var_name <- NULL
  # Register regression versions for REYAREC
  coefs <- subset(
    reg_coefs[["updated_2024.06"]],
    var_name == "REYAREC"
  )[, setdiff(
    names(reg_coefs[["updated_2024.06"]]),
    "var_name"
  )]

  register_regression_version(
    scores = REYAREC(),
    version = "updated_2024.06",
    coefs = coefs[names(which(sapply(coefs, \(x) !all(is.na(x)))))],
    covar_fns = list(
      age = \(x) age_covar_fn(x),
      sex = \(x) sex_covar_fn(x),
      educ = \(x) educ_covar_fn(x),
      race = \(x) race_covar_fn(x)
    )
  )

  coefs <- subset(
    reg_coefs[["updated_2025.06"]],
    var_name == "REYAREC"
  )[, setdiff(
    names(reg_coefs[["updated_2025.06"]]),
    "var_name"
  )]

  register_regression_version(
    scores = REYAREC(),
    version = "updated_2025.06",
    coefs = coefs[names(which(sapply(coefs, \(x) !all(is.na(x)))))],
    covar_fns = list(
      age = \(x) age_covar_fn(x),
      sex = \(x) sex_covar_fn(x),
      educ = \(x) educ_covar_fn(x),
      race = \(x) race_covar_fn(x)
    )
  )
  ## Set the default for %s
  # set_std_defaults(
  #   scores = REYAREC(),
  #   method = "T-score",
  #   version = "NA"
  # )
}
