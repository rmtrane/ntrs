#' @include new_npsych_scores.R
NULL

#' LOGIMEM Test Scores
#'
#' @description Create a `LOGIMEM` object to hold LOGIMEM scores.
#'
#' @param x Numeric scores.
#'
#' @returns
#' An object of class `LOGIMEM`.
#'
#' @export
LOGIMEM <- new_npsych_scores(
  "LOGIMEM",
  label = "Logical Memory, Immediate",
  domain = "Memory",
  short_descriptor = "Total number of story units recalled from this current test administration",
  range = c(0, 25),
  codes = c(
    "Physical problem" = 95,
    "Cognitive/behavior problem" = 96,
    "Other problem" = 97,
    "Verbal refusal" = 98,
    "Not available: UDS form submitted" = -4
  )
)

#' Setup LOGIMEM method versions
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
.setup_LOGIMEM_versions <- function() {
  var_name <- NULL
  # Register regression versions for LOGIMEM
  coefs <- subset(
    reg_coefs[["updated_2024.06"]],
    var_name == "LOGIMEM"
  )[, setdiff(
    names(reg_coefs[["updated_2024.06"]]),
    "var_name"
  )]

  register_regression_version(
    scores = LOGIMEM(),
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
    var_name == "LOGIMEM"
  )[, setdiff(
    names(reg_coefs[["updated_2025.06"]]),
    "var_name"
  )]

  register_regression_version(
    scores = LOGIMEM(),
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
    var_name == "LOGIMEM"
  )[, setdiff(
    names(reg_coefs[["nacc_legacy"]]),
    "var_name"
  )]

  coefs <- stats::setNames(
    as.numeric(coefs),
    names(coefs)
  )

  register_regression_version(
    scores = LOGIMEM(),
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
    scores = LOGIMEM(),
    method = "regression",
    version = "nacc_legacy"
  )
}
