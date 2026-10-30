#' @include new_npsych_scores.R
NULL


#' WAIS Test Scores
#'
#' @description Create a `WAIS` object to hold WAIS scores.
#'
#' @param x Numeric scores.
#'
#' @returns
#' An object of class `WAIS`.
#'
#' @export
WAIS <- new_npsych_scores(
  "WAIS",
  label = "WAIS-R Digit Symbol",
  domain = "Attention/Processing",
  short_descriptor = "WAIS-R Digit Symbol",
  range = c(0, 93),
  codes = c(
    "Physical problem" = 95,
    "Cognitive/behavior problem" = 96,
    "Other problem" = 97,
    "Verbal refusal" = 98,
    "Not available: UDS form submitted" = -4
  )
)

#' Setup WAIS method versions
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
.setup_WAIS_versions <- function() {
  var_name <- NULL
  # Register regression versions for WAIS
  coefs <- subset(
    reg_coefs[["updated_2024.06"]],
    var_name == "WAIS"
  )[, setdiff(
    names(reg_coefs[["updated_2024.06"]]),
    "var_name"
  )]

  register_regression_version(
    scores = WAIS(),
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
    var_name == "WAIS"
  )[, setdiff(
    names(reg_coefs[["updated_2025.06"]]),
    "var_name"
  )]

  register_regression_version(
    scores = WAIS(),
    version = "updated_2025.06",
    coefs = coefs[names(which(unlist(lapply(coefs, \(x) any(!is.na(x))))))],
    covar_fns = list(
      age = \(x) age_covar_fn(x),
      sex = \(x) sex_covar_fn(x),
      educ = \(x) educ_covar_fn(x),
      race = \(x) race_covar_fn(x)
    )
  )
  ## Set the default for %s
  # set_std_defaults(
  #   scores = WAIS(),
  #   method = "T-score",
  #   version = "NA"
  # )
}
