#' DIGIBLEN Test Scores
#'
#' @description Create a `DIGIBLEN` object to hold DIGIBLEN scores.
#'
#' @param scores Numeric scores.
#'
#' @returns
#' An object of class `DIGIBLEN`.
#'
#' @export
DIGIBLEN <- function(scores = numeric()) {
  ts <- npsych_scores(
    scores,
    label = "Digit Span Backward - Span Length",
    range = c(0, 8),
    codes = c(
      "Physical problem" = 95,
      "Cognitive/behavior problem" = 96,
      "Other problem" = 97,
      "Verbal refusal" = 98,
      "Not available: UDS form submitted" = -4
    ),
    subclass = "DIGIBLEN"
  )

  ts
}

#' Setup DIGIBLEN method versions
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
.setup_DIGIBLEN_versions <- function() {
  var_name <- NULL
  # Register regression versions for DIGIBLEN
  coefs <- subset(
    NpsychBatteryNorms::reg_coefs[["updated_2024.06"]],
    var_name == "DIGIBLEN"
  )[, setdiff(
    names(NpsychBatteryNorms::reg_coefs[["updated_2024.06"]]),
    "var_name"
  )]

  names(coefs)[names(coefs) == "education"] <- "educ"

  register_regression_version(
    scores = DIGIBLEN(),
    version = "updated_2024.06",
    coefs = coefs[names(which(unlist(lapply(coefs, \(x) any(!is.na(x))))))],
    covar_fns = list(
      age = \(x) {
        x[x < 0] <- 0
        x[x > 110] <- 110

        x
      },
      sex = \(x) {
        as.numeric(x == 2)
      },
      educ = \(x) {
        x[x < 0] <- 0
        x[x > 31] <- 31

        x
      }
    )
  )

  coefs <- subset(
    NpsychBatteryNorms::reg_coefs[["updated_2025.06"]],
    var_name == "DIGIBLEN"
  )[, setdiff(
    names(NpsychBatteryNorms::reg_coefs[["updated_2025.06"]]),
    "var_name"
  )]

  names(coefs)[names(coefs) == "education"] <- "educ"

  register_regression_version(
    scores = DIGIBLEN(),
    version = "updated_2025.06",
    coefs = coefs[names(which(unlist(lapply(coefs, \(x) any(!is.na(x))))))],
    covar_fns = list(
      age = \(x) {
        x[x < 0] <- 0
        x[x > 110] <- 110

        x
      },
      sex = \(x) {
        as.numeric(x == 2)
      },
      educ = \(x) {
        x[x < 0] <- 0
        x[x > 31] <- 31

        x
      }
    )
  )
  coefs <- subset(
    NpsychBatteryNorms::reg_coefs[["nacc_legacy"]],
    var_name == "DIGIBLEN"
  )[, setdiff(
    names(NpsychBatteryNorms::reg_coefs[["nacc_legacy"]]),
    "var_name"
  )]

  coefs <- stats::setNames(
    as.numeric(coefs),
    names(coefs)
  )

  names(coefs)[names(coefs) == "education"] <- "educ"

  register_regression_version(
    scores = DIGIBLEN(),
    version = "nacc_legacy",
    coefs = stats::na.omit(coefs),
    covar_fns = list(
      age = \(x) {
        x[x < 0] <- 0
        x[x > 110] <- 110

        x
      },
      sex = \(x) {
        as.numeric(x == 2)
      },
      educ = \(x) {
        x[x < 0] <- 0
        x[x > 31] <- 31

        x
      }
    )
  )
  ## Set the default for %s
  set_std_defaults(
    scores = DIGIBLEN(),
    method = "regression",
    version = "nacc_legacy"
  )
}
