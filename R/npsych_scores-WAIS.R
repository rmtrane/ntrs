#' WAIS Test Scores
#'
#' @description Create a `WAIS` object to hold WAIS scores.
#'
#' @param scores Numeric scores.
#'
#' @returns
#' An object of class `WAIS`.
#'
#' @export
WAIS <- function(scores = numeric()) {
  ts <- npsych_scores(
    scores,
    label = "WAIS-R Digit Symbol",
    range = c(0, 93),
    codes = c(
      "Physical problem" = 95,
      "Cognitive/behavior problem" = 96,
      "Other problem" = 97,
      "Verbal refusal" = 98,
      "Not available: UDS form submitted" = -4
    ),
    subclass = "WAIS"
  )

  ts
}

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
    NpsychBatteryNorms::reg_coefs[["updated_2024.06"]],
    var_name == "WAIS"
  )[, setdiff(
    names(NpsychBatteryNorms::reg_coefs[["updated_2024.06"]]),
    "var_name"
  )]

  names(coefs)[names(coefs) == "education"] <- "educ"

  register_regression_version(
    scores = WAIS(),
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
      },
      race = \(x) {
        as.numeric(
          my_fcase(
            x == 1,
            "White",
            x == 99,
            NA,
            default = "Other"
          ) ==
            "Other"
        )
      }
    )
  )

  coefs <- subset(
    NpsychBatteryNorms::reg_coefs[["updated_2025.06"]],
    var_name == "WAIS"
  )[, setdiff(
    names(NpsychBatteryNorms::reg_coefs[["updated_2025.06"]]),
    "var_name"
  )]

  names(coefs)[names(coefs) == "education"] <- "educ"

  register_regression_version(
    scores = WAIS(),
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
      },
      race = \(x) {
        as.numeric(
          my_fcase(
            x == 1,
            "White",
            x == 99,
            NA,
            default = "Other"
          ) ==
            "Other"
        )
      }
    )
  )
  ## Set the default for %s
  # set_std_defaults(
  #   scores = WAIS(),
  #   method = "T-score",
  #   version = "NA"
  # )
}
