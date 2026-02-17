#' REYTOTAL Test Scores
#'
#' @description Create a `REYTOTAL` object to hold REYTOTAL scores.
#'
#' @param scores Numeric scores.
#'
#' @returns
#' An object of class `REYTOTAL`.
#'
#' @export
REYTOTAL <- function(scores = numeric()) {
  ts <- test_scores(
    scores,
    label = "RAVLT Total Learning",
    range = c(0, 75),
    # codes = c("" = ),
    class = "REYTOTAL"
  )

  ts
}

#' Setup REYTOTAL method versions
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
.setup_REYTOTAL_versions <- function() {
  # Register regression versions for REYTOTAL
  coefs <- subset(
    NpsychBatteryNorms::reg_coefs[["updated_2024.06"]],
    var_name == "REYTOTAL"
  )[, setdiff(
    names(NpsychBatteryNorms::reg_coefs[["updated_2024.06"]]),
    "var_name"
  )]

  names(coefs)[names(coefs) == "education"] <- "educ"

  register_regression_version(
    test_class = REYTOTAL(),
    version = "updated_2024.06",
    coefs = coefs[, -which(names(coefs) == "delay")],
    covariate_prep_funs = list(
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
    var_name == "REYTOTAL"
  )[, setdiff(
    names(NpsychBatteryNorms::reg_coefs[["updated_2025.06"]]),
    "var_name"
  )]

  names(coefs)[names(coefs) == "education"] <- "educ"

  register_regression_version(
    test_class = REYTOTAL(),
    version = "updated_2025.06",
    coefs = coefs[, -which(names(coefs) == "delay")],
    covariate_prep_funs = list(
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
  set_default_method(
    test_class = REYTOTAL(),
    method = "T-score",
    version = "NA"
  )
}
