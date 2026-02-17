#' REYDLIST Test Scores
#'
#' @description Create a `REYDLIST` object to hold REYDLIST scores.
#'
#' @param scores Numeric scores.
#'
#' @returns
#' An object of class `REYDLIST`.
#'
#' @export
REYDLIST <- function(scores = numeric()) {
  ts <- test_scores(
    scores,
    label = "RAVLT Distractor List",
    range = c(0, 15),
    # codes = c("" = ),
    class = "REYDLIST"
  )

  ts
}

#' Setup REYDLIST method versions
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
.setup_REYDLIST_versions <- function() {
  # Register regression versions for REYDLIST
  coefs <- subset(
    NpsychBatteryNorms::reg_coefs[["updated_2024.06"]],
    var_name == "REYDLIST"
  )[, setdiff(
    names(NpsychBatteryNorms::reg_coefs[["updated_2024.06"]]),
    "var_name"
  )]

  names(coefs)[names(coefs) == "education"] <- "educ"

  register_regression_version(
    test_class = REYDLIST(),
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
    var_name == "REYDLIST"
  )[, setdiff(
    names(NpsychBatteryNorms::reg_coefs[["updated_2025.06"]]),
    "var_name"
  )]

  names(coefs)[names(coefs) == "education"] <- "educ"

  register_regression_version(
    test_class = REYDLIST(),
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
    test_class = REYDLIST(),
    method = "T-score",
    version = "NA"
  )
}
