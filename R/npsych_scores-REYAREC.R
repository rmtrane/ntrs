#' REYAREC Test Scores
#'
#' @description Create a `REYAREC` object to hold REYAREC scores.
#'
#' @param scores Numeric scores.
#'
#' @returns
#' An object of class `REYAREC`.
#'
#' @export
REYAREC <- function(scores = numeric()) {
  ts <- npsych_scores(
    scores,
    label = "RAVLT Recognition",
    range = c(0, 100),
    # codes = c("" = ),
    subclass = "REYAREC"
  )

  ts
}

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
    NpsychBatteryNorms::reg_coefs[["updated_2024.06"]],
    var_name == "REYAREC"
  )[, setdiff(
    names(NpsychBatteryNorms::reg_coefs[["updated_2024.06"]]),
    "var_name"
  )]

  names(coefs)[names(coefs) == "education"] <- "educ"

  register_regression_version(
    scores = REYAREC(),
    version = "updated_2024.06",
    coefs = coefs[names(which(sapply(coefs, \(x) !all(is.na(x)))))],
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
          data.table::fcase(
            x == 1  ,
            "White" ,
            x == 99 ,
            NA      ,
            default = "Other"
          ) ==
            "Other"
        )
      }
    )
  )

  coefs <- subset(
    NpsychBatteryNorms::reg_coefs[["updated_2025.06"]],
    var_name == "REYAREC"
  )[, setdiff(
    names(NpsychBatteryNorms::reg_coefs[["updated_2025.06"]]),
    "var_name"
  )]

  names(coefs)[names(coefs) == "education"] <- "educ"

  register_regression_version(
    scores = REYAREC(),
    version = "updated_2025.06",
    coefs = coefs[names(which(sapply(coefs, \(x) !all(is.na(x)))))],
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
  # set_std_defaults(
  #   scores = REYAREC(),
  #   method = "T-score",
  #   version = "NA"
  # )
}
