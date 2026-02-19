#' REYDREC Test Scores
#'
#' @description Create a `REYDREC` object to hold REYDREC scores.
#'
#' @param scores Numeric scores.
#'
#' @returns
#' An object of class `REYDREC`.
#'
#' @export
REYDREC <- function(scores = numeric()) {
  ts <- npsych_scores(
    scores,
    label = "RAVLT Long Delay",
    range = c(0, 15),
    codes = c(
      "Not assessed, optional" = 88,
      "Physical problem" = 95,
      "Cognitive/behavior problem" = 96,
      "Other problem" = 97,
      "Verbal refusal" = 98,
      "Not available: UDS form submitted did not collect data in this way, or a skip pattern precludes response to this question" = -4
    ),
    subclass = "REYDREC"
  )

  ts
}

#' Setup REYDREC method versions
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
.setup_REYDREC_versions <- function() {
  var_name <- NULL
  # Register norms versions for REYDREC
  lookup_table <- NpsychBatteryNorms::normative_summaries$ravlt_trials$REYDREC
  names(lookup_table)[which(
    names(lookup_table) %in% c("age_group")
  )] <- c("age")

  register_norms_version(
    scores = REYDREC(),
    version = "ravlt_trials",
    lookup_table = lookup_table,
    covar_fns = list(
      age = \(x) NpsychBatteryNorms::get_age_group(x, "ravlt_trials")
    )
  )

  # Register regression versions for REYDREC
  coefs <- subset(
    NpsychBatteryNorms::reg_coefs[["updated_2024.06"]],
    var_name == "REYDREC"
  )[, setdiff(
    names(NpsychBatteryNorms::reg_coefs[["updated_2024.06"]]),
    "var_name"
  )]

  names(coefs)[names(coefs) == "education"] <- "educ"

  register_regression_version(
    scores = REYDREC(),
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
      }
    )
  )

  coefs <- subset(
    NpsychBatteryNorms::reg_coefs[["updated_2025.06"]],
    var_name == "REYDREC"
  )[, setdiff(
    names(NpsychBatteryNorms::reg_coefs[["updated_2025.06"]]),
    "var_name"
  )]

  names(coefs)[names(coefs) == "education"] <- "educ"

  register_regression_version(
    scores = REYDREC(),
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
  #   scores = REYDREC(),
  #   method = "T-score",
  #   version = "NA"
  # )
}
