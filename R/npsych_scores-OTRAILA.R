#' OTRAILA Test Scores
#'
#' @description Create a `OTRAILA` object to hold OTRAILA scores.
#'
#' @param scores Numeric scores.
#'
#' @returns
#' An object of class `OTRAILA`.
#'
#' @export
OTRAILA <- function(scores = numeric()) {
  ts <- npsych_scores(
    scores,
    label = "Oral Trailmaking Part A - Completion Time",
    range = c(0, 100),
    codes = c(
      "Not assessed, optional" = 888,
      "Physical problem" = 995,
      "Cognitive/behavior problem" = 996,
      "Other problem" = 997,
      "Verbal refusal" = 998,
      "Not available: UDS form submitted did not collect data in this way, or a skip pattern precludes response to this question" = -4
    ),
    subclass = "OTRAILA"
  )

  ts
}

#' Setup OTRAILA method versions
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
.setup_OTRAILA_versions <- function() {
  var_name <- NULL
  lookup_table <- NpsychBatteryNorms::normative_summaries$updated$OTRAILA
  names(lookup_table)[which(
    names(lookup_table) %in% c("age_group")
  )] <- c("age")

  lookup_table$sex <- factor(lookup_table$sex, levels = c("m", "f"))

  register_norms_version(
    scores = OTRAILA(),
    version = "updated",
    lookup_table = lookup_table,
    covar_fns = list(
      age = \(x) NpsychBatteryNorms::get_age_group(x, "nacc"),
      sex = \(x) factor(x, levels = c(1, 2), labels = c("m", "f"))
    )
  )

  # Register regression versions for OTRAILA
  coefs <- subset(
    NpsychBatteryNorms::reg_coefs[["updated_2024.06"]],
    var_name == "OTRAILA"
  )[, setdiff(
    names(NpsychBatteryNorms::reg_coefs[["updated_2024.06"]]),
    "var_name"
  )]

  names(coefs)[names(coefs) == "education"] <- "educ"

  register_regression_version(
    scores = OTRAILA(),
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
    var_name == "OTRAILA"
  )[, setdiff(
    names(NpsychBatteryNorms::reg_coefs[["updated_2025.06"]]),
    "var_name"
  )]

  names(coefs)[names(coefs) == "education"] <- "educ"

  register_regression_version(
    scores = OTRAILA(),
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
  ## Set the default for %s
  set_std_defaults(
    scores = OTRAILA(),
    method = "regression",
    version = "updated_2025.06"
  )
}
