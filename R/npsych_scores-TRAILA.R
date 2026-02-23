#' TRAILA Test Scores
#'
#' @description Create a `TRAILA` object to hold TRAILA scores.
#'
#' @param scores Numeric scores.
#'
#' @returns
#' An object of class `TRAILA`.
#'
#' @export
TRAILA <- function(scores = numeric()) {
  ts <- npsych_scores(
    scores,
    label = "Trailmaking Part A",
    range = c(0, 150),
    codes = c(
      "Physical problem" = 995,
      "Cognitive/behavior problem" = 996,
      "Other problem" = 997,
      "Verbal refusal" = 998,
      "Not available: UDS form submitted did not collect data in this way, or a skip pattern precludes response to this question" = -4
    ),
    subclass = "TRAILA"
  )

  ts
}

#' Setup TRAILA method versions
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
.setup_TRAILA_versions <- function() {
  var_name <- NULL
  # Register norms versions for TRAILA
  lookup_table <- NpsychBatteryNorms::normative_summaries$nacc$TRAILA
  names(lookup_table)[which(
    names(lookup_table) %in% c("age_group", "edu_group", "educ_group")
  )] <- c("age", "educ")

  lookup_table$sex <- factor(lookup_table$sex, levels = c("m", "f"))

  register_norms_version(
    scores = TRAILA(),
    version = "nacc",
    lookup_table = lookup_table,
    covar_fns = list(
      age = \(x) NpsychBatteryNorms::get_age_group(x, "nacc"),
      educ = \(x) NpsychBatteryNorms::get_educ_group(x),
      sex = \(x) factor(x, levels = c(1, 2), labels = c("m", "f"))
    )
  )

  lookup_table <- NpsychBatteryNorms::normative_summaries$updated$TRAILA
  names(lookup_table)[which(
    names(lookup_table) %in% c("age_group", "edu_group", "educ_group")
  )] <- c("age", "educ")

  lookup_table$sex <- factor(lookup_table$sex, levels = c("m", "f"))

  register_norms_version(
    scores = TRAILA(),
    version = "updated",
    lookup_table = lookup_table,
    covar_fns = list(
      age = \(x) NpsychBatteryNorms::get_age_group(x, "nacc"),
      educ = \(x) NpsychBatteryNorms::get_educ_group(x),
      sex = \(x) factor(x, levels = c(1, 2), labels = c("m", "f"))
    )
  )
  # Register regression versions for TRAILA
  coefs <- subset(
    NpsychBatteryNorms::reg_coefs[["updated_2024.06"]],
    var_name == "TRAILA"
  )[, setdiff(
    names(NpsychBatteryNorms::reg_coefs[["updated_2024.06"]]),
    "var_name"
  )]

  names(coefs)[names(coefs) == "education"] <- "educ"

  register_regression_version(
    scores = TRAILA(),
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
    var_name == "TRAILA"
  )[, setdiff(
    names(NpsychBatteryNorms::reg_coefs[["updated_2025.06"]]),
    "var_name"
  )]

  names(coefs)[names(coefs) == "education"] <- "educ"

  register_regression_version(
    scores = TRAILA(),
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
  coefs <- subset(
    NpsychBatteryNorms::reg_coefs[["nacc"]],
    var_name == "TRAILA"
  )[, setdiff(
    names(NpsychBatteryNorms::reg_coefs[["nacc"]]),
    "var_name"
  )]

  coefs <- stats::setNames(
    as.numeric(coefs),
    names(coefs)
  )

  names(coefs)[names(coefs) == "education"] <- "educ"

  register_regression_version(
    scores = TRAILA(),
    version = "nacc",
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
    scores = TRAILA(),
    method = "regression",
    version = "updated_2025.06"
  )
}
