#' UDSBENTC Test Scores
#'
#' @description Create a `UDSBENTC` object to hold UDSBENTC scores.
#'
#' @param scores Numeric scores.
#'
#' @returns
#' An object of class `UDSBENTC`.
#'
#' @export
UDSBENTC <- function(scores = numeric()) {
  ts <- npsych_scores(
    scores,
    label = "Benson Figure Copy",
    range = c(0, 17),
    codes = c(
      "Physical problem" = 95,
      "Cognitive/behavior problem" = 96,
      "Other problem" = 97,
      "Verbal refusal" = 98,
      "Not available: UDS form submitted did not collect data in this way, or a skip pattern precludes response to this question" = -4
    ),
    subclass = "UDSBENTC"
  )

  ts
}

#' Setup UDSBENTC method versions
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
.setup_UDSBENTC_versions <- function() {
  var_name <- NULL
  # Register norms versions for UDSBENTC
  lookup_table <- NpsychBatteryNorms::normative_summaries$nacc$UDSBENTC
  names(lookup_table)[which(
    names(lookup_table) %in% c("age_group", "edu_group", "educ_group")
  )] <- c("age", "educ")

  lookup_table$sex <- factor(lookup_table$sex, levels = c("m", "f"))

  register_norms_version(
    scores = UDSBENTC(),
    version = "nacc",
    lookup_table = lookup_table,
    covar_fns = list(
      age = \(x) NpsychBatteryNorms::get_age_group(x, "nacc"),
      educ = \(x) NpsychBatteryNorms::get_educ_group(x),
      sex = \(x) factor(x, levels = c(1, 2), labels = c("m", "f"))
    )
  )

  lookup_table <- NpsychBatteryNorms::normative_summaries$updated$UDSBENTC
  names(lookup_table)[which(
    names(lookup_table) %in% c("age_group", "edu_group", "educ_group")
  )] <- c("age", "educ")

  lookup_table$sex <- factor(lookup_table$sex, levels = c("m", "f"))

  register_norms_version(
    scores = UDSBENTC(),
    version = "updated",
    lookup_table = lookup_table,
    covar_fns = list(
      age = \(x) NpsychBatteryNorms::get_age_group(x, "nacc"),
      educ = \(x) NpsychBatteryNorms::get_educ_group(x),
      sex = \(x) factor(x, levels = c(1, 2), labels = c("m", "f"))
    )
  )
  # Register regression versions for UDSBENTC
  coefs <- subset(
    NpsychBatteryNorms::reg_coefs[["updated_2024.06"]],
    var_name == "UDSBENTC"
  )[, setdiff(
    names(NpsychBatteryNorms::reg_coefs[["updated_2024.06"]]),
    "var_name"
  )]

  names(coefs)[names(coefs) == "education"] <- "educ"

  register_regression_version(
    scores = UDSBENTC(),
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
    var_name == "UDSBENTC"
  )[, setdiff(
    names(NpsychBatteryNorms::reg_coefs[["updated_2025.06"]]),
    "var_name"
  )]

  names(coefs)[names(coefs) == "education"] <- "educ"

  register_regression_version(
    scores = UDSBENTC(),
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
    var_name == "UDSBENTC"
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
    scores = UDSBENTC(),
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
    scores = UDSBENTC(),
    method = "regression",
    version = "updated_2025.06"
  )
}
