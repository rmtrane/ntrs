#' CRAFTDRE Test Scores
#'
#' @description Create a `CRAFTDRE` object to hold CRAFTDRE scores.
#'
#' @param scores Numeric scores.
#'
#' @returns
#' An object of class `CRAFTDRE`.
#'
#' @export
CRAFTDRE <- function(scores = numeric()) {
  ts <- test_scores(
    scores,
    label = "Craft Delay - Paraphrase",
    range = c(0, 25),
    codes = c("Physical problem" = 95,
      "Cognitive/behavior problem" = 96,
      "Other problem" = 97,
      "Verbal refusal" = 98,
      "Not available: UDS form submitted did not collect data in this way, or a skip pattern precludes response to this question" = -4),
    class = "CRAFTDRE"
  )

  ts
  }

#' Setup CRAFTDRE method versions
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
.setup_CRAFTDRE_versions <- function() {
# Register norms versions for CRAFTDRE
  lookup_table <- NpsychBatteryNorms::normative_summaries$nacc$CRAFTDRE
  names(lookup_table)[which(
    names(lookup_table) %in% c("age_group", "edu_group", "educ_group")
  )] <- c("age", "educ")

  lookup_table$sex <- factor(lookup_table$sex, levels = c("m", "f"))

  register_norms_version(
    test_class = CRAFTDRE(),
    version = "nacc",
    lookup_table = lookup_table,
    covariate_prep_funs = list(
      age = \(x) NpsychBatteryNorms::get_age_group(x, "nacc"),
      educ = \(x) NpsychBatteryNorms::get_educ_group(x),
      sex = \(x) factor(x, levels = c(1, 2), labels = c("m", "f"))
    )
  )

  lookup_table <- NpsychBatteryNorms::normative_summaries$updated$CRAFTDRE
  names(lookup_table)[which(
    names(lookup_table) %in% c("age_group", "edu_group", "educ_group")
  )] <- c("age", "educ")

  lookup_table$sex <- factor(lookup_table$sex, levels = c("m", "f"))

  register_norms_version(
    test_class = CRAFTDRE(),
    version = "updated",
    lookup_table = lookup_table,
    covariate_prep_funs = list(
      age = \(x) NpsychBatteryNorms::get_age_group(x, "nacc"),
      educ = \(x) NpsychBatteryNorms::get_educ_group(x),
      sex = \(x) factor(x, levels = c(1, 2), labels = c("m", "f"))
    )
  )
# Register regression versions for CRAFTDRE
  coefs <- subset(
    NpsychBatteryNorms::reg_coefs[["updated_2024.06"]],
    var_name == "CRAFTDRE"
  )[, setdiff(
    names(NpsychBatteryNorms::reg_coefs[["updated_2024.06"]]),
    "var_name"
  )]

  names(coefs)[names(coefs) == "education"] <- "educ"

  register_regression_version(
    test_class = CRAFTDRE(),
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
    var_name == "CRAFTDRE"
  )[, setdiff(
    names(NpsychBatteryNorms::reg_coefs[["updated_2025.06"]]),
    "var_name"
  )]

  names(coefs)[names(coefs) == "education"] <- "educ"

  register_regression_version(
    test_class = CRAFTDRE(),
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
coefs <- subset(
    NpsychBatteryNorms::reg_coefs[["nacc"]],
    var_name == "CRAFTDRE"
  )[, setdiff(
    names(NpsychBatteryNorms::reg_coefs[["nacc"]]),
    "var_name"
  )]

  coefs <- setNames(
    as.numeric(coefs),
    names(coefs)
  )

  names(coefs)[names(coefs) == "education"] <- "educ"

  register_regression_version(
    test_class = CRAFTDRE(),
    version = "nacc",
    coefs = na.omit(coefs),
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
    test_class = CRAFTDRE(),
    method = "regression",
    version = "updated_2025.06"
  )
  }
