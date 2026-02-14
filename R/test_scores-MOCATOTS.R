#' MOCATOTS Test Scores
#'
#' @description Create a `MOCATOTS` object to hold MoCA scores.
#'
#' @param scores Numeric scores.
#'
#' @returns
#' An object of class `MOCATOTS`.
#'
#' @export
MOCATOTS <- function(scores = numeric()) {
  ts <- test_scores(
    scores,
    label = "MoCA",
    range = c(0, 30),
    codes = c(
      "Not available: UDS form submitted did not collect data in this way, or a skip pattern precludes response to this question" = -4,
      "Item(s) or whole test not administered" = 88
    ),
    class = "MOCATOTS"
  )

  ts
}

# #' Standardize MOCATOTS scores using norms
# #'
# #'
# #' @param x A numeric vector of raw scores.
# #' @param age A numeric vector of ages.
# #' @param sex A vector indicating sex.
# #' @param educ A numeric vector of education levels.
# #' @param version A single string indicating the version of the norms to use.
# #'
# #' @returns
# #' A numeric vector of standardized scores.
# #'
# #' @export
# std_using_norms.MOCATOTS <- function(x, age, sex, educ, version) {
#   version_data <- get_version_data(
#     test_class = "MOCATOTS",
#     method = "norms",
#     version = version
#   )

#   NpsychBatteryNorms::std_scores_using_norms(
#     raw_scores = as.numeric(x),
#     var_name = "MOCATOTS",
#     age = age,
#     sex = sex,
#     education = educ,
#     m_sd = version_data$lookup_table,
#     age_group_fun = version_data$age_group_fun,
#     educ_group_fun = version_data$educ_group_fun,
#     sex_group_fun = version_data$sex_group_fun
#   )
# }

#' Standardize MOCATOTS scores using regression
#'
#'
#' @param test_scores A numeric vector of raw MOCATOTS scores of class `MOCATOTS`.
#' @param age A numeric vector of ages.
#' @param sex A character vector of sexes.
#' @param educ A numeric vector of education levels.
#' @param race A character vector of races.
#' @param version A single string indicating the version of the test.
#'
#' @returns
#' A numeric vector of standardized scores.
#'
#' @export
std_using_regression.MOCATOTS <- function(
  test_scores,
  age,
  sex,
  educ,
  race,
  version
) {
  version_data <- get_version_data(
    test_class = "MOCATOTS",
    method = "regression",
    version = version
  )

  if ("age_prep_fun" %in% names(version_data)) {
    age <- version_data$age_prep_fun(age)
  }

  if ("sex_prep_fun" %in% names(version_data)) {
    sex <- version_data$sex_prep_fun(sex)
  }

  if ("educ_prep_fun" %in% names(version_data)) {
    educ <- version_data$educ_prep_fun(age)
  }

  if ("race_prep_fun" %in% names(version_data)) {
    race <- version_data$race_prep_fun(race)
  }

  cur_coefs <- version_data$coefs

  if (inherits(cur_coefs, "data.frame")) {
    if (nrow(cur_coefs) > 1) {
      return(
        NpsychBatteryNorms::std_scores_using_multiple_regressions(
          raw_scores = remove_errorcodes(test_scores),
          coefs_to_use = cur_coefs,
          age = age,
          sex = sex,
          education = educ,
          race = race,
          delay = delay
        )
      )
    }
  }

  cur_coefs <- setNames(
    as.numeric(cur_coefs),
    names(cur_coefs)
  )

  NpsychBatteryNorms::std_scores_using_regression(
    raw_scores = remove_errorcodes(test_scores),
    var_name = "MOCATOTS",
    age = age,
    sex = sex,
    education = educ,
    race = race,
    delay = NULL,
    reg_coefs = na.omit(cur_coefs[-which(names(cur_coefs) == "rmse")]),
    sd = cur_coefs[["rmse"]]
  )
}

#' Setup MOCATOTS versions
#'
#' @description
#' Registers versions for the MOCATOTS test class,
#' and sets a default method. Meant to be called in .onLoad
#'
#' @returns
#' `NULL`, invisibly. This function is called for its side effects of
#' registering test versions and setting defaults.
#'
#' @keywords internal
.setup_MOCATOTS_versions <- function() {
  # Register norms versions for MOCATOTS
  lookup_table <- NpsychBatteryNorms::normative_summaries$nacc$MOCATOTS
  names(lookup_table)[which(
    names(lookup_table) %in% c("age_group", "educ_group")
  )] <- c("age", "educ")

  lookup_table$sex <- factor(lookup_table$sex, levels = c("m", "f"))

  register_norms_version(
    test_class = MOCATOTS(),
    version = "nacc",
    lookup_table = lookup_table,
    covariate_prep_funs = list(
      age = \(x) NpsychBatteryNorms::get_age_group(x, "nacc"),
      educ = \(x) NpsychBatteryNorms::get_educ_group(x),
      sex = \(x) factor(x, levels = c(1, 2), labels = c("m", "f"))
    )
  )

  lookup_table <- NpsychBatteryNorms::normative_summaries$updated$MOCATOTS
  names(lookup_table)[which(
    names(lookup_table) %in% c("age_group", "educ_group")
  )] <- c("age", "educ")

  lookup_table$sex <- factor(lookup_table$sex, levels = c("m", "f"))

  register_norms_version(
    test_class = MOCATOTS(),
    version = "updated",
    lookup_table = lookup_table,
    covariate_prep_funs = list(
      age = \(x) NpsychBatteryNorms::get_age_group(x, "nacc"),
      educ = \(x) NpsychBatteryNorms::get_educ_group(x),
      sex = \(x) factor(x, levels = c(1, 2), labels = c("m", "f"))
    )
  )

  # Register regression versions for MOCATOTS
  coefs <- subset(
    NpsychBatteryNorms::reg_coefs[["updated_2024.06"]],
    var_name == "MOCATOTS"
  )[, setdiff(
    names(NpsychBatteryNorms::reg_coefs[["updated_2024.06"]]),
    "var_name"
  )]

  names(coefs)[names(coefs) == "education"] <- "educ"

  register_regression_version(
    test_class = MOCATOTS(),
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
    var_name == "MOCATOTS"
  )[, setdiff(
    names(NpsychBatteryNorms::reg_coefs[["updated_2025.06"]]),
    "var_name"
  )]

  names(coefs)[names(coefs) == "education"] <- "educ"

  register_regression_version(
    test_class = MOCATOTS(),
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
    var_name == "MOCATOTS"
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
    test_class = MOCATOTS(),
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

  ## Set the default for MOCATOTS
  set_default_method(
    test_class = MOCATOTS(),
    method = "regression",
    version = "updated_2025.06"
  )
}
