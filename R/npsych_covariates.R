#' @include new_npsych_covariate.R
NULL

# Valid values and codes below follow the entries for each variable in the
# Researchers Data Dictionary (see `rdd`).

#' NACCAGE Covariate
#'
#' @description Create a `NACCAGE` object to hold age at visit, in years.
#'   Valid values are 18 to 120.
#'
#' @param x Numeric ages.
#'
#' @returns An object of class `NACCAGE`.
#'
#' @examples
#' NACCAGE(c(65, 72.5, NA))
#'
#' @export
NACCAGE <- new_npsych_covariate(
  "NACCAGE",
  label = "Age",
  short_descriptor = "Subject's age at visit",
  range = c(18, 120)
)

#' SEX Covariate
#'
#' @description Create a `SEX` object to hold NACC `SEX` codes: `1` = Male,
#'   `2` = Female. The codes `8` (Prefer not to answer) and `9` (Don't know)
#'   are accepted and become `NA` in [valid_values()].
#'
#' @param x Numeric `SEX` codes.
#'
#' @returns An object of class `SEX`.
#'
#' @examples
#' SEX(c(1, 2, 9))
#'
#' @export
SEX <- new_npsych_covariate(
  "SEX",
  label = "Sex",
  short_descriptor = "Subject's sex",
  values = c("Male" = 1, "Female" = 2),
  codes = c("Prefer not to answer" = 8, "Don't know" = 9)
)

#' EDUC Covariate
#'
#' @description Create an `EDUC` object to hold years of education. Valid
#'   values are 0 to 36; the code `99` (Unknown) is accepted and becomes `NA`
#'   in [valid_values()].
#'
#' @param x Numeric years of education.
#'
#' @returns An object of class `EDUC`.
#'
#' @examples
#' EDUC(c(12, 16, 99))
#'
#' @export
EDUC <- new_npsych_covariate(
  "EDUC",
  label = "Education",
  short_descriptor = "Years of education",
  range = c(0, 36),
  codes = c("Unknown" = 99)
)

#' RACE Covariate
#'
#' @description Create a `RACE` object to hold UDS-3 `RACE` codes: `1` =
#'   White, `2` = Black or African American, `3` = American Indian or Alaska
#'   Native, `4` = Native Hawaiian or Other Pacific Islander, `5` = Asian,
#'   `50` = Other. The code `99` (Unknown) is accepted and becomes `NA` in
#'   [valid_values()].
#'
#' @param x Numeric UDS-3 `RACE` codes.
#'
#' @returns An object of class `RACE`.
#'
#' @examples
#' RACE(c(1, 2, 50, 99))
#'
#' @export
RACE <- new_npsych_covariate(
  "RACE",
  label = "Race",
  short_descriptor = "Race",
  values = c(
    "White" = 1,
    "Black or African American" = 2,
    "American Indian or Alaska Native" = 3,
    "Native Hawaiian or Other Pacific Islander" = 4,
    "Asian" = 5,
    "Other" = 50
  ),
  codes = c("Unknown" = 99)
)

#' MEMTIME Covariate
#'
#' @description Create a `MEMTIME` object to hold the time, in minutes,
#'   between Logical Memory IA (immediate) and IIA (delayed). Valid values are
#'   0 to 85; the codes `99` (Unknown) and `-4` (Not available) are accepted
#'   and become `NA` in [valid_values()].
#'
#' @param x Numeric minutes.
#'
#' @returns An object of class `MEMTIME`.
#'
#' @examples
#' MEMTIME(c(20, 30, 99))
#'
#' @export
MEMTIME <- new_npsych_covariate(
  "MEMTIME",
  label = "Logical Memory delay",
  short_descriptor = "Logical Memory IIA - Delayed - Time elapsed since Logical Memory IA - Immediate",
  range = c(0, 85),
  codes = c(
    "Unknown" = 99,
    "Not available: UDS form submitted did not collect data in this way, or a skip pattern precludes response to this question" = -4
  )
)
