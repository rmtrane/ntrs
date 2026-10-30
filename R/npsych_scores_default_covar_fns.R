#' Recode NACC `RACE` for regression-based standardization
#'
#' @description
#' Covariate function for the `race` term in the `updated_2024.06` and
#' `updated_2025.06` regression versions. Converts the NACC `RACE` code to the
#' indicator used when the models were fit: `0` for White, `1` for any other
#' reported race. Unknown (`99`) and missing (`NA`) values become `NA`, so
#' [lin_reg_resids()] falls back to the submodel without race for those
#' observations.
#'
#' Passing `race = NA` for every observation therefore standardizes without
#' adjusting for race.
#'
#' @param x A numeric vector of NACC `RACE` codes (`1` = White, `2` = Black or
#'   African American, `3` = American Indian or Alaska Native, `4` = Native
#'   Hawaiian or Other Pacific Islander, `5` = Asian, `50` = Other, `99` =
#'   Unknown).
#'
#' @returns A numeric vector the same length as `x`: `0` for White, `1` for any
#'   other known race, and `NA` for unknown or missing.
#'
#' @examples
#' \dontrun{
#' race_covar_fn(c(1, 2, 5, 50, 99, NA))
#' #> [1]  0  1  1  1 NA NA
#' }
#'
#' @keywords internal
race_covar_fn <- function(x) {
  as.numeric(
    data.table::fcase(
      x == 1             , "White" ,
      is.na(x) | x == 99 , NA      ,
      default = "Other"
    ) ==
      "Other"
  )
}

#' Prepare age for regression-based standardization
#'
#' @description
#' Covariate function for the `age` term in the regression versions. Limits
#' age to the range `0`--`110`: negative values become `0` and values above
#' `110` become `110`. Missing (`NA`) values stay missing, so
#' [lin_reg_resids()] falls back to the submodel without age for those
#' observations.
#'
#' @param x A numeric vector of ages in years (e.g. NACC `NACCAGE`).
#'
#' @returns A numeric vector the same length as `x`.
#'
#' @examples
#' \dontrun{
#' age_covar_fn(c(-1, 70, 120, NA))
#' #> [1]   0  70 110  NA
#' }
#'
#' @keywords internal
age_covar_fn <- function(x) {
  x[x < 0] <- 0
  x[x > 110] <- 110

  x
}

#' Prepare education for regression-based standardization
#'
#' @description
#' Covariate function for the `educ` term in the regression versions.
#' Unknown (`99`) and missing (`NA`) values become `NA`, so [lin_reg_resids()]
#' falls back to the submodel without education for those observations. Other
#' values are limited to the range `0`--`31`: negative values become `0` and
#' values above `31` become `31`.
#'
#' @param x A numeric vector of NACC `EDUC` codes: years of education (`0`--`36`)
#'   or `99` = Unknown.
#'
#' @returns A numeric vector the same length as `x`.
#'
#' @examples
#' \dontrun{
#' educ_covar_fn(c(-1, 16, 36, 99, NA))
#' #> [1]  0 16 31 NA NA
#' }
#'
#' @keywords internal
educ_covar_fn <- function(x) {
  x[x %in% 99] <- NA
  x[x < 0] <- 0
  x[x > 31] <- 31

  x
}

#' Recode NACC `SEX` for regression-based standardization
#'
#' @description
#' Covariate function for the `sex` term in the regression versions. Converts
#' the NACC `SEX` code to the indicator used when the models were fit: `0` for
#' male, `1` for female. Any other value (including `8`, `9` and `NA`) becomes
#' `NA`, so [lin_reg_resids()] falls back to the submodel without sex for those
#' observations.
#'
#' @param x A numeric vector of NACC `SEX` codes (`1` = Male, `2` = Female).
#'
#' @returns A numeric vector the same length as `x`: `0` for male, `1` for
#'   female, and `NA` otherwise.
#'
#' @examples
#' \dontrun{
#' sex_covar_fn(c(1, 2, 8, 9, NA))
#' #> [1]  0  1 NA NA NA
#' }
#'
#' @keywords internal
sex_covar_fn <- function(x) {
  x[!x %in% c(1, 2)] <- NA

  as.numeric(x == 2)
}
