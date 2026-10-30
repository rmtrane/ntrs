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
