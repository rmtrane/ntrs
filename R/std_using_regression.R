#' Standardize using regression
#'
#' @description
#' A short description...
#'
#' @param test_scores A numeric vector of test scores.
#' @param age A numeric vector for age.
#' @param sex A character vector for sex.
#' @param educ A numeric vector for education level.
#' @param race A character vector for race.
#' @param version A single string or number indicating the version.
#' @param ... Arguments passed to the specific method.
#'
#' @returns
#' The result of the dispatched method for `std_using_regression`.
#'
#' @export
std_using_regression <- function(
  test_scores,
  age,
  sex,
  educ,
  version,
  ...
) {
  UseMethod("std_using_regression")
}

#' @export
std_using_regression.test_scores <- function(
  test_scores,
  version,
  ...
) {
  raw_scores <- as.numeric(test_scores)

  version_data <- get_version_data(
    test_scores,
    method = "regression",
    version = version
  )

  covars <- rlang::list2(...)

  covars_not_numeric <- sapply(covars, \(x) !is.numeric(x))

  if (any(covars_not_numeric)) {
    cli::cli_abort(
      "{.arg {names(covars_not_numeric[covars_not_numeric])}} must be {.cls numeric}."
    )
  }

  coefs <- version_data$coefs

  covars_needed <- setdiff(names(coefs), c("intercept", "rmse"))
  covars_missing <- setdiff(covars_needed, names(covars))

  if (length(covars_missing)) {
    cli::cli_abort(
      "{.arg {covars_missing}} are missing, but needed to standardize using {.val norms} when version is {.val {version}}."
    )
  }

  mismatched_length <- names(covars)[
    !sapply(covars, length) %in% c(1, length(raw_scores))
  ]

  if (length(mismatched_length) > 0) {
    cli::cli_abort(
      "{.arg {mismatched_length}} {?is/are} of length {lengths(covars[mismatched_length])}, but must be of length one or same as {.arg test_scores} ({.val {length(test_scores)}})"
    )
  }

  ## Apply covariate prep functions
  covariate_prep_funs <- version_data$covariate_prep_funs

  covars <- purrr::imap(covars, \(covar, covar_nm) {
    if (covar_nm %in% names(covariate_prep_funs)) {
      return(covariate_prep_funs[[covar_nm]](covar))
    }

    covar
  })

  ## Create X matrix
  X <- cbind(
    intercept = 1,
    data.frame(covars)
  )

  ## Get estimates
  (raw_scores - c(coefs[names(X)] %*% t(X))) / coefs[["rmse"]]
}
