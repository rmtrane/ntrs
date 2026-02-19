#' Standardize using regression
#'
#' @description
#' A short description...
#'
#' @param test_scores A numeric vector of test scores.
#' @param ... Arguments passed to the specific method.
#'
#' @returns
#' The result of the dispatched method for `std_using_regression`.
#'
#' @export
std_using_regression <- function(
  test_scores,
  ...
) {
  UseMethod("std_using_regression")
}

#' Standardize test scores using regression
#'
#' @description
#' A short description...
#'
#' @param test_scores A vector coercible to numeric, representing raw test scores.
#' @param ... Additional numeric covariates to be included in the regression model. Each covariate must be a numeric vector of length 1 or the same length as `test_scores`,
#'   and must be named after a coefficient present in the `version` data; see `get_version_data({test_scores}, "regression", {version})$coefs`.
#' @param version A single string specifying the version of the regression model to use for standardization.
#'
#' @returns
#' A numeric vector of standardized test scores. The function will error if any supplied covariates are not numeric, if required covariates for the specified `version` are missing, or if covariate lengths are mismatched.
#'
#' @export
std_using_regression.test_scores <- function(
  test_scores,
  ...,
  version
) {
  raw_scores <- as.numeric(test_scores)

  version_data <- get_version_data(
    test_scores,
    method = "regression",
    version = version
  )

  covars <- rlang::list2(...)

  # Check that all dots are named
  if (any(names(covars) == "")) {
    cli::cli_abort(
      "All additional arguments in ... must be named."
    )
  }

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
