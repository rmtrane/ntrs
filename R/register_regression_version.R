#' Register a regression-based standardization version
#'
#' @param scores A `npsych_scores` object, such as `MOCATOTS()`.
#' @param version Character string identifying this version.
#' @param coefs A named numeric vector or data frame of regression coefficients.
#'   Must include `rmse`. Allowed names are `intercept`, `age`, `sex`, `educ`,
#'   `race`, `delay`, and `rmse`. If a data frame, no column may consist
#'   entirely of missing values.
#' @param covar_fns Optional named list of functions that prepare
#'   covariates. Names must be a subset of `names(coefs)`.
#' @param description Optional character string describing this version.
#' @param overwrite Logical indicating whether to overwrite an existing version with the same name. Defaults to `FALSE`.
#'
#' @return Called for its side effect of registering the version in the
#'   internal `.std_versions` registry. Returns `invisible()`.
#'
#' @export
register_regression_version <- function(
  scores,
  version,
  coefs,
  covar_fns,
  description = "",
  overwrite = FALSE
) {
  UseMethod("register_regression_version")
}

#' @export
register_regression_version.npsych_scores <- function(
  scores,
  version,
  coefs,
  covar_fns,
  description = "",
  overwrite = FALSE
) {
  # Validate common parameters
  .validate_registration_params(version, description)

  # Validate regression-specific requirements
  valid_coef_classes <- c("numeric", "data.frame")

  if (!inherits(coefs, valid_coef_classes)) {
    cli::cli_abort(c(
      "{.arg coefs} must be a {.cls numeric} or {.cls data.frame}",
      "x" = "You supplied {.cls {class(coefs)}}",
      "i" = "Accepted classes: {.cls {valid_coef_classes}}"
    ))
  }

  if (is.numeric(coefs) && any(is.na(coefs))) {
    cli::cli_abort(c(
      "{.arg coefs} cannot contain missing values when a numeric vector is provided."
    ))
  }

  ## If coefs is a data.frame, check that no column consists of only missing values
  col_missing <- names(which(sapply(coefs, \(x) sum(!is.na(x)) == 0)))

  if (inherits(coefs, "data.frame") && length(col_missing) > 0) {
    cli::cli_abort(c(
      "{.arg coefs} cannot have columns with only missing values. {.field {col_missing}} {?has/have} only missing values."
    ))
  }

  # Check that coefs are named
  if (is.null(names(coefs))) {
    cli::cli_abort("{.arg coefs} must be named.")
  }

  # Make sure rmse is included
  if (!"rmse" %in% names(coefs)) {
    cli::cli_abort("{.arg coefs} must include {.field rmse}.")
  }

  # Make sure all coefficients are allowed
  allowed_coefs <- c(
    "intercept",
    "age",
    "sex",
    "educ",
    "race",
    "delay",
    "rmse"
  )

  if (!all(names(coefs) %in% allowed_coefs)) {
    cli::cli_abort(
      "{.arg coefs} must only contain a subset of {.field {allowed_coefs}}, but includes {.field {names(coefs)}}."
    )
  }

  # if (!methods::missingArg(covar_fns)) {
  if (!rlang::is_missing(covar_fns)) {
    if (!is.list(covar_fns)) {
      cli::cli_abort(
        "{.arg covar_fns} must be a {.cls list}, but is a {.cls {class(covar_fns)}}."
      )
    }

    mismatch_funs <- setdiff(names(covar_fns), names(coefs))

    if (length(mismatch_funs) > 0) {
      cli::cli_abort(
        "{.arg names(covar_fns)} must be a subset of {.code names(coefs)}. {.val {mismatch_funs}} not found in {.code names(coefs)}."
      )
    }
  }

  # Register
  .register_std_version(
    scores = scores,
    method = "regression",
    version = version,
    data = c(
      list(coefs = coefs),
      # if (!methods::missingArg(covar_fns)) {
      if (!rlang::is_missing(covar_fns)) {
        list(covar_fns = covar_fns)
      }
    ),
    description = description,
    overwrite = overwrite
  )

  cli::cli_inform(
    c(
      "v" = "Registered {.field regression} version {.val {version}} for {.cls {class(scores)[1]}}"
    ),
    class = "packageStartupMessage"
  )

  invisible()
}
