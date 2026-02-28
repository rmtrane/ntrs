#' Standardize using regression
#'
#' @description
#' A short description...
#'
#' @param scores A numeric vector of test scores.
#' @param ... Arguments passed to the specific method.
#'
#' @returns
#' The result of the dispatched method for `std_using_regression`.
#'
#' @export
std_using_regression <- S7::new_generic("std_using_regression", "scores")

#' Standardize test scores using regression
#'
#' @description
#' A short description...
#'
#' @param scores A vector coercible to numeric, representing raw test scores.
#' @param ... Additional numeric covariates to be included in the regression model. Each covariate must be a numeric vector of length 1 or the same length as `npsych_scores`,
#'   and must be named after a coefficient present in the `version` data; use `get_version_data({npsych_scores}, "regression", {version})$coefs` to inspect the coefficients.
#' @param version A single string specifying the version of the regression model to use for standardization.
#'
#' @returns
#' A numeric vector of standardized test scores. The function will error if any supplied covariates are not numeric, if required covariates for the specified `version` are missing, or if covariate lengths are mismatched.
#'
#' @export
S7::method(std_using_regression, npsych_scores) <- function(
  scores,
  ...,
  version
) {
  raw_scores <- remove_error_codes(scores)

  version_obj <- get_version_data(
    scores,
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

  coefs <- version_obj@coefs

  covars_needed <- setdiff(names(coefs), c("intercept", "rmse"))
  covars_missing <- setdiff(covars_needed, names(covars))

  if (length(covars_missing)) {
    cli::cli_abort(
      "{.arg {covars_missing}} are missing, but needed to standardize using {.val regression} when version is {.val {version}}."
    )
  }

  mismatched_length <- names(covars)[
    !sapply(covars, length) %in% c(1, length(raw_scores))
  ]

  if (length(mismatched_length) > 0) {
    cli::cli_abort(
      "{.arg {mismatched_length}} {?is/are} of length {lengths(covars[mismatched_length])}, but must be of length one or same as {.arg npsych_scores} ({.val {length(npsych_scores)}})"
    )
  }

  ## Apply covariate prep functions
  covar_fns <- version_obj@covar_fns

  covars <- sapply(
    names(covars),
    \(covar_nm) {
      covar <- covars[[covar_nm]]

      if (covar_nm %in% names(covar_fns)) {
        return(covar_fns[[covar_nm]](covar))
      }

      covar
    },
    simplify = FALSE,
    USE.NAMES = TRUE
  )

  ## Create X matrix
  X <- cbind(
    intercept = 1,
    data.frame(covars)
  )

  ## Get std scores
  lin_reg_resids(raw_scores, X, coefs)
}


#' Compute scaled residuals from linear submodels matched by missingness
#'
#' For each observation, identifies the appropriate linear submodel based on
#' which covariates are available (non-NA), computes the predicted value using
#' that model's coefficients, and returns `(raw_scores - predicted) / rmse`.
#' Submodel matching uses binary hashing of missingness patterns for efficiency.
#'
#' @param raw_scores A numeric vector of observed scores, same length as
#'   `nrow(X)`. Observations with `NA` raw scores are skipped and return `NA`.
#' @param X A matrix or data.frame of covariates. An intercept column is added
#'   automatically if not already present. Columns that are entirely NA are
#'   dropped before matching.
#' @param coefs_to_use A named vector (single model) or data.frame/matrix
#'   (multiple models) of coefficients. Must include a column/element named
#'   `rmse`. For multiple models, each row represents a submodel, with `NA`
#'   values indicating excluded covariates. Each row must have a unique
#'   missingness pattern.
#'
#' @return A numeric vector of length `nrow(X)` containing scaled residuals.
#'   Returns `NA` for observations where `raw_scores` is `NA` or where no
#'   matching submodel exists.
#'
#' @keywords internal
#' @noRd
lin_reg_resids <- function(raw_scores, X, coefs_to_use) {
  if (is.null(names(coefs_to_use))) {
    cli::cli_abort("{.arg coefs_to_use} must be named.")
  }

  if (!"rmse" %in% names(coefs_to_use)) {
    cli::cli_abort("{.arg coefs_to_use} must contain {.val rmse}.")
  }

  if (!"intercept" %in% colnames(X)) {
    X <- cbind(intercept = 1, X)
  }

  # Allow users to pass a single model as a named vector
  if (is.vector(coefs_to_use) && !is.list(coefs_to_use)) {
    coefs_to_use <- as.data.frame(t(coefs_to_use))
  }

  n <- nrow(X)
  valid <- !is.na(raw_scores)

  # Rows with missing raw_scores will be NA regardless of prediction,
  # so skip them to avoid unnecessary computation
  if (!any(valid)) {
    return(rep(NA_real_, n))
  }

  X <- X[valid, , drop = FALSE]
  raw_scores <- raw_scores[valid]

  # Entirely-NA columns can't contribute to any prediction, and removing
  # them reduces the hash space and matrix dimensions downstream
  non_empty <- colSums(!is.na(X)) > 0
  X <- X[, non_empty, drop = FALSE]

  # Keep only submodels that don't use the dropped columns — a model
  # with a non-NA coefficient for a dropped column can't be valid
  dropped <- names(non_empty[!non_empty])
  dropped <- dropped[dropped %in% names(coefs_to_use)]
  if (length(dropped) > 0) {
    drop_mat <- as.matrix(coefs_to_use[, dropped, drop = FALSE])
    coefs_to_use <- coefs_to_use[apply(is.na(drop_mat), 1, all), , drop = FALSE]
    coefs_to_use[, dropped] <- NULL
  }

  rmses <- coefs_to_use[["rmse"]]
  coefs_to_use[["rmse"]] <- NULL

  cov_cols <- intersect(names(coefs_to_use), colnames(X))

  # Binary hashing uses 2^k weights, which lose integer precision
  # in double-precision floats beyond 2^53
  if (length(cov_cols) > 53) {
    cli::cli_abort("Binary hashing supports a maximum of 53 covariates.")
  }

  # Each covariate gets a power-of-2 weight so that each unique combination
  # of missing covariates maps to a unique integer hash
  weights <- 2^(seq_along(cov_cols) - 1)

  coefs_to_use <- as.matrix(coefs_to_use[, cov_cols, drop = FALSE])
  coefs_na_mat <- is.na(coefs_to_use)
  coefs_hashes <- as.vector(coefs_na_mat %*% weights)

  X <- as.matrix(X[, cov_cols, drop = FALSE])
  nv <- nrow(X)

  # Match each observation to the model whose NA pattern matches its own,
  # i.e. the fullest model that only uses covariates present for that row
  obs_hashes <- as.vector(is.na(X) %*% weights)
  unique_hashes <- unique(obs_hashes)

  # Unmatched rows stay NA — if no model exists for a given missingness
  # pattern, we can't compute a prediction
  pred <- rep(NA_real_, nv)
  coef_idx <- rep(NA_integer_, nv)

  for (h in unique_hashes) {
    c_idx <- which(coefs_hashes == h)
    if (length(c_idx) != 1L) {
      next
    }

    obs_idx <- which(obs_hashes == h)
    active <- !coefs_na_mat[c_idx, ]

    pred[obs_idx] <- as.vector(
      X[obs_idx, active, drop = FALSE] %*% coefs_to_use[c_idx, active]
    )
    coef_idx[obs_idx] <- c_idx
  }

  # Reconstruct full-length output; unmatched rows and rows with
  # missing raw_scores propagate as NA through rmses[NA_integer_]
  out <- rep(NA_real_, n)
  out[valid] <- (raw_scores - pred) / rmses[coef_idx]

  return(out)
}
