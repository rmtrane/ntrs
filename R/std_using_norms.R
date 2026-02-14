#' Standardize `test_scores` using norms
#'
#' @description
#' A short description...
#'
#' @param x A numeric vector.
#' @param age A numeric value.
#' @param sex A single string or numeric value.
#' @param educ A numeric value.
#' @param version A single string or numeric value.
#' @param ... Arguments passed to methods.
#'
#' @returns
#' The standardized value of `x`, whose exact type depends on the specific S3
#' method called.
#'
#' @export
std_using_norms <- function(test_scores, age, sex, educ, version, ...) {
  UseMethod("std_using_norms")
}

#' @export
std_using_norms.test_scores <- function(
  test_scores,
  ...,
  version
) {
  raw_scores <- as.numeric(test_scores)

  ## Get version data (will fail if version not registered).
  version_data <- get_version_data(test_scores, "norms", version)

  ## Pull out lookup table
  lookup_table <- version_data$lookup_table

  ## Check covariates
  covars <- rlang::list2(...)

  covars_not_numeric <- sapply(covars, \(x) !is.numeric(x))
  if (any(covars_not_numeric)) {
    cli::cli_abort(
      "{.arg {names(covars_not_numeric[covars_not_numeric])}} must be {.cls numeric}."
    )
  }

  covars_needed <- setdiff(colnames(lookup_table), c("n", "m", "sd"))
  covars_missing <- setdiff(covars_needed, names(covars))

  if (length(covars_missing)) {
    cli::cli_abort(
      "{.arg {covars_missing}} are missing, but needed to standardize using {.val norms} when version is {.val {version}}."
    )
  }

  mismatched_length <- names(covars)[
    !sapply(covars, length) %in% c(1, length(test_scores))
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

  ## Create data.frame that we will match means and sd to
  match_to <- cbind(
    raw_scores = as.numeric(test_scores),
    data.frame(covars)
  )

  match_to$id <- 1:nrow(match_to)

  match_to$for_merge <- do.call(
    paste,
    c(
      as.list(match_to[,
        names(covars),
        drop = F
      ]),
      sep = "__"
    )
  )

  lookup_table$for_merge <- do.call(
    paste,
    c(as.list(lookup_table[, names(covars), drop = F]), sep = "__")
  )

  match_to[, intersect(
    c("n", "m", "sd"),
    colnames(lookup_table)
  )] <- lookup_table[
    match(match_to$for_merge, lookup_table$for_merge),
    intersect(c("n", "m", "sd"), colnames(lookup_table))
  ]

  for_standardizing <- match_to[order(match_to$id), c("raw_scores", "m", "sd")]

  with(
    for_standardizing,
    (raw_scores - m) / sd
  )
}
