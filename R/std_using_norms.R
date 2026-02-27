#' Standardize `npsych_scores` using norms
#'
#' @description
#' A short description...
#'
#' @param scores A numeric vector.
#' @param ... Arguments passed to methods.
#'
#' @returns
#' The standardized value of `x`, whose exact type depends on the specific S3
#' method called.
#'
#' @export
std_using_norms <- function(scores, ...) {
  UseMethod("std_using_norms")
}

#' Standardize test scores using norms
#'
#' @description
#' A short description...
#'
#' @param scores A numeric vector of raw test scores.
#' @param ... Additional numeric vectors representing covariates, for example `age`, `educ`, `sex`.
#'   These must be of length one or the same length as `npsych_scores`, and named as columns in the
#'   `lookup_table` for the specified `version`; use `get_version_data({npsych_scores}, "norms", {version})$lookup_table`
#'   to inspect the table.
#' @param version A single string specifying the version of the norms to use.
#'
#' @returns
#' A numeric vector of standardized scores. The function will error if `version` is not registered, if provided covariates are not numeric, if required covariates are missing based on the `lookup_table` for the specified `version`, or if covariate lengths are mismatched.
#'
#' @export
std_using_norms.npsych_scores <- function(
  scores,
  ...,
  version
) {
  raw_scores <- as.numeric(scores)

  ## Get version data (will fail if version not registered).
  version_data <- get_version_data(scores, "norms", version)

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
    !sapply(covars, length) %in% c(1, length(scores))
  ]

  if (length(mismatched_length) > 0) {
    cli::cli_abort(
      "{.arg {mismatched_length}} {?is/are} of length {lengths(covars[mismatched_length])}, but must be of length one or same as {.arg scores} ({.val {length(scores)}})"
    )
  }

  ## Drop covars supplied that are not needed
  covars_not_needed <- setdiff(names(covars), covars_needed)

  if (length(covars_not_needed)) {
    cli::cli_warn(
      "{.arg {covars_not_needed}} {?is/are} not needed to standardize using {.val norms} when version is {.val {version}} and will be ignored."
    )

    covars <- covars[setdiff(names(covars), covars_not_needed)]
  }

  ## Apply covariate prep functions
  covar_fns <- version_data$covar_fns

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

  ## Create data.frame that we will match means and sd to
  match_to <- cbind(
    raw_scores = as.numeric(scores),
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
