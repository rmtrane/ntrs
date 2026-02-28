#' Get version data
#'
#' @description
#' Retrieve the standardization data for a specific method and version of a
#' test score class from the `.std_versions` registry. When `method` and/or
#' `version` are `NULL`, defaults are resolved via [get_std_defaults()].
#'
#' @param scores An `npsych_scores` object (e.g., `MOCATOTS()`).
#' @param method A single string specifying the standardization method
#'   (e.g., `"norms"`, `"regression"`). If `NULL` (default), the default
#'   method for the scores class is used.
#' @param version A single string specifying the version name. If `NULL`
#'   (default), the default version for the resolved method is used. Must be
#'   `NULL` when `method` is `NULL`.
#'
#' @returns The registered standardization data for the requested
#'   method/version combination. Errors if the specified (or resolved default)
#'   version does not exist.
#'
#' @export
get_version_data <- function(
  scores,
  method = NULL,
  version = NULL
) {
  if (!S7::S7_inherits(scores, npsych_scores)) {
    cli::cli_abort(
      "{.arg scores} must be an object of class {.cls npsych_scores}, but is {.cls {class(scores)}}."
    )
  }

  scores_class <- S7::S7_class(scores)@name

  ## If method is NULL
  if (is.null(method)) {
    ## ... version must also be NULL
    if (!is.null(version)) {
      cli::cli_abort(
        "{.arg version} must be {.val NULL} when {.arg method} is {.val NULL}, but is {.val {version}}."
      )
    }

    scores_defaults <- get_std_defaults(scores)

    method <- scores_defaults$method

    if ("version" %in% names(scores_defaults)) {
      version <- scores_defaults$version
    }
  } else {
    # If method is given, check that it is valid
    avail_methods <- list_std_methods(scores)
    if (!method %in% avail_methods) {
      cli::cli_abort(
        "{.val {method}} is not implemented for {.cls {scores_class}}. Available methods are {.val {avail_methods}}."
      )
    }
  }

  all_versions <- list_method_versions(scores, method)

  # Check if version exists
  if (!version %in% all_versions) {
    cli::cli_abort(c(
      "x" = "No version {.val {version}} for {.field {method}} method on {.val {scores_class}}",
      "i" = "Available versions: {.val {all_versions}}"
    ))
  }

  .std_versions[[method]][[scores_class]][[version]]
}
