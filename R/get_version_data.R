#' Get version data
#'
#' @description
#' A short description...
#'
#' @param scores A single string specifying the test class.
#' @param method A single string specifying the method.
#' @param version A single string. If `"default"` (default), the function will attempt to retrieve the default version for the given method and test class.
#'
#' @returns
#' The object representing the requested version data. The function will error if `version` is not `NULL` when `method = NULL`,
#' or if the specified `version` (or the resolved default version) does not exist for the given `method` and `npsych_scores`.
#'
#' @export
get_version_data <- function(scores, method = NULL, version = NULL) {
  UseMethod("get_version_data")
}

#' @export
get_version_data.npsych_scores <- function(
  scores,
  method = "default",
  version = NULL
) {
  scores_class <- setdiff(class(scores), "npsych_scores")

  ## If method is NULL
  if (is.null(method)) {
    ## ... version must also be NULL
    if (!is.null(version)) {
      cli::cli_abort(
        "{.arg version} must be {.val NULL} when {.arg method} is {.val NULL}, but is {.val {version}}."
      )
    }

    scores_defaults <- get_std_defaults(scores)

    method <- scores_defaults["method"]

    if (version %in% names(scores_defaults)) {
      version <- scores_defaults["version"]
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

  .std_versions[[method]][[scores_class]][[version]][["data"]]
}
