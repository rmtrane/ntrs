#' Validate registration parameters for standardization versions
#'
#' Internal helper function that validates the `version` and `description`
#' parameters used when registering a new standardization method version.
#' This function is called by `.register_std_version()` to ensure parameter
#' validity before storing version data.
#'
#' @param version Character string identifying the version. Must be a single
#'   non-empty character string (length 1).
#' @param description Character string describing the version. Must be a single
#'   non-empty character string (length 1).
#'
#' @return Invisible `NULL` if validation passes. Throws an error if either
#'   parameter fails validation.
#'
#' @details
#' The function checks that both `version` and `description`:
#' \itemize{
#'   \item Are character vectors
#'   \item Have length exactly 1
#'   \item Are not `NULL`
#' }
#'
#' If validation fails, an error is thrown with a descriptive message
#' indicating which parameter failed and why.
#'
#' @keywords internal
.validate_registration_params <- function(version, description) {
  if (!is.character(version) || length(version) != 1 || version == "") {
    cli::cli_abort(c(
      "{.arg version} must be a non-empty character string",
      "x" = "You supplied {.cls {class(version)}} of length {length(version)}",
      "i" = "Example valid values: {.val updated}, {.val v2024}, {.val baseline}"
    ))
  }

  if (!is.character(description) || length(description) != 1) {
    cli::cli_abort(c(
      "{.arg description} must be a character string",
      "x" = "You supplied {.cls {class(description)}} of length {length(description)}"
    ))
  }

  invisible()
}
