.std_defaults <- new.env(parent = emptyenv())

#' Set the default standardization method (and version) an `npsych_scores` subclass
#'
#' @param scores Numeric vector of class `npsych_scores`
#' @param method Character string: "norms" or "regression"
#' @param version Character string identifying the version to set as default
#' @param overwrite Logical (default: `FALSE`); should existing default, if it exists, be overwritten?
#'
#' @export
set_std_defaults <- function(scores, method, version, overwrite = FALSE) {
  UseMethod("set_std_defaults")
}

#' @export
set_std_defaults.npsych_scores <- function(
  scores,
  method,
  version,
  overwrite = FALSE
) {
  # Verify method + version combo exists (will error if not)
  invisible(get_version_data(scores, method, version))

  scores_class <- setdiff(class(scores), "npsych_scores")

  # Check if scores_class already has a default method
  if (exists(scores_class, envir = .std_defaults, inherits = FALSE)) {
    cur_default <- .std_defaults[[scores_class]]
    if (cur_default$method == method && cur_default$version == version) {
      cli::cli_inform(
        c(
          "i" = "Default method and version for {.val {scores_class}} already set to {.val {method}} and {.val {version}}."
        )
      )

      return(invisible())
    }

    if (overwrite) {
      cli::cli_bullets(c(
        "i" = "Default method and version for {.val {scores_class}} was previously set to {.val {method}} and {.val {version}}.",
        "i" = "Will be overwritten."
      ))
    } else {
      cli::cli_abort(c(
        "i" = "Default method and version for {.val {scores_class}} was previously set to {.val {method}} and {.val {version}}.",
        "x" = "To overwrite, use {.arg overwrite = TRUE}."
      ))
    }
  }

  # Set default method, version
  .std_defaults[[scores_class]] <- list(
    "method" = method,
    "version" = version
  )

  cli::cli_inform(
    c(
      "v" = "Set {.val {version}} as default for {.field {method}} method on {.cls {scores_class}}"
    ),
    class = "packageStartupMessage"
  )

  invisible()
}

#' Get standardization defaults `npsych_scores`
#'
#' @param scores Vector of class `npsych_scores` with subclass.
#'
#' @return Character string with default method and version (when applicable)
#'
#' @export
get_std_defaults <- function(scores) {
  UseMethod("get_std_defaults")
}

#' Get standard defaults for npsych_scores
#'
#' @description
#' A short description...
#'
#' @param scores An object of class `npsych_scores`.
#'
#' @returns
#' The standard defaults associated with the specific class of the `scores` object.
#' The function will raise an error if no default method is set for the `scores_class`.
#'
#' @rdname get_std_defaults
#'
#' @export
get_std_defaults.npsych_scores <- function(scores) {
  scores_class <- setdiff(class(scores), "npsych_scores")

  if (!exists(scores_class, envir = .std_defaults, inherits = FALSE)) {
    cli::cli_abort(
      "No default method set for {.cls {scores_class}}."
    )
  }

  .std_defaults[[scores_class]]
}
