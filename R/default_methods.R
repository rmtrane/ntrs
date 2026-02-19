#' Set the default version for a method/test_class combination
#'
#' @param test_class Character string identifying the test class
#' @param method Character string: "norms" or "regression"
#' @param version Character string identifying the version to set as default
#' @param overwrite Logical (default: `FALSE`); should existing default, if it exists, be overwritten?
#'
#' @export
set_default_method <- function(scores_obj, method, version, overwrite = FALSE) {
  UseMethod("set_default_method")
}

#' @export
set_default_method.test_scores <- function(
  scores_obj,
  method,
  version,
  overwrite = FALSE
) {
  # Verify method + version combo exists (will error if not)
  invisible(get_version_data(scores_obj, method, version))

  # Create "defaults" environment if not already done
  if (!exists("defaults", envir = .std_versions, inherits = FALSE)) {
    .std_versions[["defaults"]] <- new.env(parent = emptyenv())
  }

  scores_class <- setdiff(class(scores_obj), "test_scores")

  # Check if scores_class already has a default method
  if (
    exists(scores_class, envir = .std_versions[["defaults"]], inherits = FALSE)
  ) {
    cur_default <- .std_versions[["defaults"]][[test_class]]
    if (
      cur_default[["method"]] == method && cur_default[["version"]] == version
    ) {
      cli::cli_inform(
        c(
          "i" = "Default method and version for {.val {test_class}} already set to {.val {method}} and {.val {version}}."
        ),
        class = "packageStartupMessage"
      )

      return(invisible())
    }

    if (overwrite) {
      cli::cli_bullets(c(
        "i" = "Default method and version for {.val {test_class}} was previously set to {.val {method}} and {.val {version}}.",
        "i" = "Will be overwritten."
      ))
    } else {
      cli::cli_abort(c(
        "i" = "Default method and version for {.val {test_class}} was previously set to {.val {method}} and {.val {version}}.",
        "x" = "To overwrite, use {.arg overwrite = TRUE}."
      ))
    }
  }

  # Set as attribute on the test_class environment
  .std_versions[["defaults"]][[test_class]] <- c(
    "method" = method,
    "version" = version
  )

  cli::cli_inform(
    c(
      "v" = "Set {.val {version}} as default for {.field {method}} method on {.cls {class(test_class)[1]}}"
    ),
    class = "packageStartupMessage"
  )

  invisible()
}

#' Get the default version for a method/test_class combination
#'
#' @param scores_obj Vector of class `npsych_scores` with subclass.
#'
#' @return Character string of default version, or NULL if none set
#' @export
get_default_method <- function(scores_obj) {
  UseMethod("get_default_method")
}

#' @export
get_default_method.test_scores <- function(test_class) {
  test_class <- setdiff(class(test_class), "test_scores")

  if (
    !exists("defaults", envir = .std_versions, inherits = FALSE) ||
      !exists(test_class, envir = .std_versions[["defaults"]], inherits = FALSE)
  ) {
    cli::cli_abort(
      "No default method set for {.cls {test_class}}."
    )
  }

  def_version <- .std_versions[["defaults"]][[test_class]]

  .std_versions[[def_version[["method"]]]][[test_class]][[def_version[[
    "version"
  ]]]]
}
