.std_defaults <- local(new.env(parent = emptyenv()))

#' Set the default standardization method (and version) an `npsych_scores` subclass
#'
#' @param scores S7 object of class `npsych_scores` with subclass.
#' @param method Character string, for example "norms" or "regression"
#' @param version Character string identifying the version to set as default
#' @param overwrite Logical (default: `FALSE`); should existing default, if it exists, be overwritten?
#'
#' @export
set_std_defaults <- function(
  scores,
  method,
  version = NULL,
  overwrite = FALSE
) {
  if (!S7::S7_inherits(scores, npsych_scores)) {
    cli::cli_abort(
      "{.arg scores} must be an object of class {.cls npsych_scores}, but is {.cls {class(scores)}}."
    )
  }

  # Verify method + version combo exists if version is not NULL (will error if not)

  if (!is.null(version)) {
    invisible(get_version_data(scores, method, version))
  }

  scores_class <- S7::S7_class(scores)@name

  # Check if scores_class already has a default method
  if (exists(scores_class, envir = .std_defaults, inherits = FALSE)) {
    cur_default <- .std_defaults[[scores_class]]
    new_default <- c(
      list(method = method),
      if (!is.null(version)) list(version = version)
    )

    # if (cur_default$method == method && identical(cur_default$version)) {
    if (identical(cur_default, new_default)) {
      cli::cli_inform(
        c(
          "i" = "Default method and version for {.val {scores_class}} already set to {.val {method}} and {.val {version}}."
        )
      )

      return(invisible())
    }

    if (overwrite) {
      cli::cli_bullets(c(
        "i" = "Default method and version for {.val {scores_class}} was previously set to {.val {cur_default$method}} and {.val {cur_default$version}}.",
        "i" = "Will be overwritten."
      ))
    } else {
      cli::cli_abort(c(
        "i" = "Default method and version for {.val {scores_class}} was previously set to {.val {cur_default$method}} and {.val {cur_default$version}}.",
        "x" = "To overwrite, use {.arg overwrite = TRUE}."
      ))
    }
  }

  # Set default method, version
  .std_defaults[[scores_class]] <- c(
    list("method" = method),
    if (!is.null(version)) list("version" = version)
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
#' @param scores S7 object of class `npsych_scores` with subclass.
#'
#' @return Character string with default method and version (when applicable)
#'
#' @export
get_std_defaults <- function(scores) {
  if (!S7::S7_inherits(scores, npsych_scores)) {
    cli::cli_abort(
      "{.arg scores} must be an object of class {.cls npsych_scores}, but is {.cls {class(scores)}}."
    )
  }

  scores_class <- S7::S7_class(scores)@name

  if (!exists(scores_class, envir = .std_defaults, inherits = FALSE)) {
    cli::cli_alert_info(
      "No default method set for {.cls {scores_class}}."
    )
    return(NULL)
  }

  .std_defaults[[scores_class]]
}
