#' Get available version names for a standardization method
#'
#' @param scores A `npsych_scores` object, such as `MOCATOTS()`.
#' @param method Character string identifying the standardization method
#'   (e.g., `"norms"`, `"regression"`).
#'
#' @return A character vector of registered version names.
#'
#' @export
list_method_versions <- function(scores, method) {
  if (!S7::S7_inherits(scores, npsych_scores)) {
    cli::cli_abort(
      "{.arg scores} must be an object of class {.cls npsych_scores}, but is {.cls {class(scores)}}."
    )
  }

  # Check if method exists
  if (!exists(method, envir = .std_versions, inherits = FALSE)) {
    # available_methods <- ls(envir = .std_versions)
    return(character())
    # return(ls(envir = .std_versions))

    # if (length(available_methods) == 0) {
    #   cli::cli_abort(c(
    #     "x" = "No versions have been registered for any methods yet",
    #     "i" = "Register versions using {.fn register_norms_version} or {.fn register_regression_version}, or write your own registration function that calls {.fn .register_std_version}."
    #   ))
    # } else {
    #   cli::cli_abort(c(
    #     "No versions registered for method {.val {method}}",
    #     "i" = "Available methods: {.val {available_methods}}"
    #   ))
    # }
  }

  scores_class <- S7::S7_class(scores)@name

  # Check if scores_class exists
  if (
    !exists(scores_class, envir = .std_versions[[method]], inherits = FALSE)
  ) {
    # ls(.std_versions[[method]])
    return(character())
  }

  #   cli::cli_abort(c(
  #     "No versions registered for test class {.val {scores_class}}",
  #     "x" = "Method {.val {method}} has no versions for {.val {scores_class}}",
  #     "i" = "Available test classes: {.val {available_classes}}"
  #   ))
  # }

  # available_versions <- ls(.std_versions[[method]][[scores_class]])

  return(ls(.std_versions[[method]][[scores_class]]))

  # if (length(available_versions) == 0) {
  #   cli::cli_abort(c(
  #     "No versions registered for test class {.val {scores_class}}",
  #     "x" = "Method {.val {method}} has no versions for {.val {scores_class}}",
  #     "i" = "Register versions using {.fn register_norms_version} or {.fn register_regression_version}, or write your own registration function that calls {.fn .register_std_version}."
  #   ))
  # }

  # available_versions
}
