#' Get available version names for a standardization method
#'
#' @param test_class A `test_scores` object, such as `MOCATOTS()`.
#' @param method Character string identifying the standardization method
#'   (e.g., `"norms"`, `"regression"`).
#'
#' @return A character vector of registered version names.
#'
#' @export
get_versions <- function(test_class, method) {
  UseMethod("get_versions")
}

#' @export
get_versions.test_scores <- function(test_class, method) {
  # Check if method exists
  if (!exists(method, envir = .std_versions, inherits = FALSE)) {
    available_methods <- ls(envir = .std_versions)

    if (length(available_methods) == 0) {
      cli::cli_abort(c(
        "x" = "No versions have been registered for any methods yet",
        "i" = "Register versions using {.fn register_norms_version} or {.fn register_regression_version}, or write your own registration function that calls {.fn .register_std_version}."
      ))
    } else {
      cli::cli_abort(c(
        "No versions registered for method {.val {method}}",
        "i" = "Available methods: {.val {available_methods}}"
      ))
    }
  }

  test_class <- setdiff(class(test_class), "test_scores")

  # Check if test_class exists
  if (!exists(test_class, envir = .std_versions[[method]], inherits = FALSE)) {
    available_classes <- ls(.std_versions[[method]])

    cli::cli_abort(c(
      "No versions registered for test class {.val {test_class}}",
      "x" = "Method {.val {method}} has no versions for {.val {test_class}}",
      "i" = "Available test classes: {.val {available_classes}}"
    ))
  }

  ls(.std_versions[[method]][[test_class]])
}
