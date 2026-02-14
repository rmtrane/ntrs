#' Get version data
#'
#' @description
#' A short description...
#'
#' @param method A single string specifying the method.
#' @param test_class A single string specifying the test class.
#' @param version A single string. If `"default"` (default), the function will attempt to retrieve the default version for the given method and test class.
#'
#' @returns
#' The object representing the requested version data. The function will error if `version` is `"default"` and no default version is set,
#' or if the specified `version` (or the resolved default version) does not exist for the given `method` and `test_class`.
#'
#' @export
get_version_data <- function(test_class, method = "default", version = NULL) {
  UseMethod("get_version_data")
}

#' @export
get_version_data.test_scores <- function(
  test_class,
  method = "default",
  version = NULL
) {
  if (method == "default") {
    return(get_default_method(test_class))
  }

  all_versions <- get_versions(test_class, method)

  # Check if version exists
  if (!version %in% all_versions) {
    cli::cli_abort(c(
      "x" = "No version {.val {version}} for {.field {method}} method on {.val {test_class}}",
      "i" = "Available versions: {.val {all_versions}}"
    ))
  }

  test_class <- setdiff(class(test_class), "test_scores")

  .std_versions[[method]][[test_class]][[version]][["data"]]
}
