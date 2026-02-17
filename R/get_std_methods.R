#' Get standardization methods for test scores
#'
#' @description Get methods that are available for a test class, or all
#'   available methods.
#'
#' @param test_class A `test_scores` object, such as `MOCATOTS()`. When the
#'   argument is supplied, only methods with registered versions for that
#'   specific test class are returned. When called via the `.test_scores`
#'   method with a missing argument, all registered methods are returned.
#'
#' @returns A character vector of standardization method names.
#'
#' @export
get_std_methods <- function(test_class) {
  UseMethod("get_std_methods")
}

#' @export
get_std_methods.test_scores <- function(test_class) {
  if (missingArg(test_class)) {
    return(setdiff(ls(envir = .std_versions), "defaults"))
  }

  test_class <- setdiff(class(test_class), "test_scores")

  with_test_class <- lapply(as.list(.std_versions), ls) |>
    sapply(\(x) test_class %in% x)

  setdiff(names(which(with_test_class)), "defaults")
}
