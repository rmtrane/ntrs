#' List `npsych_scores` subclasses
#'
#' @description
#' This function is a helper that lists all available subclasses, i.e. all functions created using `new_class <- new_npsych_scores(...)`.
#'
#' @returns
#' A character vector of valid test subclass names.
#'
#' @export
list_npsych_scores <- function() {
  ls(envir = .npsych_classes)
}
