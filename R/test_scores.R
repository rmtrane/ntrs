################################
##
## S3 classes and methods
##
################################

################################
## Parent class test_scores.

#' Create a `test_scores` class
#'
#' @description Create a `test_scores` class. Used internally to create test specific classes.
#'
#'
#' @param scores A numeric vector. Will error if not numeric.
#' @param label A single string.
#' @param range A numeric vector, typically of length two.
#' @param codes A vector or list of codes.
#' @param class Optional.
#'
#' @returns
#' A numeric vector with additional attributes `label`, `range`, `codes`,
#' and classes `test_scores` and `class`. Raises an error if `scores` is
#' not a numeric vector.
#'
#' @keywords internal
new_test_scores <- function(
  scores,
  label,
  range,
  codes,
  class
) {
  if (!is.numeric(scores)) {
    cli::cli_abort("{.arg scores} must be a numeric vector.")
  }

  if (!is.character(class)) {
    cli::cli_abort("{.arg class} must be a character vector.")
  }

  structure(
    scores,
    label = label,
    range = range,
    codes = codes,
    class = c(class, "test_scores")
  )
}

#' Test scores
#'
#' @description
#' A short description...
#'
#' @param scores A numeric vector.
#' @param label A single string.
#' @param range A numeric vector of length 2.
#' @param codes A character vector. Optional. Should be a named character vector of the form `c("error_label" = {numeric code})`.
#' @param class A single string. Optional.
#'
#' @returns
#' A validated `test_scores` object. Will error if validation fails.
#'
#' @export
test_scores <- function(
  scores = numeric(),
  label,
  range,
  codes = numeric(),
  class = character()
) {
  validate_test_scores(
    x = new_test_scores(scores, label, range, codes, class)
  )
}

#' Validate test scores
#'
#' @description
#' Used to validate the creation of new `test_scores`.
#'
#' @param x An object representing test scores created using `test_scores()`. It must be coercible
#'   to a numeric vector
#'   and have the following attributes: `label` (a single string), `range` (a numeric vector
#'   of length 2), and `codes` (a named numeric vector). Its class (excluding `"test_scores"`)
#'   must be a single character string.
#'
#' @returns
#' `x`, invisibly, if all validations pass. The function will raise an error if:
#'   - The class of `x` (excluding `"test_scores"`) is not a single character string.
#'   - The `label` attribute is not a single string.
#'   - The `range` attribute is not a numeric vector of length 2.
#'   - The `codes` attribute is not a named numeric vector.
#'   - Any score in `x` is not within the specified `range` and not among the `codes`.
#'
#' @keywords internal
validate_test_scores <- function(
  x
) {
  scores <- as.numeric(x)

  label <- attr(x, "label")
  range <- attr(x, "range")
  codes <- attr(x, "codes")

  cls <- setdiff(class(x), "test_scores")

  if (length(cls) != 1) {
    cli::cli_abort(
      "{.arg class} must be of length one, but is length {length(cls)}."
    )
  }

  if (!is.character(label) || length(label) != 1) {
    cli::cli_abort("{.arg label} must be a string.")
  }

  if (!is.numeric(range) || length(range) != 2) {
    cli::cli_abort("{.arg range} must be a numeric vector of length 2.")
  }

  if (length(codes) > 0 & (!is.numeric(codes) || is.null(names(codes)))) {
    cli::cli_abort("{.arg codes} must be a named numeric vector.")
  }

  if (!all((scores >= range[1] & scores <= range[2]) | scores %in% codes)) {
    cli::cli_abort(
      "{.arg scores} must all be in the range {.val {range}} or one of the {.arg codes} {.or {paste0(codes, ' (', names(codes), ')')}}."
    )
  }

  x
}


#' Remove error codes
#'
#' @description
#' Removes error codes from the `test_scores` object using the `codes` attribute.
#'
#' @param x An object of class `test_scores`
#'
#' @returns
#' An object with error codes removed.
#'
#' @export
remove_errorcodes <- function(x) {
  if (!inherits(x, "test_scores")) {
    cli::cli_abort("{.arg x} must be a {.cls test_scores} object.")
  }

  numeric_x <- as.numeric(x)

  numeric_x[numeric_x %in% attr(x, "codes")] <- NA

  numeric_x
}
