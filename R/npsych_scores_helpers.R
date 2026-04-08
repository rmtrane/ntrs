#' Remove error codes
#'
#' @description
#' Removes error codes from the `npsych_scores` object using the `codes` attribute.
#'
#' @param x An object of class `npsych_scores`
#'
#' @returns
#' An object with error codes removed.
#'
#' @examples
#' # 88 is an error code for MOCATOTS (outside range 0-30)
#' moca <- MOCATOTS(c(25, 88, 28))
#' remove_error_codes(moca) # 25 NA 28
#'
#' @export
remove_error_codes <- function(x) {
  if (!S7::S7_inherits(x, npsych_scores)) {
    cli::cli_abort("{.arg x} must be a {.cls npsych_scores} object.")
  }

  numeric_x <- as.numeric(x)

  # codes might not be error codes. We find error codes by checking if codes are inside range
  all_codes <- x@codes
  error_codes <- all_codes[all_codes < x@range[1] | all_codes > x@range[2]]

  # replace error codes by NA
  numeric_x[numeric_x %in% error_codes] <- NA

  numeric_x
}

#' Replace codes with their labels
#'
#' @description
#' Replaces numeric codes in a `npsych_scores` object with their corresponding
#' named labels from the `codes` attribute.
#'
#' @param x An object of class `npsych_scores`.
#'
#' @returns A numeric vector with code values replaced by their named labels.
#'
#' @examples
#' moca <- MOCATOTS(c(25, 88, -4, 28))
#' replace_codes(moca)
#'
#' @export
replace_codes <- function(x) {
  if (!S7::S7_inherits(x, npsych_scores)) {
    cli::cli_abort("{.arg x} must be a {.cls npsych_scores} object.")
  }

  numeric_x <- as.numeric(x)

  all_codes <- x@codes
  for (code in names(all_codes)) {
    numeric_x[numeric_x == all_codes[code]] <- code
  }

  numeric_x
}

#' Is `x` an npsych_scores object?
#'
#' @description
#' Tests whether `x` is an `npsych_scores` object.
#'
#' @param x An R object.
#'
#' @returns
#' A single logical value: `TRUE` if `x` is an `npsych_scores` object, `FALSE` otherwise.
#'
#' @examples
#' moca <- MOCATOTS(c(25, 28))
#' is_npsych_scores(moca) # TRUE
#'
#' traila <- c(53, 75)
#' is_npsych_scores(traila) # FALSE
#'
#'
#' @export
is_npsych_scores <- function(x) {
  S7::S7_inherits(x, npsych_scores)
}
