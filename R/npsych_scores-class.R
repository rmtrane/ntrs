#' @include npsych_subclass_helpers.R
NULL

#' Neuropsychological Test Scores
#'
#' @description
#' Create an `npsych_scores` object, a validated numeric vector representing
#' neuropsychological test scores. This is the parent class for all
#' test-specific subclasses (e.g., `MOCATOTS`, `TRAILA`).
#'
#' @param .data Numeric vector of test scores. Values must fall within
#'   `range` or match one of the `codes`, or be `NA`.
#' @param label A single string identifying the test
#'   (e.g., `"MoCA"`, `"Trail Making Test Part A"`).
#' @param domain A single string identifying the domain the test belongs to
#'   (e.g., `"General Cognition"`, `"Language"`). Defaults to an empty
#'   character vector.
#' @param short_descriptor A short descriptor giving a little bit more
#'   information than the label. When available, the entry from the Researchers
#'   Data Dictionary; see [rdd] for more.
#' @param range A numeric vector of length 2 giving the minimum and maximum
#'   valid scores.
#' @param codes A named numeric vector of error/special codes
#'   (e.g., `c("Not administered" = 88)`). Defaults to an empty numeric vector.
#'
#' @returns
#' An S7 object of class `npsych_scores`, inheriting from `class_double`,
#' with properties `label`, `range`, and `codes`.
#'
#' @export
npsych_scores <- S7::new_class(
  "npsych_scores",
  parent = S7::class_double,
  properties = list(
    label = S7::class_character,
    domain = S7::class_character,
    short_descriptor = S7::class_character,
    range = S7::class_double,
    codes = S7::class_double
  ),
  validator = function(self) {
    scores <- S7::S7_data(self)

    label <- self@label
    domain <- self@domain
    short_descriptor <- self@short_descriptor
    range <- self@range
    codes <- self@codes

    errs <- character()

    if (!is.character(label) || length(label) != 1) {
      errs <- c(errs, cli::format_inline("{.arg label} must be a string."))
    }

    if (!is.null(domain) && (!is.character(domain) || length(domain) > 1)) {
      errs <- c(errs, cli::format_inline("{.arg domain} must be a string."))
    }

    if (!is.character(short_descriptor) || length(short_descriptor) > 1) {
      errs <- c(
        errs,
        cli::format_inline("{.arg short_descriptor} must be a string.")
      )
    }

    if (!is.numeric(range) || length(range) != 2) {
      errs <- c(
        errs,
        cli::format_inline("{.arg range} must be a numeric vector of length 2.")
      )
    }

    if (length(codes) > 0 & (!is.numeric(codes) || is.null(names(codes)))) {
      errs <- c(
        errs,
        cli::format_inline("{.arg codes} must be a named numeric vector.")
      )
    }

    if (
      !all(
        (scores >= range[1] & scores <= range[2]) |
          scores %in% codes |
          is.na(scores)
      )
    ) {
      errs <- c(
        errs,
        cli::format_inline(
          "{.arg scores} must all be in the range {.val {range}} or one of the {.arg codes} {.or {paste0(codes, ' (', names(codes), ')')}}."
        )
      )
    }

    if (length(errs) > 0) {
      return(errs)
    }

    NULL
  }
)

if (FALSE) {
  # Should work
  tmp <- npsych_scores(
    scores = c(1, 2, NA, 99),
    label = "MoCA",
    range = c(0, 30),
    codes = c("N/A" = 99)
  )

  # Should fail
  npsych_scores(
    scores = c(1, 2, NA, 90),
    label = "MoCA",
    range = c(0, 30),
    codes = c("N/A" = 99)
  )
}

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


#' Subset `npsych_scores` objects.
#'
#' @param x An S7 object of class `npsych_scores`.
#' @param i A numeric index.
#'
#' @returns
#' A new `npsych_scores` object containing the elements of `x` specified by `i`.
#'
#' @keywords internal
#'
#' @name `[`
S7::method(`[`, npsych_scores) <- function(x, i) {
  npsych_scores_constructor <- S7::S7_class(x)@name

  do.call(npsych_scores_constructor, args = list(x = as.integer(x)[i]))
}


#' Is `x` an npsych_scores object?
#'
#' @description#' A short description...
#'
#' @param x An R object.
#'
#' @returns
#' A single logical value: `TRUE` if `x` is an `npsych_scores` object, `FALSE` otherwise.
#'
#' @export
is_npsych_scores <- function(x) {
  S7::S7_inherits(x, npsych_scores)
}
