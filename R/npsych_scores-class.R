#' Neuropsychological Test Scores
#'
#' @description
#' Create an `npsych_scores` object, a validated numeric vector representing
#' neuropsychological test scores. This is the parent class for all
#' test-specific subclasses (e.g., `MOCATOTS`, `TRAILA`).
#'
#' @param .data Numeric vector of test scores. Values must fall within
#'   `range` or match one of the `codes`, or be `NA`.
#' @param label A single character string identifying the test (e.g., `"MoCA"`).
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
    range = S7::class_double,
    codes = S7::class_double
  ),
  validator = function(self) {
    scores <- S7::S7_data(self)

    label <- self@label
    range <- self@range
    codes <- self@codes

    errs <- character()

    if (!is.character(label) || length(label) != 1) {
      errs <- c(errs, cli::format_inline("{.arg label} must be a string."))
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
      !all((scores >= range[1] & scores <= range[2]) | scores %in% c(codes, NA))
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

  numeric_x[numeric_x %in% x@codes] <- NA

  numeric_x
}
