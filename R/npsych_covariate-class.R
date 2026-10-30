#' Covariates Used for Standardization
#'
#' @description
#' Create an `npsych_covariate` object, a validated numeric vector holding a
#' NACC-coded covariate such as age, sex, education or race. This is the parent
#' class for the covariate-specific subclasses ([NACCAGE], [SEX], [EDUC],
#' [RACE], [MEMTIME]).
#'
#' Every non-missing value must be a valid value (inside `range` or one of
#' `values`) or one of the `codes` for unknown or unavailable data; anything
#' else is an error. Use [valid_values()] to get the values with the codes
#' replaced by `NA`.
#'
#' @param .data Numeric vector of covariate values.
#' @param label A single string identifying the covariate (e.g., `"Age"`).
#' @param short_descriptor A short description of the covariate. When
#'   available, the entry from the Researchers Data Dictionary; see [rdd].
#' @param range Either an empty numeric vector, or a numeric vector of length 2
#'   giving the minimum and maximum valid values for a continuous covariate.
#' @param values A numeric vector of valid values for a categorical covariate,
#'   optionally named with their labels (e.g., `c(Male = 1, Female = 2)`).
#' @param codes A named numeric vector of codes for unknown or unavailable
#'   values (e.g., `c(Unknown = 99)`). These are accepted on construction and
#'   become `NA` in [valid_values()].
#'
#' @returns
#' An S7 object of class `npsych_covariate`, inheriting from `class_double`,
#' with properties `label`, `short_descriptor`, `range`, `values`, and `codes`.
#'
#' @seealso [new_npsych_covariate()] to create a covariate subclass.
#'
#' @export
npsych_covariate <- S7::new_class(
  "npsych_covariate",
  parent = S7::class_double,
  properties = list(
    label = S7::class_character,
    short_descriptor = S7::class_character,
    range = S7::class_double,
    values = S7::class_double,
    codes = S7::class_double
  ),
  validator = function(self) {
    x <- S7::S7_data(self)

    label <- self@label
    short_descriptor <- self@short_descriptor
    range <- self@range
    values <- self@values
    codes <- self@codes

    errs <- character()

    if (!is.character(label) || length(label) != 1) {
      errs <- c(errs, cli::format_inline("{.arg label} must be a string."))
    }

    if (!is.character(short_descriptor) || length(short_descriptor) > 1) {
      errs <- c(
        errs,
        cli::format_inline("{.arg short_descriptor} must be a string.")
      )
    }

    if (!length(range) %in% c(0, 2)) {
      errs <- c(
        errs,
        cli::format_inline(
          "{.arg range} must be empty or a numeric vector of length 2."
        )
      )
    }

    if (length(range) == 0 && length(values) == 0) {
      errs <- c(
        errs,
        cli::format_inline("One of {.arg range} or {.arg values} must be set.")
      )
    }

    if (length(codes) > 0 && is.null(names(codes))) {
      errs <- c(
        errs,
        cli::format_inline("{.arg codes} must be a named numeric vector.")
      )
    }

    if (length(errs) > 0) {
      return(errs)
    }

    in_range <- if (length(range) == 2) {
      x >= range[1] & x <= range[2]
    } else {
      FALSE
    }

    ok <- is.na(x) | in_range | x %in% values | x %in% codes

    if (!all(ok)) {
      invalid <- unique(x[!ok])

      allowed <- c(
        if (length(range) == 2) {
          paste0(range[1], "\u2013", range[2])
        },
        values,
        if (length(codes) > 0) {
          paste0(codes, " (", names(codes), ")")
        }
      )

      errs <- c(
        errs,
        paste0(
          cli::format_inline("{.arg {label}} contains "),
          cli::format_inline(
            "invalid {cli::qty(length(invalid))}value{?s} {.val {invalid}}. "
          ),
          cli::format_inline("Allowed: {allowed}.")
        )
      )
    }

    if (length(errs) > 0) {
      return(errs)
    }

    NULL
  }
)


#' Subset `npsych_covariate` objects.
#'
#' @param x An S7 object of class `npsych_covariate`.
#' @param i A numeric index.
#'
#' @returns
#' A new `npsych_covariate` object containing the elements of `x` specified by
#' `i`.
#'
#' @name npsych_covariate-subset
#'
#' @keywords internal
S7::method(`[`, npsych_covariate) <- function(x, i) {
  S7::S7_data(x) <- as.numeric(x)[i]
  x
}


#' Subset replacement for `npsych_covariate` objects
#'
#' @param x An `npsych_covariate` object.
#' @param i A numeric, logical, or character index.
#' @param value A numeric value.
#'
#' @returns
#' The modified `npsych_covariate` object `x`. Errors if `value` is not valid
#' for the covariate.
#'
#' @name npsych_covariate-subset
#'
#' @keywords internal
`[<-.npsych_covariate` <- function(x, i, value) {
  data <- S7::S7_data(x)
  data[i] <- as.numeric(value)
  S7::S7_data(x) <- data
  S7::validate(x)
  x
}

#' Combine `npsych_covariate` objects
#'
#' @param x An `npsych_covariate` object.
#' @param ... Additional `npsych_covariate` objects to combine.
#'
#' @returns
#' An `npsych_covariate` object containing the combined data from `x` and
#' `...`. The function will error if different subclasses of
#' `npsych_covariate` are provided.
#'
#' @keywords internal
c.npsych_covariate <- function(x, ...) {
  all_args <- list(x, ...)

  classes <- vapply(all_args, \(a) S7::S7_class(a)@name, character(1))

  if (length(unique(classes)) > 1L) {
    cli::cli_abort(
      "Cannot combine different {.cls npsych_covariate} subclasses: {.val {unique(classes)}}."
    )
  }

  combined_data <- unlist(lapply(all_args, S7::S7_data))
  constructor <- S7::S7_class(x)
  constructor(combined_data)
}


#' Valid values of a covariate
#'
#' @description
#' Returns the values of an [npsych_covariate] as a plain numeric vector, with
#' every code for unknown or unavailable data (see the `codes` property)
#' replaced by `NA`.
#'
#' @param x An `npsych_covariate` object.
#'
#' @returns A numeric vector the same length as `x`.
#'
#' @examples
#' valid_values(EDUC(c(12, 16, 99, NA)))
#' valid_values(RACE(c(1, 2, 99)))
#'
#' @export
valid_values <- function(x) {
  if (!S7::S7_inherits(x, npsych_covariate)) {
    cli::cli_abort("{.arg x} must be an {.cls npsych_covariate} object.")
  }

  out <- as.numeric(x)
  out[out %in% x@codes] <- NA

  out
}


#' Check if an object is an `npsych_covariate`
#'
#' @param x An object.
#'
#' @returns `TRUE` if `x` inherits from [npsych_covariate], `FALSE` otherwise.
#'
#' @export
is_npsych_covariate <- function(x) {
  S7::S7_inherits(x, npsych_covariate)
}
