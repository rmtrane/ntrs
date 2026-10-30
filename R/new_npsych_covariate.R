#' @include npsych_covariate-class.R
NULL

#' Create an npsych_covariate subclass
#'
#' @description
#' Factory function that creates a new S7 subclass of [npsych_covariate] with
#' fixed `label`, `short_descriptor`, `range`, `values`, and `codes`
#' properties.
#'
#' @param name A non-empty single string giving the subclass name
#'   (e.g., `"EDUC"`).
#' @inheritParams npsych_covariate
#'
#' @returns An S7 class object (subclass of [npsych_covariate]) whose
#'   constructor accepts a single argument `x`. The constructor errors on
#'   values that are neither valid values nor codes.
#'
#' @export
new_npsych_covariate <- function(
  name,
  label,
  short_descriptor = NA_character_,
  range = numeric(),
  values = numeric(),
  codes = numeric()
) {
  # Validate definition-time arguments
  if (!is.character(name) || length(name) != 1 || name == "") {
    cli::cli_abort("{.arg name} must be a non-empty single string.")
  }
  if (!is.character(label) || length(label) != 1) {
    cli::cli_abort("{.arg label} must be a single string.")
  }
  if (!is.numeric(range) || !length(range) %in% c(0, 2)) {
    cli::cli_abort(
      "{.arg range} must be empty or a numeric vector of length 2."
    )
  }
  if (!is.numeric(values)) {
    cli::cli_abort("{.arg values} must be a numeric vector.")
  }
  if (length(range) == 0 && length(values) == 0) {
    cli::cli_abort("One of {.arg range} or {.arg values} must be set.")
  }
  if (length(codes) > 0 && is.null(names(codes))) {
    cli::cli_abort("{.arg codes} must be named when non-empty.")
  }

  overlap <- codes[
    codes %in% values |
      (length(range) == 2 & codes >= range[1] & codes <= range[2])
  ]
  if (length(overlap) > 0) {
    cli::cli_abort(
      "{.arg codes} must not overlap the valid values, but {.val {overlap}} {?does/do}."
    )
  }

  S7::new_class(
    name,
    parent = npsych_covariate,
    constructor = function(x = numeric()) {
      if (S7::S7_inherits(x, npsych_covariate)) {
        x <- as.numeric(x)
      }

      # Allow integers and all-NA logicals, e.g. RACE(NA)
      if (is.integer(x) || (is.logical(x) && all(is.na(x)))) {
        x <- as.double(x)
      }

      obj <- S7::new_object(
        x,
        label = label,
        short_descriptor = short_descriptor,
        range = as.double(range),
        values = as.double(values) |> stats::setNames(names(values)),
        codes = codes
      )
      S7::validate(obj)
      obj
    }
  )
}
