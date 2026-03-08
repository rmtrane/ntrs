#' Calculate the Rey AVLT Accuracy
#'
#' Calculates accuracy from true positives and false positives, but applies `valid_values_only()`
#' first. As percentage, i.e. a value of 55 means 55%.
#'
#' @param REYTCOR true positives
#' @param REYFPOS false positives
#'
#' @export
calc_REYAREC <- function(
  reytcor,
  reyfpos
) {
  if (!S7::S7_inherits(reytcor, ntrs::REYTCOR)) {
    cli::cli_abort(
      "{.arg reytcor} must be of class {.cls ntrs::REYTCOR}, but is {.cls {class(reytcor)}}"
    )
  }

  if (!S7::S7_inherits(reyfpos, ntrs::REYFPOS)) {
    cli::cli_abort(
      "{.arg reyfpos} must be of class {.cls ntrs::REYTCOR}, but is {.cls {class(reyfpos)}}"
    )
  }

  ntrs::REYAREC(
    (ntrs::remove_error_codes(reytcor) +
      15 -
      ntrs::remove_error_codes(reyfpos)) *
      100 /
      30
  )
}
