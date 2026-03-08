#' @include new_npsych_scores.R
NULL

#' REY1REC Test Scores
#'
#' @description Create a `REY1REC` object to hold REY1REC scores.
#'
#' @param x Numeric scores.
#'
#' @returns
#' An object of class `REY1REC`.
#'
#' @export
REY1REC <- new_npsych_scores(
  "REY1REC",
  label = "RAVLT (Immediate) Trial 1 Total recall",
  range = c(0, 15),
  codes = c(
    "Not assessed, optional" = 88,
    "Physical problem" = 95,
    "Cognitive/behavior problem" = 96,
    "Other problem" = 97,
    "Verbal refusal" = 98,
    "Not available: UDS form submitted did not collect data in this way, or a skip pattern precludes response to this question" = -4
  )
)
