#' @include new_npsych_scores.R
NULL

#' REY2REC Test Scores
#'
#' @description Create a `REY2REC` object to hold REY2REC scores.
#'
#' @param x Numeric scores.
#'
#' @returns
#' An object of class `REY2REC`.
#'
#' @export
REY2REC <- new_npsych_scores(
  "REY2REC",
  label = "RAVLT (Immediate) Trial 2 Total recall",
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
