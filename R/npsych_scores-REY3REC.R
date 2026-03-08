#' @include new_npsych_scores.R
NULL

#' REY3REC Test Scores
#'
#' @description Create a `REY3REC` object to hold REY3REC scores.
#'
#' @param x Numeric scores.
#'
#' @returns
#' An object of class `REY3REC`.
#'
#' @export
REY3REC <- new_npsych_scores(
  "REY3REC",
  label = "RAVLT (Immediate) Trial 3 Total recall",
  short_descriptor = "Rey Auditory Verbal Learning (Immediate) Trial 3 Total recall",
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
