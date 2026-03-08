#' @include new_npsych_scores.R
NULL

#' REY5REC Test Scores
#'
#' @description Create a `REY5REC` object to hold REY5REC scores.
#'
#' @param x Numeric scores.
#'
#' @returns
#' An object of class `REY5REC`.
#'
#' @export
REY5REC <- new_npsych_scores(
  "REY5REC",
  label = "RAVLT (Immediate) Trial 5 Total recall",
  short_descriptor = "Rey Auditory Verbal Learning (Immediate) Trial 5 Total recall",
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
