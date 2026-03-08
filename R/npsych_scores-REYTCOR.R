#' REYTCOR Test Scores
#'
#' @description Create a `REYTCOR` object to hold REYTCOR scores.
#'
#' @param x Numeric scores.
#'
#' @returns
#' An object of class `REYTCOR`.
#'
#' @export
REYTCOR <- new_npsych_scores(
  "REYTCOR",
  label = "RAVLT (Delayed) Recognition: Tocal Correct",
  short_descriptor = "Rey Auditory Verbal Learning (Delayed) Recognition - Total correct",
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
