#' REYFPOS Test Scores
#'
#' @description Create a `REYFPOS` object to hold REYFPOS scores.
#'
#' @param x Numeric scores.
#'
#' @returns
#' An object of class `REYFPOS`.
#'
#' @export
REYFPOS <- new_npsych_scores(
  "REYFPOS",
  label = "RAVLT (Delayed) Recognition: Tocal false positives",
  short_descriptor = "Rey Auditory Verbal Learning (Delayed) Recognition - Total false positive",
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
