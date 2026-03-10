#' TRAILALI Test Scores
#'
#' @description Create a `TRAILALI` object to hold TRAILALI scores.
#'
#' @param x Numeric scores.
#'
#' @returns
#' An object of class `TRAILALI`.
#'
#' @export
TRAILALI <- new_npsych_scores(
  "TRAILALI",
  label = "TRAILA Number of correct lines",
  domain = "Attention/Processing",
  short_descriptor = "Trail Making Test Part A - Number of correct lines",
  range = c(0, 24),
  codes = c(
    "Physical problem" = 95,
    "Cognitive/behavior problem" = 96,
    "Other problem" = 97,
    "Verbal refusal" = 98,
    "Not available: UDS form submitted did not collect data in this way, or a skip pattern precludes response to this question" = -4
  )
)
