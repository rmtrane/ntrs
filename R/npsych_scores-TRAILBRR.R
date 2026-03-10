#' TRAILBRR Test Scores
#'
#' @description Create a `TRAILBRR` object to hold TRAILBRR scores.
#'
#' @param x Numeric scores.
#'
#' @returns
#' An object of class `TRAILBRR`.
#'
#' @export
TRAILBRR <- new_npsych_scores(
  "TRAILBRR",
  label = "TRAILB Number of Errors",
  domain = "Attention/Processing",
  short_descriptor = "Trail Making Test Part B - Number of commission errors",
  range = c(0, 40),
  codes = c(
    "Physical problem" = 95,
    "Cognitive/behavior problem" = 96,
    "Other problem" = 97,
    "Verbal refusal" = 98,
    "Not available: UDS form submitted did not collect data in this way, or a skip pattern precludes response to this question" = -4
  )
)
