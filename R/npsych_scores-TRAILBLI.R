#' TRAILBLI Test Scores
#'
#' @description Create a `TRAILBLI` object to hold TRAILBLI scores.
#'
#' @param x Numeric scores.
#'
#' @returns
#' An object of class `TRAILBLI`.
#'
#' @export
TRAILBLI <- new_npsych_scores(
  "TRAILBLI",
  label = "TRAILB Number of correct lines",
  domain = "Attention/Processing",
  short_descriptor = "Trail Making Test Part B - Number of correct lines",
  range = c(0, 24),
  codes = c(
    "Physical problem" = 95,
    "Cognitive/behavior problem" = 96,
    "Other problem" = 97,
    "Verbal refusal" = 98,
    "Not available: UDS form submitted did not collect data in this way, or a skip pattern precludes response to this question" = -4
  )
)
