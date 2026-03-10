#' TRAILARR Test Scores
#'
#' @description Create a `TRAILARR` object to hold TRAILARR scores.
#'
#' @param x Numeric scores.
#'
#' @returns
#' An object of class `TRAILARR`.
#'
#' @export
TRAILARR <- new_npsych_scores(
  "TRAILARR",
  label = "TRAILA Number of Errors",
  domain = "Attention/Processing",
  short_descriptor = "Trail Making Test Part A - Number of commission errors",
  range = c(0, 40),
  codes = c(
    "Physical problem" = 95,
    "Cognitive/behavior problem" = 96,
    "Other problem" = 97,
    "Verbal refusal" = 98,
    "Not available: UDS form submitted did not collect data in this way, or a skip pattern precludes response to this question" = -4
  )
)
