#' CDRSUM Test Scores
#'
#' @description Create a `CDRSUM` object to hold CDRSUM scores.
#'
#' @param x Numeric scores.
#'
#' @returns
#' An object of class `CDRSUM`.
#'
#' @export
CDRSUM <- new_npsych_scores(
  "CDRSUM",
  label = "CDR sum of boxes",
  domain = "General Cognition",
  short_descriptor = "Standard CDR sum of boxes",
  range = c(0, 18),
  codes = c(
    "N/A (not official error code, but shows up in full NACC data)" = 99
  )
)
