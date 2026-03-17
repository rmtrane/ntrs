#' CDRGLOB Test Scores
#'
#' @description Create a `CDRGLOB` object to hold CDRGLOB scores. 
#'
#' @param x Numeric scores.
#'
#' @returns
#' An object of class `CDRGLOB`.
#'
#' @export
CDRGLOB <- new_npsych_scores(
  "CDRGLOB",
  label = "Global CDR",
  domain = "General Cognition",
  short_descriptor = "Global CDR",
  range = c(0, 3),
  codes = c(
    "No impairment" = 0,
    "Questionable impairment" = 0.5,
    "Mild impairment" = 1,
    "Moderate impairment" = 2,
    "Severe impairment" = 3,
    "N/A (not official error code, but shows up in full NACC data)" = 99
  )
)
