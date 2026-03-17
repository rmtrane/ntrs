#' @include new_npsych_scores.R
NULL

#' UDSBENRS Test Scores
#'
#' @description Create a `UDSBENRS` object to hold UDSBENRS scores.
#'
#' @param x Numeric scores.
#'
#' @returns
#' An object of class `UDSBENRS`.
#'
#' @export
UDSBENRS <- new_npsych_scores(
  "UDSBENRS",
  label = "Benson Figure Recall",
  domain = "Visuospatial",
  short_descriptor = "Benson Complex Figure Recall - Recognized original stimulus among four options",
  range = c(0, 1),
  codes = c(
    "No" = 0,
    "Yes" = 1,
    "Not available: UDS form submitted did not collect data in this way, or a skip pattern precludes response to this question" = -4,
    "N/A (not official error code, but shows up in full NACC data)" = 9
  )
)
