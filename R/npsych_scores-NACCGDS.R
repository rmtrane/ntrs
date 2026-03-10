#' @include new_npsych_scores.R
NULL

#' NACCGDS Test Scores
#'
#' @description Create a `NACCGDS` object to hold Total GDS Scores.
#'
#' @param x Numeric scores.
#'
#' @returns
#' An object of class `NACCGDS`.
#'
#' @export
NACCGDS <- new_npsych_scores(
  "NACCGDS",
  label = "Total GDS Score",
  domain = "Mood",
  short_descriptor = "Total GDS Score",
  range = c(0, 15),
  codes = c(
    "Not available: UDS form submitted did not collect data in this way, or a skip pattern precludes response to this question" = -4,
    "Could not be calculated" = 88
  )
)
