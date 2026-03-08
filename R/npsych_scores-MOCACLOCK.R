#' MOCACLOCK Test Scores
#'
#' @description Create a `MOCACLOCK` object to hold MOCACLOCK scores.
#'
#' @param x Numeric scores.
#'
#' @returns
#' An object of class `MOCACLOCK`.
#'
#' @export
MOCACLOCK <- new_npsych_scores(
  "MOCACLOCK",
  label = "Clock Drawing Test",
  domain = "Executive Functioning",
  short_descriptor = "Sum of MOCACLOC, MOCACLON, and MOCACLOH (not in NACC data)",
  range = c(0, 3)
)
