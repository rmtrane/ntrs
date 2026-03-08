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
  range = c(0, 3)
)
