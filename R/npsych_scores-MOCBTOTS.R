#' @include new_npsych_scores.R
NULL

#' MOCBTOTS Test Scores
#'
#' @description Create a `MOCBTOTS` object to hold MoCA Blind scores.
#'
#' @param x Numeric scores.
#'
#' @returns
#' An object of class `MOCBTOTS`.
#'
#' @export
MOCBTOTS <- new_npsych_scores(
  "MOCBTOTS",
  label = "MoCA-Blind",
  domain = "General Cognition",
  short_descriptor = "MoCA-Blind Total Raw Score - uncorrected",
  range = c(0, 22),
  codes = c(
    "Not available: UDS form submitted did not collect data in this way, or a skip pattern precludes response to this question" = -4,
    "Item(s) or whole test not administered" = 88
  )
)
