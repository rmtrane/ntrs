#' Calculate Rey Total
#'
#' @description
#' A short description...
#'
#' @param rey1rec A `REY1REC` object, or a numeric vector.
#' @param rey2rec A `REY2REC` object, or a numeric vector.
#' @param rey3rec A `REY3REC` object, or a numeric vector.
#' @param rey4rec A `REY4REC` object, or a numeric vector.
#' @param rey5rec A `REY5REC` object, or a numeric vector.
#'
#' @returns
#' A `REYTOTAL` object representing the sum of the provided `rey` records
#' after error codes have been removed.
#'
#' @export
calc_REYTOTAL <- function(
  rey1rec,
  rey2rec,
  rey3rec,
  rey4rec,
  rey5rec
) {
  if (!S7::S7_inherits(rey1rec, ntrs::REY1REC)) {
    rey1rec <- ntrs::REY1REC(rey1rec)
  }
  if (!S7::S7_inherits(rey2rec, ntrs::REY2REC)) {
    rey2rec <- ntrs::REY2REC(rey2rec)
  }
  if (!S7::S7_inherits(rey3rec, ntrs::REY3REC)) {
    rey3rec <- ntrs::REY3REC(rey3rec)
  }
  if (!S7::S7_inherits(rey4rec, ntrs::REY4REC)) {
    rey4rec <- ntrs::REY4REC(rey4rec)
  }
  if (!S7::S7_inherits(rey5rec, ntrs::REY5REC)) {
    rey5rec <- ntrs::REY5REC(rey5rec)
  }

  reytotal <- ntrs::remove_error_codes(rey1rec) +
    ntrs::remove_error_codes(rey2rec) +
    ntrs::remove_error_codes(rey3rec) +
    ntrs::remove_error_codes(rey4rec) +
    ntrs::remove_error_codes(rey5rec)

  ntrs::REYTOTAL(reytotal)
}
