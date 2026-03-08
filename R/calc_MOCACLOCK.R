#' Calculate MoCA clock drawing test total
#'
#' @description
#' Sum of MOCACLOC, MOCACLON, and MOCACLOH, but applies removes any values not 0 or 1.
#'
#' @param MOCACLOC values for clock contour
#' @param MOCACLON values for clock numbers
#' @param MOCACLOH values for clock hands
#'
#' @export
calc_MOCACLOCK <- function(
  MOCACLOC,
  MOCACLON,
  MOCACLOH
) {
  if (!all(MOCACLOC %in% c(0, 1, ntrs::rdd$MOCACLOC$codes))) {
    cli::cli_abort(
      "{.arg MOCACLOC} must be one of {.val {c(0,1, ntrs::rdd$MOCACLOC$codes)}}"
    )
  }

  MOCACLOC[!MOCACLOC %in% c(0, 1)] <- NA

  if (!all(MOCACLON %in% c(0, 1, ntrs::rdd$MOCACLON$codes))) {
    cli::cli_abort(
      "{.arg MOCACLON} must be one of {.val {c(0,1, ntrs::rdd$MOCACLON$codes)}}"
    )
  }

  MOCACLON[!MOCACLON %in% c(0, 1)] <- NA

  if (!all(MOCACLOH %in% c(0, 1, ntrs::rdd$MOCACLOH$codes))) {
    cli::cli_abort(
      "{.arg MOCACLOH} must be one of {.val {c(0,1, ntrs::rdd$MOCACLOH$codes)}}"
    )
  }

  MOCACLOH[!MOCACLOH %in% c(0, 1)] <- NA

  MOCACLOCK(MOCACLOC + MOCACLON + MOCACLOH)
}
