#' Get an npsych_scores constructor by name
#'
#' @param name A single string, e.g. "REYTOTAL".
#' @returns The S7 class object (which is callable as a constructor).
#' @export
get_npsych_scores <- function(name) {
  classes <- .find_npsych_classes()
  cls <- classes[[name]]

  if (is.null(cls)) {
    cli::cli_abort(
      "No {.cls npsych_scores} subclass {.val {name}} found in any loaded namespace."
    )
  }

  cls
}
