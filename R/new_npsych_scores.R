# Environment to store S7 npsych_scores subclass objects.
# Populated by new_npsych_scores().
.npsych_classes <- local(new.env(parent = emptyenv()))

#' @include npsych_scores-class.R
NULL

#' Create an npsych_scores subclass
#'
#' @description
#' Factory function that creates a new S7 subclass of [npsych_scores] with
#' fixed `label`, `range`, and `codes` properties. The returned class object
#' is also registered in the `.npsych_classes` environment for discovery by
#' [list_npsych_scores()].
#'
#' @param name A non-empty single string giving the subclass name
#'   (e.g., `"MOCATOTS"`). Used as both the S7 class name and registry key.
#' @param label A single string identifying the test
#'   (e.g., `"MoCA"`, `"Trail Making Test Part A"`).
#' @param range A numeric vector of length 2 giving the minimum and maximum
#'   valid scores.
#' @param codes A named numeric vector of error/special codes
#'   (e.g., `c("Not administered" = 88)`). Defaults to an empty numeric vector.
#'
#' @returns An S7 class object (subclass of [npsych_scores]) whose constructor
#'   accepts a single `.data` argument.
#'
#' @keywords internal
new_npsych_scores <- function(name, label, range, codes = numeric()) {
  # Validate definition-time arguments
  if (!is.character(name) || length(name) != 1 || name == "") {
    cli::cli_abort("{.arg name} must be a non-empty single string.")
  }
  if (!is.character(label) || length(label) != 1) {
    cli::cli_abort("{.arg label} must be a single string.")
  }
  if (!is.numeric(range) || length(range) != 2) {
    cli::cli_abort("{.arg range} must be a numeric vector of length 2.")
  }
  if (length(codes) > 0 && is.null(names(codes))) {
    cli::cli_abort("{.arg codes} must be named when non-empty.")
  }

  cls <- S7::new_class(
    name,
    parent = npsych_scores,
    constructor = function(x = numeric()) {
      if (is.integer(x)) {
        x <- as.double(x)
      }

      obj <- S7::new_object(
        x,
        label = label,
        range = range,
        codes = codes
      )
      S7::validate(obj)
      obj
    }
  )

  # Register in the class registry
  .npsych_classes[[name]] <- cls

  cls
}
