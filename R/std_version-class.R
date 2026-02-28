#' Standardization Version
#'
#' @description
#' Create a `std_version` object that identifies a specific standardization
#' version for a neuropsychological test. This is the base class for
#' method-specific versions (e.g., norms-based or regression-based).
#'
#' @param scores_class A single non-empty string giving the `npsych_scores`
#'   subclass this version applies to (e.g., `"MOCATOTS"`).
#' @param method_name A single non-empty string identifying the standardization
#'   method (e.g., `"norms"`, `"regression"`).
#' @param version_id A single non-empty string uniquely identifying this
#'   version within its method and scores class.
#' @param description An optional single string describing the version.
#'
#' @returns
#' An S7 object of class `std_version` with properties `scores_class`,
#' `method_name`, `version_id`, and `description`.
#'
#' @export
std_version <- S7::new_class(
  "std_version",
  properties = list(
    scores_class = S7::class_character,
    method_name = S7::class_character,
    version_id = S7::class_character,
    description = S7::class_character
  ),
  validator = function(self) {
    errs <- character()
    if (length(self@scores_class) != 1 || self@scores_class == "") {
      errs <- c(errs, "scores_class must be a non-empty single string")
    }
    if (length(self@method_name) != 1 || self@method_name == "") {
      errs <- c(errs, "method_name must be a non-empty single string")
    }
    if (length(self@version_id) != 1 || self@version_id == "") {
      errs <- c(errs, "version_id must be a non-empty single string")
    }
    if (length(self@description) > 1) {
      errs <- c(errs, "description must empty or a single string")
    }
    if (length(errs)) errs else NULL
  }
)
