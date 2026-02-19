#' Check if a standardization method is implemented for a test class
#'
#' Internal S3 generic that determines whether a specific standardization
#' method has been implemented for a given npsych_scores class. This is used
#' by `.register_std_version()` to validate that a method exists before
#' allowing version registration.
#'
#' @param scores A npsych_scores object to check method implementation for
#' @param method Character string naming the standardization method to check
#'   (e.g., "norms", "regression"). This corresponds to a `std_using_*()`
#'   generic function.
#'
#' @return Logical. `TRUE` if the method is implemented (i.e., a
#'   `std_using_<method>()` S3 method exists for the test class), `FALSE`
#'   otherwise.
#'
#' @details
#' The function constructs a call to `std_using_<method>(scores)` and
#' uses `sloop::s3_dispatch()` to check if a method exists for the specific
#' test class. It searches both the current environment and all parent frames
#' to detect methods that may be defined in different contexts.
#'
#' @keywords internal
.method_implemented <- function(scores, method) {
  UseMethod(".method_implemented")
}

#' Method for `npsych_scores` objects
#'
#' This method uses `utils::getS3method()` and `exists(..., envir = .GlobalEnv)` to check whether a
#' `std_using_<method>()` S3 method exists for the specific test class.
#'
#' @exportS3Method NpsychBatteryNormsS3::.method_implemented
#'
#' @rdname .method_implemented
#'
#' @keywords internal
.method_implemented.npsych_scores <- function(scores, method) {
  generic_name <- paste0("std_using_", method)
  classes <- class(scores)

  any(vapply(
    classes,
    function(cls) {
      # Check registered S3 methods
      !is.null(utils::getS3method(generic_name, cls, optional = TRUE)) ||
        # Check for methods defined in global environment
        exists(
          paste0(generic_name, ".", cls),
          envir = .GlobalEnv,
          inherits = FALSE
        )
    },
    logical(1)
  ))
}
