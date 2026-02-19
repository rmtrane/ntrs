# Registry Environments
.std_versions <- new.env(parent = emptyenv())

#' Register a standardization method version for a test score class
#'
#' Internal S3 generic for registering versions of standardization methods
#' (e.g., norms, regression models) for specific test score classes. This
#' function stores the standardization data in a nested environment structure
#' organized by method, test class, and version.
#'
#' @param test_class A test_scores object whose class determines which
#'   registration method to use
#' @param method Character string naming the standardization method (e.g.,
#'   "norms", "regression"). Must correspond to an implemented `std_using_*()`
#'   generic function.
#' @param version Character string identifying this version of the method
#'   (e.g., "updated", "nacc", "v2024"). Used to retrieve this specific
#'   standardization data later.
#' @param data A list containing the standardization data (e.g., norm tables,
#'   regression coefficients, lookup tables). Structure depends on the method.
#' @param description Character string describing this version (e.g., "Updated
#'   norms from 2024 cohort"). Used for documentation and user information.
#' @param overwrite Logical. If `TRUE`, allows overwriting an existing version.
#'   If `FALSE` (default), attempting to register an existing version throws an
#'   error.
#'
#' @return Invisible `NULL`. Called for side effects (storing data in
#'   `.std_versions` environment).
#'
#' @keywords internal
.register_std_version <- function(
  test_class,
  method,
  version,
  data,
  description = "",
  overwrite = FALSE
) {
  UseMethod(".register_std_version")
}

#' @describeIn .register_std_version Method for test_scores objects
#'
#' This method validates that the specified standardization method is
#' implemented for the test class, creates the necessary nested environment
#' structure if needed, and stores the version data with metadata.
#'
#' The function stores data in a three-level nested environment:
#' `.std_versions[[method]][[test_class]][[version]]`, where each version
#' entry contains:
#' \itemize{
#'   \item `test_class` - The specific test class name
#'   \item `method` - The standardization method name
#'   \item `version` - The version identifier
#'   \item `data` - The standardization data
#'   \item `description` - Version description
#' }
#'
#' @exportS3Method NpsychBatteryNormsS3::.register_std_version
#'
#' @keywords internal
.register_std_version.test_scores <- function(
  test_class,
  method,
  version,
  data,
  description,
  overwrite = FALSE
) {
  # Check that method exists for test_class
  if (!.method_implemented(test_class, method)) {
    cli::cli_abort(c(
      "x" = "Method {.val {method}} not implemented for {.value {test_class}}."
    ))
  }

  test_class <- setdiff(class(test_class), "test_scores")

  # Create method environment if needed
  if (!exists(method, envir = .std_versions, inherits = FALSE)) {
    .std_versions[[method]] <- new.env(parent = emptyenv())
  }

  # Create test_class environment if needed
  if (!exists(test_class, envir = .std_versions[[method]], inherits = FALSE)) {
    .std_versions[[method]][[test_class]] <- new.env(parent = emptyenv())
  }

  # Check if version already exists
  if (
    exists(
      version,
      envir = .std_versions[[method]][[test_class]],
      inherits = FALSE
    )
  ) {
    if (overwrite) {
      cli::cli_warn(c(
        "Overwriting existing version",
        "i" = "Version {.val {version}} already exists for {.val {method}} method on {.val {test_class}}",
        "i" = "The previous version will be replaced"
      ))
    } else {
      cli::cli_abort(c(
        "Version {.val {version}} already exists for {.val {method}} method on {.val {test_class}}. To overwrite, call with {.arg overwrite = TRUE}."
      ))
    }
  }

  # Store version
  .std_versions[[method]][[test_class]][[version]] <- list(
    test_class = test_class,
    method = method,
    version = version,
    data = data,
    description = description
  )

  invisible()
}
