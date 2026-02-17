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

#' Validate registration parameters for standardization versions
#'
#' Internal helper function that validates the `version` and `description`
#' parameters used when registering a new standardization method version.
#' This function is called by `.register_std_version()` to ensure parameter
#' validity before storing version data.
#'
#' @param version Character string identifying the version. Must be a single
#'   non-empty character string (length 1).
#' @param description Character string describing the version. Must be a single
#'   non-empty character string (length 1).
#'
#' @return Invisible `NULL` if validation passes. Throws an error if either
#'   parameter fails validation.
#'
#' @details
#' The function checks that both `version` and `description`:
#' \itemize{
#'   \item Are character vectors
#'   \item Have length exactly 1
#'   \item Are not `NULL`
#' }
#'
#' If validation fails, an error is thrown with a descriptive message
#' indicating which parameter failed and why.
#'
#' @keywords internal
.validate_registration_params <- function(version, description) {
  if (!is.character(version) || length(version) != 1 || version == "") {
    cli::cli_abort(c(
      "{.arg version} must be a non-empty character string",
      "x" = "You supplied {.cls {class(version)}} of length {length(version)}",
      "i" = "Example valid values: {.val updated}, {.val v2024}, {.val baseline}"
    ))
  }

  if (!is.character(description) || length(description) != 1) {
    cli::cli_abort(c(
      "{.arg description} must be a character string",
      "x" = "You supplied {.cls {class(description)}} of length {length(description)}"
    ))
  }

  invisible()
}

#' Check if a standardization method is implemented for a test class
#'
#' Internal S3 generic that determines whether a specific standardization
#' method has been implemented for a given test_scores class. This is used
#' by `.register_std_version()` to validate that a method exists before
#' allowing version registration.
#'
#' @param test_scores A test_scores object to check method implementation for
#' @param method Character string naming the standardization method to check
#'   (e.g., "norms", "regression"). This corresponds to a `std_using_*()`
#'   generic function.
#'
#' @return Logical. `TRUE` if the method is implemented (i.e., a
#'   `std_using_<method>()` S3 method exists for the test class), `FALSE`
#'   otherwise.
#'
#' @details
#' The function constructs a call to `std_using_<method>(test_scores)` and
#' uses `sloop::s3_dispatch()` to check if a method exists for the specific
#' test class. It searches both the current environment and all parent frames
#' to detect methods that may be defined in different contexts.
#'
#' @keywords internal
.method_implemented <- function(test_scores, method) {
  UseMethod(".method_implemented")
}

#' @describeIn .method_implemented Method for test_scores objects
#'
#' This method uses `sloop::s3_dispatch()` to check whether a
#' `std_using_<method>()` S3 method exists for the specific test class.
#' It searches through the call stack to handle cases where methods might
#' be defined in different evaluation contexts. The latter is particularly
#' important for unit tests using `testthat`.
#'
#' @exportS3Method NpsychBatteryNormsS3::.method_implemented
#'
#' @keywords internal
.method_implemented.test_scores <- function(test_scores, method) {
  # call <- rlang::parse_expr(paste0("std_using_", method, "(test_scores)"))
  # if (any(rlang::inject(sloop::s3_dispatch(!!call))$exists)) {
  #   return(TRUE)
  # }

  # nframes <- sys.nframe()

  # print(nframes)

  # for (i in seq_len(nframes)) {
  #   s3_dispatches <- rlang::inject(
  #     sloop::s3_dispatch(!!call),
  #     env = sys.frame(i - 1)
  #   )

  #   if (any(s3_dispatches$exists)) {
  #     return(TRUE)
  #   }
  # }

  # FALSE

  generic_name <- paste0("std_using_", method)
  classes <- class(test_scores)

  # Check registered S3 methods
  if (
    any(vapply(
      classes,
      function(cls) {
        !is.null(utils::getS3method(generic_name, cls, optional = TRUE))
      },
      logical(1)
    ))
  ) {
    return(TRUE)
  }

  # Fallback: search calling environments for method-style functions
  # This handles cases where methods are defined in local scopes (e.g., testthat)
  n <- sys.nframe()
  for (i in seq_len(n)) {
    env <- sys.frame(i)
    for (cls in classes) {
      method_name <- paste0(generic_name, ".", cls)
      if (exists(method_name, envir = env, inherits = FALSE)) {
        return(TRUE)
      }
    }
  }

  FALSE
}
