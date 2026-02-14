# Registry Environments
.std_versions <- new.env(parent = emptyenv())

#' Get test score classes
#'
#' @description
#' Get all test score classes available with some method/version
#'
#' @returns
#' A character vector of the names of test score classes.
#'
#' @keywords internal
get_test_scores_classes <- function() {
  unique(unlist(lapply(.std_versions, \(x) ls(envir = x))))
}

#' Register a version
#'
#' @description
#' A short description...
#'
#' @param method A single string. Must be one of the implemented methods. See `get_`
#' @param test_class A single string.
#' @param version A single string.
#' @param data Data.
#' @param description A single string. Optional.
#' @param overwrite A logical value. Optional.
#'
#' @returns
#' Returns `NULL`, invisibly. The function will raise an error if `method` is not
#' implemented for `test_class` or if `version` already exists and `overwrite` is `FALSE`.
#' A warning will be issued if `version` exists and `overwrite` is `TRUE`.
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
    description = description,
    registered_at = Sys.time()
  )

  invisible()
}

#' Internal function to validate common registration parameters
#' @noRd
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

#' @export
.method_implemented <- function(test_scores, method) {
  UseMethod(".method_implemented")
}

#' @export
.method_implemented.test_scores <- function(test_scores, method) {
  call <- rlang::parse_expr(paste0("std_using_", method, "(test_scores)"))
  s3_dispatches <- rlang::inject(sloop::s3_dispatch(!!call))

  any(s3_dispatches$exists)
}

# .method_implemented <- function(test_class, method) {
#   generic_name <- paste0("std_using_", method)
#   func_name <- paste0(generic_name, ".", test_class)

#   # Check if the function exists in the namespace.
#   # This works during .onLoad() because the namespace environment
#   # is already populated with function definitions — it's the
#   # S3 methods *table* that hasn't been built yet.

#   # ns <- asNamespace("NpsychBatteryNormsS3")
#   ns <- tryCatch(
#     asNamespace("NpsychBatteryNormsS3"),
#     error = function(e) parent.env()
#   )

#   if (exists(func_name, envir = ns, mode = "function", inherits = FALSE)) {
#     return(TRUE)
#   }

#   # Check the S3 methods table (user extensions, other packages)
#   s3_method <- utils::getS3method(
#     f = generic_name,
#     class = test_class,
#     optional = TRUE,
#   )

#   if (!is.null(s3_method)) {
#     return(TRUE)
#   }

#   # Check user-supplied environment (for testing purposes)
#   # if (
#   #   !is.null(env) &&
#   #     exists(func_name, envir = env, mode = "function", inherits = FALSE)
#   # ) {
#   #   return(TRUE)
#   # }

#   for (i in seq_len(sys.nframe())) {
#     frame <- sys.frame(i)
#     if (exists(func_name, envir = frame, mode = "function", inherits = FALSE)) {
#       return(TRUE)
#     }
#   }

#   if (
#     test_class != "test_scores" &&
#       .method_implemented(test_class = "test_scores", method)
#   ) {
#     cli::cli_inform(
#       "No specific {.val {method}} method implemented for {.cls {test_class}}. Make sure version specification is compatible with the standard implementation of {.val method} for a generic {.cls test_scores} object."
#     )

#     return(TRUE)
#   }

#   FALSE
# }
