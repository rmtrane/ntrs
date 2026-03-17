# Registry Environments
.std_versions <- local(new.env(parent = emptyenv()))

#' Register a standardization version
#'
#' @description
#' Stores a [std_version] object (e.g., [norms_version], [regression_version])
#' in the `.std_versions` registry, organized by method, scores class, and
#' version ID.
#'
#' @param version_obj An S7 object inheriting from [std_version]. Contains all
#'   version metadata (`scores_class`, `method_name`, `version_id`) and the
#'   method-specific data (e.g., `lookup_table` or `coefs`).
#' @param overwrite Logical. If `TRUE`, allows overwriting an existing version
#'   with a warning. If `FALSE` (default), attempting to register an existing
#'   version throws an error.
#'
#' @return Invisible `NULL`. Called for side effects (storing data in
#'   `.std_versions` environment).
#'
#' @keywords internal
.register_std_version <- function(
  version_obj,
  overwrite = FALSE
) {
  if (!S7::S7_inherits(version_obj, std_version)) {
    cli::cli_abort(c(
      "{.arg version_obj} must be an S7 object of class {.cls std_version}",
      "x" = "You supplied an object of class {.cls {class(version_obj)}}"
    ))
  }

  method <- version_obj@method_name
  scores_class <- version_obj@scores_class
  version_id <- version_obj@version_id

  # Check that method exists for scores_class
  # Check that a std_using_<method> generic exists
  generic_name <- paste0("std_using_", method)
  generic_fn <- tryCatch(get(generic_name), error = \(e) NULL)

  if (is.null(generic_fn) || !inherits(generic_fn, "S7_generic")) {
    cli::cli_abort(
      "No S7 generic {.fn {generic_name}} found. Define it before registering versions."
    )
  }

  # Check that the generic has a method for this specific class
  # class_obj <- .npsych_classes[[scores_class]]

  # if (is.null(class_obj)) {
  #   cli::cli_abort(
  #     "No registered npsych_scores subclass {.val {scores_class}}. Create it with {.fn new_npsych_test} first."
  #   )
  # }

  all_classes <- .find_npsych_classes()
  class_obj <- all_classes[[scores_class]]

  if (is.null(class_obj)) {
    cli::cli_abort(
      "No npsych_scores subclass {.val {scores_class}} found in any loaded namespace. Create it with {.fn new_npsych_scores} first."
    )
  }

  has_method <- tryCatch(
    {
      S7::method(generic_fn, class_obj)
      TRUE
    },
    error = \(e) FALSE
  )

  if (!has_method) {
    cli::cli_abort(c(
      "x" = "Method {.fn {generic_name}} not implemented for {.val {scores_class}}.",
      "i" = "Register a method with {.code S7::method({generic_name}, {scores_class}) <- function(scores, ...) {{...}}}"
    ))
  }

  # Create method environment if needed
  if (!exists(method, envir = .std_versions, inherits = FALSE)) {
    .std_versions[[method]] <- new.env(parent = emptyenv())
  }

  # Create scores_class environment if needed
  if (
    !exists(scores_class, envir = .std_versions[[method]], inherits = FALSE)
  ) {
    .std_versions[[method]][[scores_class]] <- new.env(parent = emptyenv())
  }

  # Check if version already exists
  if (
    exists(
      version_id,
      envir = .std_versions[[method]][[scores_class]],
      inherits = FALSE
    )
  ) {
    if (overwrite) {
      cli::cli_warn(c(
        "Overwriting existing version",
        "i" = "Version {.val {version}} already exists for {.val {method}} method on {.val {scores_class}}",
        "i" = "The previous version will be replaced"
      ))
    } else {
      cli::cli_abort(c(
        "Version {.val {version}} already exists for {.val {method}} method on {.val {scores_class}}. To overwrite, call with {.arg overwrite = TRUE}."
      ))
    }
  }

  # Store the S7 object directly (not a list wrapping it)
  .std_versions[[method]][[scores_class]][[version_id]] <- version_obj

  invisible()
}
