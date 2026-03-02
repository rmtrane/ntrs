#' Standardize neuropsychological test scores
#'
#' @description
#' User-facing wrapper that dispatches to the appropriate `std_using_*()`
#' method for an `npsych_scores` object. When `method` and `version` are
#' omitted, the registered defaults (see [set_std_defaults()]) are used.
#'
#' @param scores An `npsych_scores` object, such as `MOCATOTS(c(25, 28))`.
#' @param ... Named covariates passed through to the underlying
#'   `std_using_*()` method (e.g., `age`, `sex`, `educ`).
#' @param method A single string naming the standardization method (e.g.,
#'   `"norms"`, `"regression"`). When `NULL` (the default), the default
#'   method registered via [set_std_defaults()] is used.
#' @param version A single string identifying the version of the method
#'   (e.g., `"nacc"`, `"updated_2025.06"`). When `NULL` (the default), the
#'   default version registered via [set_std_defaults()] is used. Ignored
#'   when the resolved method does not use versioned data.
#'
#'
#' @returns A numeric vector of standardized scores.
#'
#' @seealso [std_data()] for batch-standardizing every `npsych_scores`
#'   column in a data frame, [list_std_methods()], [list_method_versions()],
#'   [get_std_defaults()], [set_std_defaults()].
#'
#' @examples
#' \dontrun{
#' # Using registered defaults
#' std(MOCATOTS(c(25, 28)), age = 72, sex = 1, educ = 16)
#'
#' # Specifying method and version explicitly
#' std(MOCATOTS(c(25, 28)), method = "norms", version = "nacc",
#'     age = 72, sex = "m", educ = 16)
#' }
#'
#' @importFrom data.table .SD :=
#'
#' @export
std <- function(
  scores,
  ...,
  method = NULL,
  version = NULL
) {
  if (!S7::S7_inherits(scores, npsych_scores)) {
    cli::cli_abort(c(
      "x" = "{.arg scores} must be an {.cls npsych_scores} object.",
      "i" = "Use a constructor like {.fun MOCATOTS()} to create one."
    ))
  }

  # Resolve defaults when method/version are NULL
  if (is.null(method)) {
    defaults <- get_std_defaults(scores)

    if (is.null(defaults)) {
      scores_class <- S7::S7_class(scores)@name

      cli::cli_abort(c(
        "x" = "No default method registered for {.cls {scores_class}}.",
        "i" = "Register a default via {.fun set_std_defaults()}."
      ))
    }

    method <- defaults$method

    if (is.null(version) && "version" %in% names(defaults)) {
      version <- defaults$version
    }
  }

  # Build the generic name
  generic_name <- paste0("std_using_", method)

  fn <- tryCatch(match.fun(generic_name), error = function(e) NULL)

  if (is.null(fn)) {
    scores_class <- setdiff(class(scores), "npsych_scores")
    cli::cli_abort(c(
      "x" = "Method {.val {method}} is not available for {.cls {scores_class}}.",
      "i" = "Available methods: {.val {list_std_methods(scores)}}."
    ))
  }

  # Dispatch — pass version only when non-NULL
  if (!is.null(version)) {
    fn(scores, ..., version = version)
  } else {
    fn(scores, ...)
  }
}
