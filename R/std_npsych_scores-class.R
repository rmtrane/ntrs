#' Standardized neuropsychological test scores
#'
#' An S7 class representing the result of standardizing an `npsych_scores`
#' vector via [std()], [std_using_norms()], or [std_using_regression()].
#' Inherits from `S7::class_double`, so it behaves as a numeric vector while
#' carrying standardization metadata as S7 properties.
#'
#' @param .data A numeric vector of standardized (z-score) values.
#' @param scores_subclass A string naming the originating `npsych_scores`
#'   subclass (e.g., `"MOCATOTS"`), or `NULL`.
#' @param description A non-empty string summarizing the standardization
#'   applied, including the method, version, and covariates.
#' @param method A non-empty string indicating the standardization method
#'   (e.g., `"norms"`, `"regression"`).
#' @param version A string identifying the version used (e.g., `"nacc"`,
#'   `"updated_2025.06"`), or `NULL`.
#'
#' @returns An S7 object of class `std_npsych_scores`, which behaves as a
#'   numeric vector with additional properties: `scores_subclass`,
#'   `description`, `method`, and `version`.
#'
#' @section Properties:
#' \describe{
#'   \item{`scores_subclass`}{(`character(1)` or `NULL`) The name of the
#'     originating `npsych_scores` subclass (e.g., `"MOCATOTS"`).}
#'   \item{`description`}{(`character(1)`) A human-readable summary of the
#'     standardization applied, including the method, version, and covariates.}
#'   \item{`method`}{(`character(1)`) The standardization method used
#'     (e.g., `"norms"`, `"regression"`).}
#'   \item{`version`}{(`character(1)` or `NULL`) The version identifier
#'     (e.g., `"nacc"`, `"updated_2025.06"`).}
#' }
#'
#' @seealso [std()], [std_using_norms()], [std_using_regression()],
#'   [std_data()]
#'
#' @export
std_npsych_scores <- S7::new_class(
  "std_npsych_scores",
  parent = S7::class_double,
  properties = list(
    scores_subclass = S7::class_character | NULL,
    description = S7::class_character,
    method = S7::class_character,
    version = S7::class_character | NULL
  ),
  validator = function(self) {
    scores <- S7::S7_data(self)

    scores_subclass <- self@scores_subclass
    description <- self@description
    method <- self@method
    version <- self@version

    errs <- character()

    if (
      !is.null(scores_subclass) &&
        length(scores_subclass) > 0 &&
        (!is.character(scores_subclass) ||
          length(scores_subclass) > 1)
    ) {
      errs <- c(
        errs,
        cli::format_inline("{.arg scores_subclass} must be a string or NULL.")
      )
    }

    if (
      length(scores_subclass) == 1 && !scores_subclass %in% list_npsych_scores()
    ) {
      errs <- c(
        errs,
        cli::format_inline(
          "{.arg scores_subclass} must be the name of a {.cls npsych_scores} subclass. See {.fun list_npsych_scores()}."
        )
      )
    }

    if (
      !is.character(description) ||
        length(description) > 1 ||
        description == ""
    ) {
      errs <- c(
        errs,
        cli::format_inline("{.arg description} must be a non-empty string.")
      )
    }

    if (!is.character(method) || length(method) != 1 || method == "") {
      errs <- c(
        errs,
        cli::format_inline("{.arg method} must be a non-empty string.")
      )
    }

    if (
      !is.null(version) &&
        length(version) > 0 &&
        (!is.character(version) || length(version) > 1 || version == "")
    ) {
      errs <- c(
        errs,
        cli::format_inline("{.arg version} must be a non-empty string or NULL.")
      )
    }

    if (length(errs) > 0) {
      return(errs)
    }

    NULL
  }
)


#' Subset `npsych_scores` objects.
#'
#' @param x An S7 object of class `npsych_scores`.
#' @param i A numeric index.
#'
#' @returns
#' A new `npsych_scores` object containing the elements of `x` specified by `i`.
#'
#' @keywords internal
#'
#' @name `[`
S7::method(`[`, std_npsych_scores) <- function(x, i) {
  S7::S7_data(x) <- as.numeric(x)[i]

  x
}


#' @export
`[<-.std_npsych_scores` <- function(x, i, value) {
  data <- S7::S7_data(x)
  data[i] <- as.numeric(value)
  S7::S7_data(x) <- data
  x
}

#' @export
c.std_npsych_scores <- function(x, ...) {
  all_args <- list(x, ...)

  # Verify all args are std_npsych_scores with same method/version/description
  # (or decide on a policy for mixed metadata)
  scores_subclasses <- unique(unlist(lapply(
    all_args,
    \(a) a@scores_subclass
  )))

  methods <- unique(unlist(lapply(all_args, \(a) a@method)))
  versions <- unique(unlist(lapply(all_args, \(a) a@version)))
  descriptions <- unique(unlist(lapply(all_args, \(a) a@description)))

  if (
    length(methods) > 1L ||
      length(versions) > 1L ||
      length(scores_subclasses) > 1L ||
      length(descriptions) > 1L
  ) {
    cli::cli_abort(
      "Cannot combine {.cls std_npsych_scores} with different method/version/description/scores_subclass."
    )
  }

  combined_data <- unlist(lapply(all_args, S7::S7_data))

  std_npsych_scores(
    combined_data,
    scores_subclass = scores_subclasses,
    method = methods,
    version = versions,
    description = descriptions
  )
}
