#' Extract standardization method and version from `std_data()` output
#'
#' Reads the `method` and `version` properties from the [std_npsych_scores]
#' columns produced by [std_data()], and uses the `prefix_std` attribute on
#' `dat` to strip the prefix from column names.
#'
#' @param dat A `data.frame` or `data.table` returned by [std_data()].
#'   Must carry a `prefix_std` attribute (set automatically by `std_data()`).
#' @param std_cols Optional character vector of standardized column names to
#'   inspect. If omitted, all [std_npsych_scores] columns are used.
#'
#' @returns A named list, one element per standardized column. Each element is
#'   a character vector with `method` and `version` entries. Names correspond
#'   to score column names with the `prefix_std` stripped.
#'
#' @seealso [std_data()], [std()], [std_npsych_scores]
#'
#' @export
methods_from_std_data <- function(dat, std_cols = NULL) {
  if (data.table::is.data.table(dat)) {
    dat <- as.data.frame(dat)
  }

  ## If no columns are specified, find std_npsych_scores columns
  if (is.null(std_cols)) {
    is_std <- vapply(
      dat,
      \(x) S7::S7_inherits(x, std_npsych_scores),
      logical(1)
    )
    std_cols <- names(is_std[is_std])
  }

  ## Get method/version from S7 properties
  methods <- lapply(dat[, std_cols, drop = FALSE], \(x) {
    c(method = x@method, version = x@version)
  })

  ## Remove prefix from names
  names(methods) <- gsub(
    x = names(methods),
    pattern = attr(dat, "prefix_std"),
    replacement = ""
  )

  return(methods)
}
