#' Register a norms-based standardization version
#'
#' @description
#' Creates a [norms_version] object and registers it in the `.std_versions`
#' registry via [.register_std_version()].
#'
#' @param scores A `npsych_scores` object, such as `MOCATOTS()`. Used to
#'   determine the scores class for registration.
#' @param version Character string identifying this version (e.g., `"nacc"`,
#'   `"updated"`).
#' @inheritParams norms_version
#' @param overwrite Logical. If `FALSE` (the default), an error is thrown if a
#'   version with the same `version` and scores class already exists in the
#'   registry. If `TRUE`, the existing version is overwritten with the new
#'   one.
#'
#' @return Invisible `NULL`. Called for its side effect of registering the
#'   version in the internal `.std_versions` registry.
#'
#' @export
register_norms_version <- function(
  scores,
  version,
  lookup_table,
  raw_scores_fn = \(x) x,
  covar_fns,
  post_proc_fn = \(x) x,
  overwrite = FALSE,
  description = ""
) {
  if (!S7::S7_inherits(scores, npsych_scores)) {
    cli::cli_abort(c(
      "{.arg scores} must be a {.cls npsych_scores} object.",
      "x" = "You supplied an object of class {.cls {class(scores)}}."
    ))
  }

  v <- norms_version(
    scores_class = S7::S7_class(scores)@name,
    version_id = version,
    description = description,
    lookup_table = lookup_table,
    raw_scores_fn = raw_scores_fn,
    covar_fns = covar_fns,
    post_proc_fn = post_proc_fn
  )

  .register_std_version(v, overwrite = overwrite)
}
