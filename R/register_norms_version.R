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
#' @param lookup_table Data frame containing norms. Must include columns `m`
#'   (mean) and `sd` (standard deviation), may include `n`(sample size).
#'   Additional columns are treated as covariate grouping variables and must
#'   have matching entries in `covar_fns`. See [norms_version] for validation
#'   details.
#' @param covar_fns A named list of functions that transform raw covariate
#'   inputs to match the levels in `lookup_table`. Names must match the
#'   non-statistic columns (everything but `m`, `sd`, `n`) in `lookup_table`.
#' @param description Optional character string describing this version.
#'
#' @return Invisible `NULL`. Called for its side effect of registering the
#'   version in the internal `.std_versions` registry.
#'
#' @export
register_norms_version <- function(
  scores,
  version,
  lookup_table,
  covar_fns,
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
    covar_fns = covar_fns
  )

  .register_std_version(v, overwrite = overwrite)
}
