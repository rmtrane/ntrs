#' Register a regression-based standardization version
#'
#' @description
#' Creates a [regression_version] object and registers it in the
#' `.std_versions` registry via [.register_std_version()].
#'
#' @param scores A `npsych_scores` object, such as `MOCATOTS()`. Used to
#'   determine the scores class for registration.
#' @param version Character string identifying this version (e.g., `"nacc"`,
#'   `"updated"`).
#' @param coefs A named numeric vector or a data frame of regression coefficients.
#'   Must include `"intercept"` and `"rmse"` entries. Additional names are treated as
#'   covariate coefficients. See [regression_version] for validation details.
#' @param covar_fns A named list of functions that transform raw covariate
#'   inputs. Must include entries for all coefficient names other than
#'   `"intercept"` and `"rmse"`.
#' @param description Optional character string describing this version.
#' @param overwrite Logical. If `TRUE`, allows overwriting an existing version
#'   with a warning. Defaults to `FALSE`.
#'
#' @return Invisible `NULL`. Called for its side effect of registering the
#'   version in the internal `.std_versions` registry.
#'
#' @export
register_regression_version <- function(
  scores,
  version,
  coefs,
  raw_scores_fn = \(x) x,
  covar_fns = NULL,
  post_proc_fn = \(x) x,
  description = "",
  overwrite = FALSE
) {
  if (!S7::S7_inherits(scores, npsych_scores)) {
    cli::cli_abort(c(
      "{.arg scores} must be a {.cls npsych_scores} object.",
      "x" = "You supplied an object of class {.cls {class(scores)}}."
    ))
  }

  if (is.null(covar_fns)) {
    covar_fns <- lapply(setdiff(names(coefs), c("intercept", "rmse")), \(x) {
      identity
    })
  }

  v <- regression_version(
    scores_class = S7::S7_class(scores)@name,
    version_id = version,
    description = description,
    coefs = coefs,
    raw_scores_fn = raw_scores_fn,
    covar_fns = covar_fns,
    post_proc_fn = post_proc_fn
  )

  .register_std_version(v, overwrite = overwrite)
}
