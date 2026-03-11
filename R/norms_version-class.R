#' @include std_version-class.R
NULL


#' Norms-Based Standardization Version
#'
#' @description
#' Create a `norms_version` object that stores a lookup table and covariate
#' functions for norms-based standardization. Inherits from [std_version].
#'
#' @param scores_class A single non-empty string giving the `npsych_scores`
#'   subclass this version applies to (e.g., `"MOCATOTS"`).
#' @param version_id A single non-empty string uniquely identifying this
#'   version within its method and scores class.
#' @param description An optional single string describing the version.
#' @param lookup_table A data frame containing columns `m` (mean) and `sd`
#'   (standard deviation), plus covariate columns.
#' @param covar_fns A named list of functions. Names must match the
#'   non-statistic columns in `lookup_table`.
#'
#' @returns
#' An S7 object of class `norms_version` with properties inherited from
#' `std_version` plus `lookup_table` and `covar_fns`.
#'
#' @export
norms_version <- S7::new_class(
  "norms_version",
  parent = std_version,
  properties = list(
    lookup_table = S7::class_data.frame,
    raw_scores_fn = S7::class_function | NULL,
    covar_fns = S7::class_list,
    post_proc_fn = S7::class_function | NULL
  ),
  constructor = function(
    scores_class,
    version_id,
    lookup_table,
    covar_fns,
    raw_scores_fn = \(x) x,
    post_proc_fn = \(x) x,
    description = ""
  ) {
    S7::new_object(
      S7::S7_object(),
      scores_class = scores_class,
      method_name = "norms",
      version_id = version_id,
      description = description,
      lookup_table = lookup_table,
      raw_scores_fn = raw_scores_fn,
      covar_fns = covar_fns,
      post_proc_fn = post_proc_fn
    )
  },
  validator = function(self) {
    errs <- character()

    required_cols <- c("m", "sd")
    missing_cols <- setdiff(required_cols, names(self@lookup_table))
    if (length(missing_cols)) {
      errs <- c(
        errs,
        cli::format_inline(
          "{.arg lookup_table} missing required columns: {.val {missing_cols}}"
        )
      )
    }

    covar_names <- setdiff(names(self@lookup_table), c("m", "sd", "n"))
    if (!setequal(names(self@covar_fns), covar_names)) {
      missing_fns <- setdiff(covar_names, names(self@covar_fns))
      extra_fns <- setdiff(names(self@covar_fns), covar_names)
      errs <- c(
        errs,
        cli::format_inline(
          c(
            "x" = "{.arg covar_fns} names must match non-stat columns in {.arg lookup_table}.",
            if (length(missing_fns)) {
              "i" = "Missing from {.arg covar_fns}: {.val {missing_fns}}."
            },
            if (length(extra_fns)) {
              "i" = "Extra in {.arg covar_fns}: {.val {extra_fns}}."
            }
          )
        )
      )
    }

    if (!all(vapply(self@covar_fns, is.function, logical(1)))) {
      errs <- c(
        errs,
        cli::format_inline("all {.arg covar_fns} entries must be functions")
      )
    }

    if (length(errs)) errs else NULL
  }
)
