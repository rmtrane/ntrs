#' @include std_version-class.R
NULL

#' Regression-Based Standardization Version
#'
#' @description
#' Create a `regression_version` object that stores regression coefficients and
#' covariate functions for regression-based standardization. Inherits from
#' [std_version].
#'
#' @param scores_class A single non-empty string giving the `npsych_scores`
#'   subclass this version applies to (e.g., `"MOCATOTS"`).
#' @param method_name A single non-empty string identifying the standardization
#'   method (e.g., `"regression"`).
#' @param version_id A single non-empty string uniquely identifying this
#'   version within its method and scores class.
#' @param description An optional single string describing the version.
#' @param coefs A named numeric vector of regression coefficients. Must include
#'   `"intercept"` and `"rmse"` entries.
#' @param covar_fns A named list of functions. May include entries for all
#'   coefficient names other than `"intercept"` and `"rmse"`.
#'
#' @returns
#' An S7 object of class `regression_version` with properties inherited from
#' `std_version` plus `coefs` and `covar_fns`.
#'
#' @export
regression_version <- S7::new_class(
  "regression_version",
  parent = std_version,
  properties = list(
    coefs = S7::class_numeric | S7::class_data.frame,
    covar_fns = S7::class_list
  ),
  # Special constructor to enforce method_name = "regression"
  constructor = function(
    scores_class,
    version_id,
    coefs,
    covar_fns,
    description = ""
  ) {
    S7::new_object(
      S7::S7_object(),
      scores_class = scores_class,
      method_name = "regression",
      version_id = version_id,
      description = description,
      coefs = coefs,
      covar_fns = covar_fns
    )
  },
  validator = function(self) {
    errs <- character()

    if (!"rmse" %in% names(self@coefs)) {
      errs <- c(errs, cli::format_inline("coefs must contain {.val rmse}"))
    }
    if (!"intercept" %in% names(self@coefs)) {
      errs <- c(errs, cli::format_inline("coefs must contain {.val intercept}"))
    }

    covar_names <- setdiff(names(self@coefs), c("intercept", "rmse"))

    if (
      length(covar_names) > 0 &&
        !all(names(self@covar_fns) %in% covar_names)
    ) {
      extra_covar_fns <- setdiff(names(self@covar_fns), covar_names)

      errs <- c(
        errs,
        cli::format_inline(
          c(
            "x" = "{.arg covar_fns} may only include entries for non-intercept, non-rmse coefs. Extras: {.val {extras_covar_fns}}."
            # "i" = "If no transformation needed, provide the identity function, e.g., {.val covar_fns = list({missing_covar_fns[1]} = identity)}. (equivalent to {.val covar_fns = list({missing_covar_fns[1]} = function(x) x))})"
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
