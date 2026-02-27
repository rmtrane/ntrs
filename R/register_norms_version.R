#' Register a norms-based standardization version
#'
#' @param scores A `npsych_scores` object, such as `MOCATOTS()`.
#' @param version Character string identifying this version (e.g., `"nacc"`,
#'   `"updated"`).
#' @param lookup_table Data frame containing norms. Required columns are
#'   `m` (mean) and `sd` (standard deviation). Optional columns are `n`
#'   (sample size), `age`, `sex`, and `educ`. The grouping columns (`age`,
#'   `sex`, `educ`) must be factors, and all rows must be unique with respect
#'   to these grouping columns. No `NA` values are allowed.
#' @param covar_fns A named list of functions that prepare covariates
#'   for lookup. Names must be a subset of the columns in `lookup_table`. Each
#'   function takes a numeric vector and returns a factor whose sorted unique
#'   levels match those in the corresponding `lookup_table` column. Validation
#'   uses `identical()` on `sort(unique(...))` with domain-specific test
#'   vectors: `0:110` for `age`, `c(1, 2)` for `sex`, and `0:36` for `educ`.
#' @param description Optional character string describing this version.
#' @param overwrite Logical. If `TRUE`, an existing version with the same name
#'   will be overwritten. Defaults to `FALSE`.
#'
#' @return Called for its side effect of registering the version in the
#'   internal `.std_versions` registry. Returns `invisible()`.
#'
#' @export
register_norms_version <- function(
  scores,
  version,
  lookup_table,
  covar_fns,
  description = "",
  overwrite = FALSE
) {
  UseMethod("register_norms_version")
}

#' @export
register_norms_version.npsych_scores <- function(
  scores,
  version,
  lookup_table,
  covar_fns,
  description = "",
  overwrite = FALSE
) {
  # Validate common parameters
  .validate_registration_params(version, description)

  # Validate norms-specific requirements
  if (!is.data.frame(lookup_table)) {
    cli::cli_abort(c(
      "{.arg lookup_table} must be a {.cls data.frame}",
      "x" = "You supplied {.cls {class(lookup_table)}}",
      "i" = "Create a data frame with required columns {.field m}, {.field sd} and optional columns {.field age_group}, {.field sex}, {.field educ_group}."
    ))
  }

  required_cols <- c("m", "sd")
  missing_cols <- setdiff(required_cols, names(lookup_table))

  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "{.arg lookup_table} is missing required columns",
      "x" = "Missing: {.field {missing_cols}}",
      "i" = "Required columns: {.field {required_cols}}",
      "i" = "Current columns: {.field {names(lookup_table)}}"
    ))
  }

  # Make sure we don't have duplicate rows specifying groups
  if (
    nrow(unique(lookup_table[,
      setdiff(names(lookup_table), c("n", "m", "sd")),
      drop = F
    ])) !=
      nrow(lookup_table)
  ) {
    col_select <- paste0(
      'c("',
      paste(setdiff(names(lookup_table), c("n", "m", "sd")), collapse = '", "'),
      '")'
    )

    cli::cli_abort(
      "{.arg lookup_table[, {col_select}, drop = F]} must unique rows, but {.arg lookup_table[, {col_select}, drop = F]} has different number of rows from {.arg lookup_table}."
    )
  }

  optional_cols <- c("n", "age", "sex", "educ")

  not_allowed_cols <- setdiff(
    names(lookup_table),
    c(required_cols, optional_cols)
  )

  if (length(not_allowed_cols) > 0) {
    cli::cli_abort(c(
      "{.arg lookup_table} contains {cli::qty(not_allowed_cols)} {?a column/multiple columns} not allowed.",
      "x" = "Not allowed: {.field {not_allowed_cols}}.",
      "i" = "Remove {cli::qty(not_allowed_cols)}{?column/columns} not allowed and try again."
    ))
  }

  # Check for NA values in required and optional columns present
  allowed_cols <- intersect(
    c(required_cols, optional_cols),
    names(lookup_table)
  )

  na_cols <- allowed_cols[sapply(allowed_cols, function(col) {
    any(is.na(lookup_table[[col]]))
  })]

  if (length(na_cols) > 0) {
    cli::cli_abort(c(
      "{.arg lookup_table} contains {.val NA} values in required columns",
      "x" = "Columns with {.val NA}: {.field {na_cols}}",
      "i" = "All values in {.field {allowed_cols}} must be non-missing"
    ))
  }

  # Validate data types
  if (!is.numeric(lookup_table$m)) {
    cli::cli_abort(c(
      "Column {.field mean} must be numeric",
      "x" = "You supplied {.cls {class(lookup_table$mean)}}"
    ))
  }

  if (!is.numeric(lookup_table$sd)) {
    cli::cli_abort(c(
      "Column {.field sd} must be numeric",
      "x" = "You supplied {.cls {class(lookup_table$sd)}}"
    ))
  }

  # Check for non-positive standard deviations
  if (any(lookup_table$sd <= 0, na.rm = TRUE)) {
    cli::cli_abort(c(
      "Column {.field sd} must contain only positive values",
      "x" = "Found {sum(lookup_table$sd <= 0, na.rm = TRUE)} non-positive value{?s}",
      "i" = "Standard deviations must be greater than 0"
    ))
  }

  # age, educ, sex must be factors
  covars_present <- intersect(c("age", "educ", "sex"), names(lookup_table))

  if (length(covars_present) > 0) {
    non_factor_covars <- covars_present[sapply(
      lookup_table[covars_present],
      function(x) !is.factor(x)
    )]

    if (length(non_factor_covars) > 0) {
      cli::cli_abort(c(
        "{.arg lookup_table} columns {.field {non_factor_covars}} must be factors",
        "x" = "You supplied {.cls {sapply(lookup_table[non_factor_covars], class)}} for {.field {non_factor_covars}}"
      ))
    }
  }

  # Check covar_fns

  # First, if no covars are present, then covar_fns should not be provided
  if (
    length(covars_present) == 0 &&
      # !methods::missingArg(covar_fns) &&
      !rlang::is_missing(covar_fns) &&
      length(covar_fns) > 0
  ) {
    cli::cli_abort(
      "{.arg covar_fns} should not be provided when no covariate columns ({.field age}, {.field sex}, {.field educ}) are present in {.arg lookup_table}."
    )
  }

  # Is it a list?
  if (!is.list(covar_fns)) {
    cli::cli_abort(
      "{.arg covar_fns} must be a {.cls list}, but is a {.cls {class(covar_fns)}}."
    )
  }

  names_mismatches <- setdiff(names(covar_fns), names(lookup_table))

  if (length(names_mismatches) > 0) {
    cli::cli_abort(
      "{.arg covar_fns} must have names that are a subset of the columns in {.arg lookup_table}. {.val {names_mismatches}} are not present in {.arg lookup_table}."
    )
  }

  # Check that all prep functions give appropriate outcomes
  for (fun_nm in names(covar_fns)) {
    fun_x <- switch(
      fun_nm,
      "age" = 0:110,
      "sex" = c(1, 2),
      "educ" = 0:36
    )

    observed <- sort(unique(covar_fns[[fun_nm]](fun_x)))
    target <- sort(unique(lookup_table[[fun_nm]]))

    if (!identical(observed, target)) {
      fun_x <- switch(
        fun_nm,
        "age" = "0:110",
        "sex" = "c(1,2)",
        "educ" = "0:36"
      )
      cli::cli_abort(
        "{.arg covar_fns${fun_nm}} must be a function taking a single argument, and the output of {.code sort(unique(covar_fns${fun_nm}({fun_x})} must be identical to {.code sort(unique(lookup_table${fun_nm}))}."
      )
    }
  }

  # Register
  .register_std_version(
    scores = scores,
    method = "norms",
    version = version,
    data = c(
      list(lookup_table = lookup_table),
      # if (!methods::missingArg(covar_fns)) {
      if (!rlang::is_missing(covar_fns)) {
        list(covar_fns = covar_fns)
      }
    ),
    description = description,
    overwrite = overwrite
  )

  cli::cli_inform(
    c(
      "v" = "Registered {.field norms} version {.val {version}} for {.cls {class(scores)[1]}}"
    ),
    class = "packageStartupMessage"
  )

  invisible()
}
