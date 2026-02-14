#' Register a norms-based standardization version
#'
#' @param version Character string identifying this version (e.g., "nacc", "updated")
#' @param test_class Character string identifying the test class (e.g., "MOCATOTS")
#' @param lookup_table Data frame containing norms with columns: age_group, mean, sd
#' @param age_group_fun Function taking a numeric vector as argument and returns
#'   exactly the factor from `lookup_table$age_group`, if this is present. Internal
#'   check is performed by comparing `unique(age_group_fun(0:110))` to `unique(lookup_table$age_group)`
#'   using `all.equal`.
#' @param educ_group_fun Function taking a numeric vector as argument and returns
#'   exactly the factor from `lookup_table$educ_group`, if this is present. Internal
#'   check is performed by comparing `unique(educ_group_fun(0:50))` to `unique(lookup_table$educ_group)`
#'   using `all.equal`.
#' @param description Optional description of this version
#'
#' @export
register_norms_version <- function(
  test_class,
  version,
  lookup_table,
  covariate_prep_funs,
  description = "",
  overwrite = FALSE
) {
  UseMethod("register_norms_version")
}

#' @export
register_norms_version.test_scores <- function(
  test_class,
  version,
  lookup_table,
  covariate_prep_funs,
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
  non_factors <- names(which(sapply(
    lookup_table[,
      intersect(c("age", "educ", "sex"), names(lookup_table)),
      drop = F
    ],
    \(x) !is.factor(x)
  )))

  if (length(non_factors) > 0) {
    non_factors <- sapply(lookup_table[, non_factors, drop = F], class)
    cli::cli_abort(
      "{cli::qty(non_factors)}Column{?s} {.field {names(non_factors)}} must be of class {.cls factor}, but {?is/are} {.cls {unname(non_factors)}}."
    )
  }

  # Check covariate_prep_funs
  # Is it a list?
  if (!is.list(covariate_prep_funs)) {
    cli::cli_abort(
      "{.arg covariate_prep_funs} must be a {.cls list}, but is a {.cls {class(covariate_prep_funs)}}."
    )
  }

  names_mismatches <- setdiff(names(covariate_prep_funs), names(lookup_table))

  if (length(names_mismatches) > 0) {
    cli::cli_abort(
      "{.arg covariate_prep_funs} must be have names that are a subset of the columns in {.arg lookup_table}. {.val {names_mismatches}} are not present in {.arg lookup_table}."
    )
  }

  # Check that all prep functions give appropriate outcomes
  for (fun_nm in names(covariate_prep_funs)) {
    fun_x <- switch(
      fun_nm,
      "age" = 0:110,
      "sex" = c(1, 2),
      "educ" = 0:36
    )

    observed <- sort(unique(covariate_prep_funs[[fun_nm]](fun_x)))
    target <- sort(unique(lookup_table[[fun_nm]]))

    if (!identical(observed, target)) {
      fun_x <- switch(
        fun_nm,
        "age" = "0:110",
        "sex" = "c(1,2)",
        "educ" = "0:36"
      )
      cli::cli_abort(
        "{.arg covariate_prep_funs${fun_nm}} must be a function taking a single argument, and the output of {.code sort(unique(covariate_prep_funs${fun_nm}({fun_x})} must be identical to {.code sort(unique(lookup_table${fun_nm}))}."
      )
    }
  }

  # Register
  .register_std_version(
    test_class = test_class,
    method = "norms",
    version = version,
    data = c(
      list(lookup_table = lookup_table),
      if (!missingArg(covariate_prep_funs)) {
        list(covariate_prep_funs = covariate_prep_funs)
      }
    ),
    description = description,
    overwrite = overwrite
  )

  cli::cli_alert_success(
    "Registered {.field norms} version {.val {version}} for {.val {test_class}}"
  )

  invisible()
}

#' Register a regression-based standardization version
#'
#' @param version Character string identifying this version
#' @param test_class Character string identifying the test class
#' @param description Optional description of this version
#'
#' @export
register_regression_version <- function(
  test_class,
  version,
  coefs,
  covariate_prep_funs,
  description = ""
) {
  UseMethod("register_regression_version")
}

#' @export
register_regression_version.test_scores <- function(
  test_class,
  version,
  coefs,
  covariate_prep_funs,
  description = ""
) {
  # Validate common parameters
  .validate_registration_params(version, description)

  # Validate regression-specific requirements
  valid_coef_classes <- c("numeric", "data.frame")

  if (!inherits(coefs, valid_coef_classes)) {
    cli::cli_abort(c(
      "{.arg coefs} must be a {.cls numeric} or {.cls data.frame}",
      "x" = "You supplied {.cls {class(coefs)}}",
      "i" = "Accepted classes: {.cls {valid_coef_classes}}"
    ))
  }

  if (is.numeric(coefs) && any(is.na(coefs))) {
    cli::cli_abort(c(
      "{.arg coefs} cannot contain missing values when a numeric vector is provided."
    ))
  }

  ## If coefs is a data.frame, check that no column consists of only missing values
  col_missing <- names(which(sapply(coefs, \(x) sum(!is.na(x)) == 0)))

  if (inherits(coefs, "data.frame") && length(col_missing) > 0) {
    cli::cli_abort(c(
      "{.arg coefs} cannot have columns with only missing values. {.field {col_missing}} {?has/have} only missing values."
    ))
  }

  # Check that coefs are named
  if (is.null(names(coefs))) {
    cli::cli_abort("{.arg coefs} must be named.")
  }

  # Make sure rmse is included
  if (!"rmse" %in% names(coefs)) {
    cli::cli_abort("{.arg coefs} must include {.field rmse}.")
  }

  # Make sure all coefficients are allowed
  allowed_coefs <- c(
    "intercept",
    "age",
    "sex",
    "educ",
    "race",
    "delay",
    "rmse"
  )

  if (!all(names(coefs) %in% allowed_coefs)) {
    cli::cli_abort(
      "{.arg coefs} must only contain a subset of {.field {allowed_coefs}}, but includes {.field {names(coefs)}}."
    )
  }

  if (!missingArg(covariate_prep_funs)) {
    if (!is.list(covariate_prep_funs)) {
      cli::cli_abort(
        "{.arg covariate_prep_funs} must be a {.cls list}, but is a {.cls {class(covariate_prep_funs)}}."
      )
    }

    mismatch_funs <- setdiff(names(covariate_prep_funs), names(coefs))

    if (length(mismatch_funs) > 0) {
      cli::cli_abort(
        "{.arg names(covariate_prep_funs)} must be a subset of {.code names(coefs)}. {.val {mismatch_funs}} not found in {.code names(coefs)}."
      )
    }
  }

  # Register
  .register_std_version(
    test_class = test_class,
    method = "regression",
    version = version,
    data = c(
      list(coefs = coefs),
      if (!missingArg(covariate_prep_funs)) {
        list(covariate_prep_funs = covariate_prep_funs)
      }
    ),
    description = description
  )

  cli::cli_alert_success(
    "Registered {.field regression} version {.val {version}} for {.val {test_class}}"
  )

  invisible()
}

# ============================================================================
# Retrieval Functions
# ============================================================================

#' Get a specific standardization version or all available versions
#'
#' @param method Character string giving method.
#' @param test_class Character string identifying the test class
#' @param version Character string identifying the version. If NULL, uses default.
#'
#' @return List containing version information and data
#'
#' @export
get_versions <- function(test_class, method) {
  UseMethod("get_versions")
}

#' @export
get_versions.test_scores <- function(test_class, method) {
  # Check if method exists
  if (!exists(method, envir = .std_versions, inherits = FALSE)) {
    available_methods <- ls(envir = .std_versions)

    if (length(available_methods) == 0) {
      cli::cli_abort(c(
        "No versions registered for method {.val {method}}",
        "x" = "No versions have been registered yet",
        "i" = "Register versions using {.fn register_norms_version} or {.fn register_regression_version}"
      ))
    } else {
      cli::cli_abort(c(
        "No versions registered for method {.val {method}}",
        "i" = "Available methods: {.val {available_methods}}"
      ))
    }
  }

  test_class <- setdiff(class(test_class), "test_scores")

  # Check if test_class exists
  if (!exists(test_class, envir = .std_versions[[method]], inherits = FALSE)) {
    available_classes <- ls(.std_versions[[method]])

    cli::cli_abort(c(
      "No versions registered for test class {.val {test_class}}",
      "x" = "Method {.val {method}} has no versions for {.val {test_class}}",
      "i" = "Available test classes: {.val {available_classes}}"
    ))
  }

  ls(.std_versions[[method]][[test_class]])
}


#' Get standardization methods for test scores
#'
#' @description Get methods that are available for a test class, or all available methods.
#'
#' @param test_class Optional. An `"test_scores"` object, such as `MOCATOTS()`.
#'
#' @returns
#' A character vector of method names. If `test_class` is missing, returns all available standardization methods.
#' If `test_class` is provided, returns standard methods applicable to that specific test class.
#'
#' @export
get_std_methods <- function(test_class) {
  UseMethod("get_std_methods")
}

#' @export
get_std_methods.test_scores <- function(test_class) {
  if (missingArg(test_class)) {
    return(setdiff(ls(envir = .std_versions), "defaults"))
  }

  test_class <- setdiff(class(test_class), "test_scores")

  with_test_class <- lapply(as.list(.std_versions), ls) |>
    sapply(\(x) test_class %in% x)

  setdiff(names(which(with_test_class)), "defaults")
}


# ============================================================================
# Utility Functions
# ============================================================================

#' Remove a registered version
#'
#' @param method Character string: "norms" or "regression"
#' @param test_class Character string identifying the test class
#' @param version Character string identifying the version to remove
#'
#' @export
remove_std_version <- function(test_class, method, version) {
  available_versions <- get_versions(test_class, method)

  # Verify version exists (will error if not)
  if (version %in% available_versions) {
    cli::cli_abort("{.val {version}} is not implemented for ")
  }

  # Check if it's the default
  if (identical(get_default_version(test_class, method), version)) {
    remaining <- setdiff(ls(.std_versions[[method]][[test_class]]), version)
    cli::cli_warn(c(
      "Removing default version",
      "!" = "Version {.val {version}} is currently the default",
      "i" = "Remaining versions: {.val {remaining}}",
      "i" = "Consider setting a new default with {.fn set_default_method}"
    ))
  }

  # Remove
  rm(list = version, envir = .std_versions[[method]][[test_class]])

  cli::cli_alert_success(
    "Removed version {.val {version}} for {.field {method}} method on {.val {test_class}}"
  )

  invisible()
}
