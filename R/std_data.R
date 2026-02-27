#' Standardize all `npsych_scores` columns in a data frame
#'
#' @description
#' Finds every column in `data` that inherits from `npsych_scores`,
#' standardizes it via [std()], and adds the result as a new column with a
#' `"z_"` prefix (e.g., `MOCATOTS` → `z_MOCATOTS`).
#'
#' Each score class can use a different method/version combination.
#' Classes not mentioned in `methods` fall back to their registered
#' defaults (see [set_std_defaults()]).
#'
#' @param data A `data.frame` or `data.table` containing one or more
#'   `npsych_scores` columns.
#' @param methods An optional named list keyed by `npsych_scores` **class
#'   name** (not column name). Each element is a named character vector of
#'   the form `c(method = "...", version = "...")`. Both elements are
#'   optional within each entry; omitted elements resolve to the registered
#'   default. Classes not listed use their defaults.
#'
#'   ```
#'   methods = list(
#'     MOCATOTS = c(method = "regression", version = "nacc"),
#'     ANIMALS  = c(method = "norms")
#'   )
#'   ```
#' @param ... Named covariates shared across all score columns (e.g.,
#'   `age`, `sex`, `educ`). These are passed to every [std()] call and
#'   must be supplied explicitly. Values can be bare column names
#'   referencing columns in `data` (e.g., `age = age`) or explicit
#'   vectors (e.g., `age = c(72, 65, 80)`).
#' @param prefix A single string prepended to each score column name to
#'   form the new column name. Defaults to `"z_"`.
#' @param .cols An optional character vector of `npsych_scores` **column
#'   names** to standardize. When `NULL` (the default), all `npsych_scores`
#'   columns are processed.
#'
#' @returns The input `data` with additional standardized score columns
#'   appended. The original columns are left unchanged. The return type
#'   matches the input type (`data.frame` or `data.table`).
#'
#' @seealso [std()] for standardizing a single `npsych_scores` vector.
#'
#' @examples
#' \dontrun{
#' # Bare column names — evaluated against data
#' std_data(my_data, age = age, sex = sex, educ = educ)
#'
#' # Explicit vectors work too
#' std_data(my_data, age = my_data$age, sex = my_data$sex, educ = my_data$educ)
#'
#' # Override method/version for specific test classes
#' std_data(
#'   my_data,
#'   methods = list(
#'     MOCATOTS = c(method = "norms", version = "nacc"),
#'     ANIMALS  = c(method = "regression", version = "updated_2025.06")
#'   ),
#'   age = age, sex = sex, educ = educ
#' )
#' }
#'
#' @export
std_data <- function(
  data,
  methods = list(),
  ...,
  prefix = "z_",
  .cols = NULL
) {
  if (!is.data.frame(data)) {
    cli::cli_abort(
      "{.arg data} must be a {.cls data.frame} or {.cls data.table}."
    )
  }

  # ---- Convert to data.table; remember whether to revert ----
  input_is_dt <- data.table::is.data.table(data)
  if (!input_is_dt) {
    data <- data.table::as.data.table(data)
  }

  # ---- Identify npsych_scores columns ----
  npsych_cols <- names(data)[
    vapply(data, inherits, logical(1), what = "npsych_scores")
  ]

  if (length(npsych_cols) == 0L) {
    cli::cli_warn("No {.cls npsych_scores} columns found in {.arg data}.")
    if (!input_is_dt) {
      data <- as.data.frame(data)
    }
    return(data)
  }

  # ---- Subset to requested columns ----
  if (!is.null(.cols)) {
    bad_cols <- setdiff(.cols, npsych_cols)
    if (length(bad_cols)) {
      cli::cli_abort(
        "{.arg .cols} contains column{?s} that {?is/are} not {.cls npsych_scores}: {.val {bad_cols}}."
      )
    }
    npsych_cols <- intersect(npsych_cols, .cols)
  }

  # ---- Build class → column map ----
  col_classes <- vapply(
    data[, npsych_cols, with = FALSE],
    function(x) setdiff(class(x), "npsych_scores"),
    character(1)
  )

  # ---- Validate methods argument ----
  if (!is.list(methods)) {
    cli::cli_abort(
      "{.arg methods} must be a named {.cls list}, not {.cls {class(methods)[[1]]}}."
    )
  }

  if (length(methods) > 0L) {
    known_classes <- unique(col_classes)
    unknown_classes <- setdiff(names(methods), known_classes)
    if (length(unknown_classes)) {
      cli::cli_warn(
        "{.arg methods} contains class name{?s} not matching any {.cls npsych_scores} column in {.arg data}: {.val {unknown_classes}}."
      )
    }
  }

  # ---- Evaluate covariates: support bare column names via tidy eval ----
  dots <- rlang::enquos(...)

  if (length(dots) > 0L && (is.null(names(dots)) || any(names(dots) == ""))) {
    cli::cli_abort("All covariates in {.arg ...} must be named.")
  }

  data_mask <- rlang::as_data_mask(data)
  caller_env <- rlang::caller_env()

  covars <- lapply(dots, function(quo) {
    rlang::eval_tidy(quo, data = data_mask, env = caller_env)
  })

  # ---- New column names ----
  new_nms <- paste0(prefix, npsych_cols)

  # ---- Standardize using .SD / .SDcols ----
  data[,
    (new_nms) := lapply(.SD, function(scores_col) {
      scores_class <- setdiff(class(scores_col), "npsych_scores")
      col_spec <- methods[[scores_class]]
      col_method <- col_spec[["method"]]
      col_version <- col_spec[["version"]]

      tryCatch(
        do.call(
          std,
          c(
            list(
              scores = scores_col,
              method = col_method,
              version = col_version
            ),
            covars
          )
        ),
        error = function(e) {
          col_nm <- npsych_cols[
            match(scores_class, col_classes)
          ]
          cli::cli_warn(c(
            "!" = "Failed to standardize column {.field {col_nm}} ({.cls {scores_class}}).",
            "x" = conditionMessage(e)
          ))
          rep(NA_real_, length(scores_col))
        }
      )
    }),
    .SDcols = npsych_cols
  ]

  # ---- Revert to data.frame if input was not data.table ----
  if (!input_is_dt) {
    data <- as.data.frame(data)
  }

  data
}
