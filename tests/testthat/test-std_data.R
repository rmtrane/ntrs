# Tests for std_data() — batch standardization of npsych_scores columns
#
# std_data() finds npsych_scores columns in a data.frame / data.table,
# standardizes each via std(), and appends new columns with a prefix.
#
# State isolation:
# Tests that modify .std_defaults use local_restore_default() from
# helper-restore-defaults.R.

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("std_data() errors when data is not a data.frame", {
  testthat::local_reproducible_output()

  expect_error(
    std_data(list(a = 1)),
    "data.frame"
  )
})

test_that("std_data() errors when methods is not a list", {
  testthat::local_reproducible_output()

  df <- data.frame(x = 1:3)
  df$moca <- MOCATOTS(c(25, 26, 27))

  expect_error(
    std_data(df, methods = "norms"),
    "must be a"
  )
})

test_that("std_data() errors when there are unnamed arguments in ...", {
  testthat::local_reproducible_output()

  df <- data.frame(x = 1:3)
  df$moca <- MOCATOTS(c(25, 26, 27))

  expect_error(
    std_data(df, age = c(70, 75, 80), "unnamed_arg"),
    "must be named"
  )
})

# ---------------------------------------------------------------------------
# No npsych_scores columns
# ---------------------------------------------------------------------------

test_that("std_data() warns when no npsych_scores columns found", {
  df <- data.frame(x = 1:3, y = letters[1:3])

  expect_warning(
    std_data(df),
    "No.*npsych_scores.*columns"
  )
})

test_that("std_data() returns data unchanged when no npsych_scores columns found", {
  df <- data.frame(x = 1:3, y = letters[1:3])

  result <- suppressWarnings(std_data(df))

  expect_equal(result, df)
})

# ---------------------------------------------------------------------------
# .cols validation
# ---------------------------------------------------------------------------

test_that("std_data() errors when .cols names a non-npsych_scores column", {
  testthat::local_reproducible_output()

  df <- data.frame(x = 1:3)
  df$moca <- MOCATOTS(c(25, 26, 27))

  expect_error(
    std_data(df, .cols = "x"),
    "not.*npsych_scores"
  )
})

# ---------------------------------------------------------------------------
# Basic standardization — data.frame input
# ---------------------------------------------------------------------------

test_that("std_data() appends z_ prefixed columns for npsych_scores columns", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
  )

  df <- data.frame(age = c(72, 75), sex = c(1, 2), educ = c(16, 12))
  df$moca <- MOCATOTS(c(25, 28))

  result <- std_data(df, age = df$age, sex = df$sex, educ = df$educ)

  expect_true("z_moca" %in% names(result))
})

test_that("std_data() standardized values match std() output", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
  )

  df <- data.frame(age = c(72, 75), sex = c(1, 2), educ = c(16, 12))
  df$moca <- MOCATOTS(c(25, 28))

  result <- std_data(df, age = df$age, sex = df$sex, educ = df$educ)

  expected <- std(df$moca, age = df$age, sex = df$sex, educ = df$educ)

  expect_equal(result$z_moca, expected)
})

test_that("std_data() returns a data.frame when input is a data.frame", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
  )

  df <- data.frame(age = c(72, 75), sex = c(1, 2), educ = c(16, 12))
  df$moca <- MOCATOTS(c(25, 28))

  result <- std_data(df, age = df$age, sex = df$sex, educ = df$educ)

  expect_s3_class(result, "data.frame")
  expect_false(data.table::is.data.table(result))
})

# ---------------------------------------------------------------------------
# data.table input preserved
# ---------------------------------------------------------------------------

test_that("std_data() returns a data.table when input is a data.table", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
  )

  dt <- data.table::data.table(
    age = c(72, 75),
    sex = c(1, 2),
    educ = c(16, 12)
  )
  dt[, moca := MOCATOTS(c(25, 28))]

  result <- std_data(dt, age = dt$age, sex = dt$sex, educ = dt$educ)

  expect_true(data.table::is.data.table(result))
})

# ---------------------------------------------------------------------------
# .cols subsetting
# ---------------------------------------------------------------------------

test_that("std_data() only standardizes columns listed in .cols", {
  local_restore_default("MOCATOTS")
  local_restore_default("ANIMALS")

  suppressMessages({
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
    set_std_defaults(ANIMALS(), method = "norms", version = "nacc")
  })

  df <- data.frame(age = c(72, 75), sex = c(1, 2), educ = c(16, 12))
  df$moca <- MOCATOTS(c(25, 28))
  df$animals <- ANIMALS(c(20, 22))

  result <- std_data(
    df,
    .cols = "moca",
    age = df$age,
    sex = df$sex,
    educ = df$educ
  )

  expect_true("z_moca" %in% names(result))
  expect_false("z_animals" %in% names(result))
})

# ---------------------------------------------------------------------------
# methods argument — override per class
# ---------------------------------------------------------------------------

test_that("std_data() uses methods argument to override defaults", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(
      MOCATOTS(),
      method = "regression",
      version = "updated_2025.06"
    )
  )

  df <- data.frame(age = c(72, 75), sex = c(1, 2), educ = c(16, 12))
  df$moca <- MOCATOTS(c(25, 28))

  result <- std_data(
    df,
    methods = list(MOCATOTS = c(method = "norms", version = "nacc")),
    age = df$age,
    sex = df$sex,
    educ = df$educ
  )

  expected <- std_using_norms(
    df$moca,
    age = df$age,
    sex = df$sex,
    educ = df$educ,
    version = "nacc"
  )

  expect_equal(result$z_moca, expected)
})

# ---------------------------------------------------------------------------
# methods argument — warns for unknown class names
# ---------------------------------------------------------------------------

test_that("std_data() warns when methods contains unknown class names", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
  )

  df <- data.frame(age = c(72, 75), sex = c(1, 2), educ = c(16, 12))
  df$moca <- MOCATOTS(c(25, 28))

  expect_warning(
    std_data(
      df,
      methods = list(NONEXISTENT = c(method = "norms", version = "nacc")),
      age = df$age,
      sex = df$sex,
      educ = df$educ
    ),
    "NONEXISTENT"
  )
})

# ---------------------------------------------------------------------------
# Custom prefix
# ---------------------------------------------------------------------------

test_that("std_data() respects custom prefix", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
  )

  df <- data.frame(age = c(72, 75), sex = c(1, 2), educ = c(16, 12))
  df$moca <- MOCATOTS(c(25, 28))

  result <- std_data(
    df,
    prefix = "std_",
    age = df$age,
    sex = df$sex,
    educ = df$educ
  )

  expect_true("std_moca" %in% names(result))
  expect_false("z_moca" %in% names(result))
})

# ---------------------------------------------------------------------------
# Error in standardization — warns and returns NA
# ---------------------------------------------------------------------------

test_that("std_data() warns and returns NA when standardization fails for a column", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
  )

  df <- data.frame(dummy = 1:3)
  df$moca <- MOCATOTS(c(25, 26, 27))

  # Missing covariates should trigger the tryCatch error path
  result <- suppressWarnings(
    std_data(df)
  )

  expect_warning(
    std_data(df),
    "Failed to standardize"
  )
})

test_that("std_data() returns NA for columns that fail standardization", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
  )

  df <- data.frame(dummy = 1:3)
  df$moca <- MOCATOTS(c(25, 26, 27))

  # Missing covariates — standardization will fail
  result <- suppressWarnings(std_data(df))

  expect_true(all(is.na(result$z_moca)))
  expect_length(result$z_moca, 3)
})

# ---------------------------------------------------------------------------
# Original columns are preserved
# ---------------------------------------------------------------------------

test_that("std_data() preserves original columns unchanged", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
  )

  df <- data.frame(age = c(72, 75), sex = c(1, 2), educ = c(16, 12))
  df$moca <- MOCATOTS(c(25, 28))

  result <- std_data(df, age = df$age, sex = df$sex, educ = df$educ)

  expect_equal(result$moca, df$moca)
  expect_equal(result$age, df$age)
})
