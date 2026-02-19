# ---------------------------------------------------------------------------
# OTRAILA()
# ---------------------------------------------------------------------------

test_that("OTRAILA returns an object inheriting both OTRAILA and npsych_scores", {
  result <- OTRAILA()

  expect_s3_class(result, "OTRAILA")
  expect_s3_class(result, "npsych_scores")
})

test_that("OTRAILA sets class with OTRAILA first, npsych_scores second", {
  result <- OTRAILA()

  expect_equal(class(result), c("OTRAILA", "npsych_scores"))
})

test_that("OTRAILA returns an object with the correct values", {
  result <- OTRAILA(c(0, 0, 100))

  expect_equal(as.numeric(result), c(0, 0, 100))
})

test_that("OTRAILA sets label to 'Oral Trailmaking Part A - Completion Time'", {
  result <- OTRAILA()

  expect_equal(
    attr(result, "label"),
    "Oral Trailmaking Part A - Completion Time"
  )
})

test_that("OTRAILA sets range to c(0, 100)", {
  result <- OTRAILA()

  expect_equal(attr(result, "range"), c(0, 100))
})

test_that("OTRAILA sets codes with the correct values", {
  result <- OTRAILA()
  codes <- attr(result, "codes")

  expect_true(888 %in% codes)
  expect_true(995 %in% codes)
  expect_true(996 %in% codes)
  expect_true(997 %in% codes)
  expect_true(998 %in% codes)
  expect_true(-4 %in% codes)
})

test_that("OTRAILA codes are named", {
  result <- OTRAILA()

  expect_false(is.null(names(attr(result, "codes"))))
})

test_that("OTRAILA accepts scores at the range boundaries (0 and 100)", {
  expect_no_error(OTRAILA(c(0, 100)))
})

test_that("OTRAILA accepts valid error code 888", {
  expect_no_error(OTRAILA(c(0, 888)))
})

test_that("OTRAILA accepts valid error code 995", {
  expect_no_error(OTRAILA(c(0, 995)))
})

test_that("OTRAILA accepts valid error code 996", {
  expect_no_error(OTRAILA(c(0, 996)))
})

test_that("OTRAILA accepts valid error code 997", {
  expect_no_error(OTRAILA(c(0, 997)))
})

test_that("OTRAILA accepts valid error code 998", {
  expect_no_error(OTRAILA(c(0, 998)))
})

test_that("OTRAILA accepts valid error code -4", {
  expect_no_error(OTRAILA(c(0, -4)))
})

test_that("OTRAILA errors when scores are out of range and not a code", {
  testthat::local_reproducible_output()

  expect_error(OTRAILA(c(0, 101)), regexp = "scores")
})

test_that("OTRAILA errors when scores is not numeric", {
  testthat::local_reproducible_output()

  expect_error(OTRAILA(c("a", "b")), regexp = "scores")
})

test_that("OTRAILA with no arguments returns an empty OTRAILA object", {
  result <- OTRAILA()

  expect_s3_class(result, "OTRAILA")
  expect_equal(length(result), 0L)
})


# ---------------------------------------------------------------------------
# .setup_OTRAILA_versions()
#
# .setup_*_versions() writes to the package-level .std_versions environment
# and errors on duplicate registration. The function is called once here
# outside any test_that() block; all tests below only read the registry.
# ---------------------------------------------------------------------------

rm(list = ls(envir = .std_versions), envir = .std_versions)
.setup_OTRAILA_versions()

test_that(".setup_OTRAILA_versions registers the expected methods", {
  methods <- list_std_methods(OTRAILA())

  expect_true("norms" %in% methods)
  expect_true("regression" %in% methods)
})

# ---------------------------------------------------------------------------
# Norms versions
# ---------------------------------------------------------------------------

test_that(".setup_OTRAILA_versions registers the 'updated' norms version", {
  expect_true("updated" %in% list_method_versions(OTRAILA(), "norms"))
})

test_that("'updated' norms version data contains a lookup_table with m and sd columns", {
  data <- get_version_data(OTRAILA(), "norms", "updated")

  expect_true("lookup_table" %in% names(data))
  expect_s3_class(data$lookup_table, "data.frame")
  expect_true("m" %in% names(data$lookup_table))
  expect_true("sd" %in% names(data$lookup_table))
})

test_that("'updated' norms version data contains the expected covar_fns", {
  data <- get_version_data(OTRAILA(), "norms", "updated")

  expect_true("covar_fns" %in% names(data))
  expect_true("age" %in% names(data$covar_fns))
  expect_true("sex" %in% names(data$covar_fns))
})

# ---------------------------------------------------------------------------
# Regression versions
# ---------------------------------------------------------------------------

test_that(".setup_OTRAILA_versions registers the 'updated_2024.06' regression version", {
  expect_true(
    "updated_2024.06" %in% list_method_versions(OTRAILA(), "regression")
  )
})

test_that(".setup_OTRAILA_versions registers the 'updated_2025.06' regression version", {
  expect_true(
    "updated_2025.06" %in% list_method_versions(OTRAILA(), "regression")
  )
})

test_that("regression version data contains coefs with rmse", {
  for (version in c("updated_2024.06", "updated_2025.06")) {
    data <- get_version_data(OTRAILA(), "regression", version)

    expect_true("coefs" %in% names(data), info = version)
    expect_true("rmse" %in% names(data$coefs), info = version)
  }
})

test_that("regression version data contains covar_fns for age, sex, and educ", {
  for (version in c("updated_2024.06", "updated_2025.06")) {
    data <- get_version_data(OTRAILA(), "regression", version)

    expect_true("covar_fns" %in% names(data), info = version)
    expect_true("age" %in% names(data$covar_fns), info = version)
    expect_true("sex" %in% names(data$covar_fns), info = version)
    expect_true("educ" %in% names(data$covar_fns), info = version)
  }
})

# ---------------------------------------------------------------------------
# Default method
# ---------------------------------------------------------------------------

test_that(".setup_OTRAILA_versions sets the default method to 'regression' / 'updated_2025.06'", {
  default <- get_std_defaults(OTRAILA())

  expect_equal(default$method, "regression")
  expect_equal(default$version, "updated_2025.06")
})
