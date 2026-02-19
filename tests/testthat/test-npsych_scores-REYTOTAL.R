# ---------------------------------------------------------------------------
# REYTOTAL()
# ---------------------------------------------------------------------------

# NOTE: REYTOTAL() explicitly passes codes = numeric(). The current
# validate_npsych_scores() requires a *named* numeric vector for codes,
# so calling REYTOTAL() with any scores will currently error. The
# constructor tests below document the intended behaviour; they will
# fail until the constructor or validate_npsych_scores() is updated.

test_that("REYTOTAL returns an object inheriting both REYTOTAL and npsych_scores", {
  result <- REYTOTAL()

  expect_s3_class(result, "REYTOTAL")
  expect_s3_class(result, "npsych_scores")
})

test_that("REYTOTAL sets class with REYTOTAL first, npsych_scores second", {
  result <- REYTOTAL()

  expect_equal(class(result), c("REYTOTAL", "npsych_scores"))
})

test_that("REYTOTAL returns an object with the correct values", {
  result <- REYTOTAL(c(0, 0, 75))

  expect_equal(as.numeric(result), c(0, 0, 75))
})

test_that("REYTOTAL sets label to 'RAVLT Total Learning'", {
  result <- REYTOTAL()

  expect_equal(attr(result, "label"), "RAVLT Total Learning")
})

test_that("REYTOTAL sets range to c(0, 75)", {
  result <- REYTOTAL()

  expect_equal(attr(result, "range"), c(0, 75))
})

test_that("REYTOTAL accepts scores at the range boundaries (0 and 75)", {
  expect_no_error(REYTOTAL(c(0, 75)))
})

test_that("REYTOTAL errors when scores are out of range and not a code", {
  testthat::local_reproducible_output()

  expect_error(REYTOTAL(c(0, 76)), regexp = "scores")
})

test_that("REYTOTAL errors when scores is not numeric", {
  testthat::local_reproducible_output()

  expect_error(REYTOTAL(c("a", "b")), regexp = "scores")
})

test_that("REYTOTAL with no arguments returns an empty REYTOTAL object", {
  result <- REYTOTAL()

  expect_s3_class(result, "REYTOTAL")
  expect_equal(length(result), 0L)
})


# ---------------------------------------------------------------------------
# .setup_REYTOTAL_versions()
#
# .setup_*_versions() writes to the package-level .std_versions environment
# and errors on duplicate registration. The function is called once here
# outside any test_that() block; all tests below only read the registry.
# ---------------------------------------------------------------------------

rm(list = ls(envir = .std_versions), envir = .std_versions)
.setup_REYTOTAL_versions()

test_that(".setup_REYTOTAL_versions registers the expected methods", {
  methods <- list_std_methods(REYTOTAL())

  expect_true("norms" %in% methods)
  expect_true("regression" %in% methods)
})

# ---------------------------------------------------------------------------
# Norms versions
# ---------------------------------------------------------------------------

test_that(".setup_REYTOTAL_versions registers the 'ravlt_trials' norms version", {
  expect_true("ravlt_trials" %in% list_method_versions(REYTOTAL(), "norms"))
})

test_that("'ravlt_trials' norms version data contains a lookup_table with m and sd columns", {
  data <- get_version_data(REYTOTAL(), "norms", "ravlt_trials")

  expect_true("lookup_table" %in% names(data))
  expect_s3_class(data$lookup_table, "data.frame")
  expect_true("m" %in% names(data$lookup_table))
  expect_true("sd" %in% names(data$lookup_table))
})

test_that("'ravlt_trials' norms version data contains the expected covar_fns", {
  data <- get_version_data(REYTOTAL(), "norms", "ravlt_trials")

  expect_true("covar_fns" %in% names(data))
  expect_true("age" %in% names(data$covar_fns))
})

# ---------------------------------------------------------------------------
# Regression versions
# ---------------------------------------------------------------------------

test_that(".setup_REYTOTAL_versions registers the 'updated_2024.06' regression version", {
  expect_true(
    "updated_2024.06" %in% list_method_versions(REYTOTAL(), "regression")
  )
})

test_that(".setup_REYTOTAL_versions registers the 'updated_2025.06' regression version", {
  expect_true(
    "updated_2025.06" %in% list_method_versions(REYTOTAL(), "regression")
  )
})

test_that("regression version data contains coefs with rmse", {
  for (version in c("updated_2024.06", "updated_2025.06")) {
    data <- get_version_data(REYTOTAL(), "regression", version)

    expect_true("coefs" %in% names(data), info = version)
    expect_true("rmse" %in% names(data$coefs), info = version)
  }
})

test_that("regression version data contains covar_fns for age, sex, and educ", {
  for (version in c("updated_2024.06", "updated_2025.06")) {
    data <- get_version_data(REYTOTAL(), "regression", version)

    expect_true("covar_fns" %in% names(data), info = version)
    expect_true("age" %in% names(data$covar_fns), info = version)
    expect_true("sex" %in% names(data$covar_fns), info = version)
    expect_true("educ" %in% names(data$covar_fns), info = version)
  }
})

# No default method is set by .setup_REYTOTAL_versions().
