# ---------------------------------------------------------------------------
# MEMUNITS()
# ---------------------------------------------------------------------------

test_that("MEMUNITS returns an object inheriting both MEMUNITS and npsych_scores", {
  result <- MEMUNITS()

  expect_s3_class(result, "MEMUNITS")
  expect_s3_class(result, "npsych_scores")
})

test_that("MEMUNITS sets class with MEMUNITS first, npsych_scores second", {
  result <- MEMUNITS()

  expect_equal(class(result), c("MEMUNITS", "npsych_scores"))
})

test_that("MEMUNITS returns an object with the correct values", {
  result <- MEMUNITS(c(0, 0, 25))

  expect_equal(as.numeric(result), c(0, 0, 25))
})

test_that("MEMUNITS sets label to 'Logical Memory, Delayed'", {
  result <- MEMUNITS()

  expect_equal(attr(result, "label"), "Logical Memory, Delayed")
})

test_that("MEMUNITS sets range to c(0, 25)", {
  result <- MEMUNITS()

  expect_equal(attr(result, "range"), c(0, 25))
})

test_that("MEMUNITS sets codes with the correct values", {
  result <- MEMUNITS()
  codes <- attr(result, "codes")

  expect_true(95 %in% codes)
  expect_true(96 %in% codes)
  expect_true(97 %in% codes)
  expect_true(98 %in% codes)
  expect_true(-4 %in% codes)
})

test_that("MEMUNITS codes are named", {
  result <- MEMUNITS()

  expect_false(is.null(names(attr(result, "codes"))))
})

test_that("MEMUNITS accepts scores at the range boundaries (0 and 25)", {
  expect_no_error(MEMUNITS(c(0, 25)))
})

test_that("MEMUNITS accepts valid error code 95", {
  expect_no_error(MEMUNITS(c(0, 95)))
})

test_that("MEMUNITS accepts valid error code 96", {
  expect_no_error(MEMUNITS(c(0, 96)))
})

test_that("MEMUNITS accepts valid error code 97", {
  expect_no_error(MEMUNITS(c(0, 97)))
})

test_that("MEMUNITS accepts valid error code 98", {
  expect_no_error(MEMUNITS(c(0, 98)))
})

test_that("MEMUNITS accepts valid error code -4", {
  expect_no_error(MEMUNITS(c(0, -4)))
})

test_that("MEMUNITS errors when scores are out of range and not a code", {
  testthat::local_reproducible_output()

  expect_error(MEMUNITS(c(0, 26)), regexp = "scores")
})

test_that("MEMUNITS errors when scores is not numeric", {
  testthat::local_reproducible_output()

  expect_error(MEMUNITS(c("a", "b")), regexp = "scores")
})

test_that("MEMUNITS with no arguments returns an empty MEMUNITS object", {
  result <- MEMUNITS()

  expect_s3_class(result, "MEMUNITS")
  expect_equal(length(result), 0L)
})


# ---------------------------------------------------------------------------
# .setup_MEMUNITS_versions()
#
# .setup_*_versions() writes to the package-level .std_versions environment
# and errors on duplicate registration. The function is called once here
# outside any test_that() block; all tests below only read the registry.
# ---------------------------------------------------------------------------

rm(list = ls(envir = .std_versions), envir = .std_versions)
.setup_MEMUNITS_versions()

test_that(".setup_MEMUNITS_versions registers the expected methods", {
  methods <- list_std_methods(MEMUNITS())

  expect_true("regression" %in% methods)
})

# ---------------------------------------------------------------------------
# Regression versions
# ---------------------------------------------------------------------------

test_that(".setup_MEMUNITS_versions registers the 'updated_2024.06' regression version", {
  expect_true(
    "updated_2024.06" %in% list_method_versions(MEMUNITS(), "regression")
  )
})

test_that(".setup_MEMUNITS_versions registers the 'updated_2025.06' regression version", {
  expect_true(
    "updated_2025.06" %in% list_method_versions(MEMUNITS(), "regression")
  )
})

test_that(".setup_MEMUNITS_versions registers the 'nacc_legacy' regression version", {
  expect_true("nacc_legacy" %in% list_method_versions(MEMUNITS(), "regression"))
})

test_that("regression version data contains coefs with rmse", {
  for (version in c("updated_2024.06", "updated_2025.06", "nacc_legacy")) {
    data <- get_version_data(MEMUNITS(), "regression", version)

    expect_true("coefs" %in% names(data), info = version)
    expect_true("rmse" %in% names(data$coefs), info = version)
  }
})

test_that("regression version data contains covar_fns for age, sex, and educ", {
  for (version in c("updated_2024.06", "updated_2025.06", "nacc_legacy")) {
    data <- get_version_data(MEMUNITS(), "regression", version)

    expect_true("covar_fns" %in% names(data), info = version)
    expect_true("age" %in% names(data$covar_fns), info = version)
    expect_true("sex" %in% names(data$covar_fns), info = version)
    expect_true("educ" %in% names(data$covar_fns), info = version)
  }
})

# ---------------------------------------------------------------------------
# Default method
# ---------------------------------------------------------------------------

test_that(".setup_MEMUNITS_versions sets the default method to 'regression' / 'nacc_legacy'", {
  default <- get_std_defaults(MEMUNITS())

  expect_equal(default$method, "regression")
  expect_equal(default$version, "nacc_legacy")
})
