# ---------------------------------------------------------------------------
# BOSTON()
# ---------------------------------------------------------------------------

test_that("BOSTON returns an object inheriting both BOSTON and npsych_scores", {
  result <- BOSTON()

  expect_s3_class(result, "BOSTON")
  expect_s3_class(result, "npsych_scores")
})

test_that("BOSTON sets class with BOSTON first, npsych_scores second", {
  result <- BOSTON()

  expect_equal(class(result), c("BOSTON", "npsych_scores"))
})

test_that("BOSTON returns an object with the correct values", {
  result <- BOSTON(c(0, 0, 30))

  expect_equal(as.numeric(result), c(0, 0, 30))
})

test_that("BOSTON sets label to 'Boston Naming Test'", {
  result <- BOSTON()

  expect_equal(attr(result, "label"), "Boston Naming Test")
})

test_that("BOSTON sets range to c(0, 30)", {
  result <- BOSTON()

  expect_equal(attr(result, "range"), c(0, 30))
})

test_that("BOSTON sets codes with the correct values", {
  result <- BOSTON()
  codes <- attr(result, "codes")

  expect_true(95 %in% codes)
  expect_true(96 %in% codes)
  expect_true(97 %in% codes)
  expect_true(98 %in% codes)
  expect_true(-4 %in% codes)
})

test_that("BOSTON codes are named", {
  result <- BOSTON()

  expect_false(is.null(names(attr(result, "codes"))))
})

test_that("BOSTON accepts scores at the range boundaries (0 and 30)", {
  expect_no_error(BOSTON(c(0, 30)))
})

test_that("BOSTON accepts valid error code 95", {
  expect_no_error(BOSTON(c(0, 95)))
})

test_that("BOSTON accepts valid error code 96", {
  expect_no_error(BOSTON(c(0, 96)))
})

test_that("BOSTON accepts valid error code 97", {
  expect_no_error(BOSTON(c(0, 97)))
})

test_that("BOSTON accepts valid error code 98", {
  expect_no_error(BOSTON(c(0, 98)))
})

test_that("BOSTON accepts valid error code -4", {
  expect_no_error(BOSTON(c(0, -4)))
})

test_that("BOSTON errors when scores are out of range and not a code", {
  testthat::local_reproducible_output()

  expect_error(BOSTON(c(0, 31)), regexp = "scores")
})

test_that("BOSTON errors when scores is not numeric", {
  testthat::local_reproducible_output()

  expect_error(BOSTON(c("a", "b")), regexp = "scores")
})

test_that("BOSTON with no arguments returns an empty BOSTON object", {
  result <- BOSTON()

  expect_s3_class(result, "BOSTON")
  expect_equal(length(result), 0L)
})


# ---------------------------------------------------------------------------
# .setup_BOSTON_versions()
#
# .setup_*_versions() writes to the package-level .std_versions environment
# and errors on duplicate registration. The function is called once here
# outside any test_that() block; all tests below only read the registry.
# ---------------------------------------------------------------------------

rm(list = ls(envir = .std_versions), envir = .std_versions)
.setup_BOSTON_versions()

test_that(".setup_BOSTON_versions registers the expected methods", {
  methods <- list_std_methods(BOSTON())

  expect_true("regression" %in% methods)
})

# ---------------------------------------------------------------------------
# Regression versions
# ---------------------------------------------------------------------------

test_that(".setup_BOSTON_versions registers the 'updated_2024.06' regression version", {
  expect_true(
    "updated_2024.06" %in% list_method_versions(BOSTON(), "regression")
  )
})

test_that(".setup_BOSTON_versions registers the 'updated_2025.06' regression version", {
  expect_true(
    "updated_2025.06" %in% list_method_versions(BOSTON(), "regression")
  )
})

test_that(".setup_BOSTON_versions registers the 'nacc_legacy' regression version", {
  expect_true("nacc_legacy" %in% list_method_versions(BOSTON(), "regression"))
})

test_that("regression version data contains coefs with rmse", {
  for (version in c("updated_2024.06", "updated_2025.06", "nacc_legacy")) {
    data <- get_version_data(BOSTON(), "regression", version)

    expect_true("coefs" %in% names(data), info = version)
    expect_true("rmse" %in% names(data$coefs), info = version)
  }
})

test_that("regression version data contains covar_fns for age, sex, and educ", {
  for (version in c("updated_2024.06", "updated_2025.06", "nacc_legacy")) {
    data <- get_version_data(BOSTON(), "regression", version)

    expect_true("covar_fns" %in% names(data), info = version)
    expect_true("age" %in% names(data$covar_fns), info = version)
    expect_true("sex" %in% names(data$covar_fns), info = version)
    expect_true("educ" %in% names(data$covar_fns), info = version)
  }
})

# ---------------------------------------------------------------------------
# Default method
# ---------------------------------------------------------------------------

test_that(".setup_BOSTON_versions sets the default method to 'regression' / 'nacc_legacy'", {
  default <- get_std_defaults(BOSTON())

  expect_equal(default$method, "regression")
  expect_equal(default$version, "nacc_legacy")
})
