# ---------------------------------------------------------------------------
# WAIS()
# ---------------------------------------------------------------------------

# NOTE: .setup_WAIS_versions() does not call set_default_method().
# No default method test is included.

test_that("WAIS returns an object inheriting both WAIS and test_scores", {
  result <- WAIS()

  expect_s3_class(result, "WAIS")
  expect_s3_class(result, "test_scores")
})

test_that("WAIS sets class with WAIS first, test_scores second", {
  result <- WAIS()

  expect_equal(class(result), c("WAIS", "test_scores"))
})

test_that("WAIS returns an object with the correct values", {
  result <- WAIS(c(0, 0, 93))

  expect_equal(as.numeric(result), c(0, 0, 93))
})

test_that("WAIS sets label to 'WAIS-R Digit Symbol'", {
  result <- WAIS()

  expect_equal(attr(result, "label"), "WAIS-R Digit Symbol")
})

test_that("WAIS sets range to c(0, 93)", {
  result <- WAIS()

  expect_equal(attr(result, "range"), c(0, 93))
})

test_that("WAIS sets codes with the correct values", {
  result <- WAIS()
  codes <- attr(result, "codes")

  expect_true(95 %in% codes)
  expect_true(96 %in% codes)
  expect_true(97 %in% codes)
  expect_true(98 %in% codes)
  expect_true(-4 %in% codes)
})

test_that("WAIS codes are named", {
  result <- WAIS()

  expect_false(is.null(names(attr(result, "codes"))))
})

test_that("WAIS accepts scores at the range boundaries (0 and 93)", {
  expect_no_error(WAIS(c(0, 93)))
})

test_that("WAIS accepts valid error code 95", {
  expect_no_error(WAIS(c(0, 95)))
})

test_that("WAIS accepts valid error code 96", {
  expect_no_error(WAIS(c(0, 96)))
})

test_that("WAIS accepts valid error code 97", {
  expect_no_error(WAIS(c(0, 97)))
})

test_that("WAIS accepts valid error code 98", {
  expect_no_error(WAIS(c(0, 98)))
})

test_that("WAIS accepts valid error code -4", {
  expect_no_error(WAIS(c(0, -4)))
})

test_that("WAIS errors when scores are out of range and not a code", {
  testthat::local_reproducible_output()

  expect_error(WAIS(c(0, 94)), regexp = "scores")
})

test_that("WAIS errors when scores is not numeric", {
  testthat::local_reproducible_output()

  expect_error(WAIS(c("a", "b")), regexp = "scores")
})

test_that("WAIS with no arguments returns an empty WAIS object", {
  result <- WAIS()

  expect_s3_class(result, "WAIS")
  expect_equal(length(result), 0L)
})


# ---------------------------------------------------------------------------
# .setup_WAIS_versions()
#
# .setup_*_versions() writes to the package-level .std_versions environment
# and errors on duplicate registration. The function is called once here
# outside any test_that() block; all tests below only read the registry.
# ---------------------------------------------------------------------------

rm(list = ls(envir = .std_versions), envir = .std_versions)
.setup_WAIS_versions()

test_that(".setup_WAIS_versions registers the expected methods", {
  methods <- get_std_methods(WAIS())

  expect_true("regression" %in% methods)
})

# ---------------------------------------------------------------------------
# Regression versions
# ---------------------------------------------------------------------------

test_that(".setup_WAIS_versions registers the 'updated_2024.06' regression version", {
  expect_true("updated_2024.06" %in% get_versions(WAIS(), "regression"))
})

test_that(".setup_WAIS_versions registers the 'updated_2025.06' regression version", {
  expect_true("updated_2025.06" %in% get_versions(WAIS(), "regression"))
})

test_that("regression version data contains coefs with rmse", {
  for (version in c("updated_2024.06", "updated_2025.06")) {
    data <- get_version_data(WAIS(), "regression", version)

    expect_true("coefs" %in% names(data), info = version)
    expect_true("rmse" %in% names(data$coefs), info = version)
  }
})

test_that("regression version data contains covariate_prep_funs for age, sex, and educ", {
  for (version in c("updated_2024.06", "updated_2025.06")) {
    data <- get_version_data(WAIS(), "regression", version)

    expect_true("covariate_prep_funs" %in% names(data), info = version)
    expect_true("age" %in% names(data$covariate_prep_funs), info = version)
    expect_true("sex" %in% names(data$covariate_prep_funs), info = version)
    expect_true("educ" %in% names(data$covariate_prep_funs), info = version)
  }
})

# No default method is set by .setup_WAIS_versions().
