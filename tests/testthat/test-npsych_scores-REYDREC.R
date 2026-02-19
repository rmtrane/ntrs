# ---------------------------------------------------------------------------
# REYDREC()
# ---------------------------------------------------------------------------

test_that("REYDREC returns an object inheriting both REYDREC and npsych_scores", {
  result <- REYDREC()

  expect_s3_class(result, "REYDREC")
  expect_s3_class(result, "npsych_scores")
})

test_that("REYDREC sets class with REYDREC first, npsych_scores second", {
  result <- REYDREC()

  expect_equal(class(result), c("REYDREC", "npsych_scores"))
})

test_that("REYDREC returns an object with the correct values", {
  result <- REYDREC(c(0, 0, 15))

  expect_equal(as.numeric(result), c(0, 0, 15))
})

test_that("REYDREC sets label to 'RAVLT Long Delay'", {
  result <- REYDREC()

  expect_equal(attr(result, "label"), "RAVLT Long Delay")
})

test_that("REYDREC sets range to c(0, 15)", {
  result <- REYDREC()

  expect_equal(attr(result, "range"), c(0, 15))
})

test_that("REYDREC sets codes with the correct values", {
  result <- REYDREC()
  codes <- attr(result, "codes")

  expect_true(88 %in% codes)
  expect_true(95 %in% codes)
  expect_true(96 %in% codes)
  expect_true(97 %in% codes)
  expect_true(98 %in% codes)
  expect_true(-4 %in% codes)
})

test_that("REYDREC codes are named", {
  result <- REYDREC()

  expect_false(is.null(names(attr(result, "codes"))))
})

test_that("REYDREC accepts scores at the range boundaries (0 and 15)", {
  expect_no_error(REYDREC(c(0, 15)))
})

test_that("REYDREC accepts valid error code 88", {
  expect_no_error(REYDREC(c(0, 88)))
})

test_that("REYDREC accepts valid error code 95", {
  expect_no_error(REYDREC(c(0, 95)))
})

test_that("REYDREC accepts valid error code 96", {
  expect_no_error(REYDREC(c(0, 96)))
})

test_that("REYDREC accepts valid error code 97", {
  expect_no_error(REYDREC(c(0, 97)))
})

test_that("REYDREC accepts valid error code 98", {
  expect_no_error(REYDREC(c(0, 98)))
})

test_that("REYDREC accepts valid error code -4", {
  expect_no_error(REYDREC(c(0, -4)))
})

test_that("REYDREC errors when scores are out of range and not a code", {
  testthat::local_reproducible_output()

  expect_error(REYDREC(c(0, 16)), regexp = "scores")
})

test_that("REYDREC errors when scores is not numeric", {
  testthat::local_reproducible_output()

  expect_error(REYDREC(c("a", "b")), regexp = "scores")
})

test_that("REYDREC with no arguments returns an empty REYDREC object", {
  result <- REYDREC()

  expect_s3_class(result, "REYDREC")
  expect_equal(length(result), 0L)
})


# ---------------------------------------------------------------------------
# .setup_REYDREC_versions()
#
# .setup_*_versions() writes to the package-level .std_versions environment
# and errors on duplicate registration. The function is called once here
# outside any test_that() block; all tests below only read the registry.
# ---------------------------------------------------------------------------

rm(list = ls(envir = .std_versions), envir = .std_versions)
.setup_REYDREC_versions()

test_that(".setup_REYDREC_versions registers the expected methods", {
  methods <- list_std_methods(REYDREC())

  expect_true("norms" %in% methods)
  expect_true("regression" %in% methods)
})

# ---------------------------------------------------------------------------
# Norms versions
# ---------------------------------------------------------------------------

test_that(".setup_REYDREC_versions registers the 'ravlt_trials' norms version", {
  expect_true("ravlt_trials" %in% list_method_versions(REYDREC(), "norms"))
})

test_that("'ravlt_trials' norms version data contains a lookup_table with m and sd columns", {
  data <- get_version_data(REYDREC(), "norms", "ravlt_trials")

  expect_true("lookup_table" %in% names(data))
  expect_s3_class(data$lookup_table, "data.frame")
  expect_true("m" %in% names(data$lookup_table))
  expect_true("sd" %in% names(data$lookup_table))
})

test_that("'ravlt_trials' norms version data contains the expected covar_fns", {
  data <- get_version_data(REYDREC(), "norms", "ravlt_trials")

  expect_true("covar_fns" %in% names(data))
  expect_true("age" %in% names(data$covar_fns))
})

# ---------------------------------------------------------------------------
# Regression versions
# ---------------------------------------------------------------------------

test_that(".setup_REYDREC_versions registers the 'updated_2024.06' regression version", {
  expect_true(
    "updated_2024.06" %in% list_method_versions(REYDREC(), "regression")
  )
})

test_that(".setup_REYDREC_versions registers the 'updated_2025.06' regression version", {
  expect_true(
    "updated_2025.06" %in% list_method_versions(REYDREC(), "regression")
  )
})

test_that("regression version data contains coefs with rmse", {
  for (version in c("updated_2024.06", "updated_2025.06")) {
    data <- get_version_data(REYDREC(), "regression", version)

    expect_true("coefs" %in% names(data), info = version)
    expect_true("rmse" %in% names(data$coefs), info = version)
  }
})

test_that("regression version data contains covar_fns for age, sex, and educ", {
  for (version in c("updated_2024.06", "updated_2025.06")) {
    data <- get_version_data(REYDREC(), "regression", version)

    expect_true("covar_fns" %in% names(data), info = version)
    expect_true("age" %in% names(data$covar_fns), info = version)
    expect_true("sex" %in% names(data$covar_fns), info = version)
    expect_true("educ" %in% names(data$covar_fns), info = version)
  }
})

# No default method is set by .setup_REYDREC_versions().
