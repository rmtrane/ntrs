# ---------------------------------------------------------------------------
# REY6REC()
# ---------------------------------------------------------------------------

test_that("REY6REC returns an object inheriting both REY6REC and npsych_scores", {
  result <- REY6REC()

  expect_s3_class(result, "REY6REC")
  expect_s3_class(result, "npsych_scores")
})

test_that("REY6REC sets class with REY6REC first, npsych_scores second", {
  result <- REY6REC()

  expect_equal(class(result), c("REY6REC", "npsych_scores"))
})

test_that("REY6REC returns an object with the correct values", {
  result <- REY6REC(c(0, 0, 15))

  expect_equal(as.numeric(result), c(0, 0, 15))
})

test_that("REY6REC sets label to 'RAVLT Short Delay'", {
  result <- REY6REC()

  expect_equal(attr(result, "label"), "RAVLT Short Delay")
})

test_that("REY6REC sets range to c(0, 15)", {
  result <- REY6REC()

  expect_equal(attr(result, "range"), c(0, 15))
})

test_that("REY6REC sets codes with the correct values", {
  result <- REY6REC()
  codes <- attr(result, "codes")

  expect_true(88 %in% codes)
  expect_true(95 %in% codes)
  expect_true(96 %in% codes)
  expect_true(97 %in% codes)
  expect_true(98 %in% codes)
  expect_true(-4 %in% codes)
})

test_that("REY6REC codes are named", {
  result <- REY6REC()

  expect_false(is.null(names(attr(result, "codes"))))
})

test_that("REY6REC accepts scores at the range boundaries (0 and 15)", {
  expect_no_error(REY6REC(c(0, 15)))
})

test_that("REY6REC accepts valid error code 88", {
  expect_no_error(REY6REC(c(0, 88)))
})

test_that("REY6REC accepts valid error code 95", {
  expect_no_error(REY6REC(c(0, 95)))
})

test_that("REY6REC accepts valid error code 96", {
  expect_no_error(REY6REC(c(0, 96)))
})

test_that("REY6REC accepts valid error code 97", {
  expect_no_error(REY6REC(c(0, 97)))
})

test_that("REY6REC accepts valid error code 98", {
  expect_no_error(REY6REC(c(0, 98)))
})

test_that("REY6REC accepts valid error code -4", {
  expect_no_error(REY6REC(c(0, -4)))
})

test_that("REY6REC errors when scores are out of range and not a code", {
  testthat::local_reproducible_output()

  expect_error(REY6REC(c(0, 16)), regexp = "scores")
})

test_that("REY6REC errors when scores is not numeric", {
  testthat::local_reproducible_output()

  expect_error(REY6REC(c("a", "b")), regexp = "scores")
})

test_that("REY6REC with no arguments returns an empty REY6REC object", {
  result <- REY6REC()

  expect_s3_class(result, "REY6REC")
  expect_equal(length(result), 0L)
})


# ---------------------------------------------------------------------------
# .setup_REY6REC_versions()
#
# .setup_*_versions() writes to the package-level .std_versions environment
# and errors on duplicate registration. The function is called once here
# outside any test_that() block; all tests below only read the registry.
# ---------------------------------------------------------------------------

lapply(
  c(.std_versions[["norms"]], .std_versions[["regression"]], .std_defaults),
  \(x) {
    suppressWarnings(rm(
      list = "REY6REC",
      envir = x
    ))
  }
)

suppressMessages(.setup_REY6REC_versions())


test_that(".setup_REY6REC_versions registers the expected methods", {
  methods <- list_std_methods(REY6REC())

  expect_true("norms" %in% methods)
  expect_true("regression" %in% methods)
})

# ---------------------------------------------------------------------------
# Norms versions
# ---------------------------------------------------------------------------

test_that(".setup_REY6REC_versions registers the 'ravlt_trials' norms version", {
  expect_true("ravlt_trials" %in% list_method_versions(REY6REC(), "norms"))
})

test_that("'ravlt_trials' norms version data contains a lookup_table with m and sd columns", {
  data <- get_version_data(REY6REC(), "norms", "ravlt_trials")

  expect_true("lookup_table" %in% names(data))
  expect_s3_class(data$lookup_table, "data.frame")
  expect_true("m" %in% names(data$lookup_table))
  expect_true("sd" %in% names(data$lookup_table))
})

test_that("'ravlt_trials' norms version data contains the expected covar_fns", {
  data <- get_version_data(REY6REC(), "norms", "ravlt_trials")

  expect_true("covar_fns" %in% names(data))
  expect_true("age" %in% names(data$covar_fns))
})

# ---------------------------------------------------------------------------
# Regression versions
# ---------------------------------------------------------------------------

test_that(".setup_REY6REC_versions registers the 'updated_2024.06' regression version", {
  expect_true(
    "updated_2024.06" %in% list_method_versions(REY6REC(), "regression")
  )
})

test_that(".setup_REY6REC_versions registers the 'updated_2025.06' regression version", {
  expect_true(
    "updated_2025.06" %in% list_method_versions(REY6REC(), "regression")
  )
})

test_that("regression version data contains coefs with rmse", {
  for (version in c("updated_2024.06", "updated_2025.06")) {
    data <- get_version_data(REY6REC(), "regression", version)

    expect_true("coefs" %in% names(data), info = version)
    expect_true("rmse" %in% names(data$coefs), info = version)
  }
})

test_that("regression version data contains covar_fns for age, sex, and educ", {
  for (version in c("updated_2024.06", "updated_2025.06")) {
    data <- get_version_data(REY6REC(), "regression", version)

    expect_true("covar_fns" %in% names(data), info = version)
    expect_true("age" %in% names(data$covar_fns), info = version)
    expect_true("sex" %in% names(data$covar_fns), info = version)
    expect_true("educ" %in% names(data$covar_fns), info = version)
  }
})

# No default method is set by .setup_REY6REC_versions().
