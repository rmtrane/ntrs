# ---------------------------------------------------------------------------
# DIGFORSL()
# ---------------------------------------------------------------------------

test_that("DIGFORSL returns an object inheriting both DIGFORSL and npsych_scores", {
  result <- DIGFORSL()

  expect_s3_class(result, "DIGFORSL")
  expect_s3_class(result, "npsych_scores")
})

test_that("DIGFORSL sets class with DIGFORSL first, npsych_scores second", {
  result <- DIGFORSL()

  expect_equal(class(result), c("DIGFORSL", "npsych_scores"))
})

test_that("DIGFORSL returns an object with the correct values", {
  result <- DIGFORSL(c(0, 0, 9))

  expect_equal(as.numeric(result), c(0, 0, 9))
})

test_that("DIGFORSL sets label to 'Number Span Forward - Span Length'", {
  result <- DIGFORSL()

  expect_equal(attr(result, "label"), "Number Span Forward - Span Length")
})

test_that("DIGFORSL sets range to c(0, 9)", {
  result <- DIGFORSL()

  expect_equal(attr(result, "range"), c(0, 9))
})

test_that("DIGFORSL sets codes with the correct values", {
  result <- DIGFORSL()
  codes <- attr(result, "codes")

  expect_true(95 %in% codes)
  expect_true(96 %in% codes)
  expect_true(97 %in% codes)
  expect_true(98 %in% codes)
  expect_true(-4 %in% codes)
})

test_that("DIGFORSL codes are named", {
  result <- DIGFORSL()

  expect_false(is.null(names(attr(result, "codes"))))
})

test_that("DIGFORSL accepts scores at the range boundaries (0 and 9)", {
  expect_no_error(DIGFORSL(c(0, 9)))
})

test_that("DIGFORSL accepts valid error code 95", {
  expect_no_error(DIGFORSL(c(0, 95)))
})

test_that("DIGFORSL accepts valid error code 96", {
  expect_no_error(DIGFORSL(c(0, 96)))
})

test_that("DIGFORSL accepts valid error code 97", {
  expect_no_error(DIGFORSL(c(0, 97)))
})

test_that("DIGFORSL accepts valid error code 98", {
  expect_no_error(DIGFORSL(c(0, 98)))
})

test_that("DIGFORSL accepts valid error code -4", {
  expect_no_error(DIGFORSL(c(0, -4)))
})

test_that("DIGFORSL errors when scores are out of range and not a code", {
  testthat::local_reproducible_output()

  expect_error(DIGFORSL(c(0, 10)), regexp = "scores")
})

test_that("DIGFORSL errors when scores is not numeric", {
  testthat::local_reproducible_output()

  expect_error(DIGFORSL(c("a", "b")), regexp = "scores")
})

test_that("DIGFORSL with no arguments returns an empty DIGFORSL object", {
  result <- DIGFORSL()

  expect_s3_class(result, "DIGFORSL")
  expect_equal(length(result), 0L)
})


# ---------------------------------------------------------------------------
# .setup_DIGFORSL_versions()
#
# .setup_*_versions() writes to the package-level .std_versions environment
# and errors on duplicate registration. The function is called once here
# outside any test_that() block; all tests below only read the registry.
# ---------------------------------------------------------------------------

lapply(
  c(.std_versions[["norms"]], .std_versions[["regression"]], .std_defaults),
  \(x) {
    rm(
      list = "DIGFORSL",
      envir = x
    )
  }
)

suppressMessages(.setup_DIGFORSL_versions())

test_that(".setup_DIGFORSL_versions registers the expected methods", {
  methods <- list_std_methods(DIGFORSL())

  expect_true("norms" %in% methods)
  expect_true("regression" %in% methods)
})

# ---------------------------------------------------------------------------
# Norms versions
# ---------------------------------------------------------------------------

test_that(".setup_DIGFORSL_versions registers the 'nacc' norms version", {
  expect_true("nacc" %in% list_method_versions(DIGFORSL(), "norms"))
})

test_that(".setup_DIGFORSL_versions registers the 'updated' norms version", {
  expect_true("updated" %in% list_method_versions(DIGFORSL(), "norms"))
})

test_that("'nacc' norms version data contains a lookup_table with m and sd columns", {
  data <- get_version_data(DIGFORSL(), "norms", "nacc")

  expect_true("lookup_table" %in% names(data))
  expect_s3_class(data$lookup_table, "data.frame")
  expect_true("m" %in% names(data$lookup_table))
  expect_true("sd" %in% names(data$lookup_table))
})

test_that("'nacc' norms version data contains the expected covar_fns", {
  data <- get_version_data(DIGFORSL(), "norms", "nacc")

  expect_true("covar_fns" %in% names(data))
  expect_true("age" %in% names(data$covar_fns))
  expect_true("educ" %in% names(data$covar_fns))
  expect_true("sex" %in% names(data$covar_fns))
})

test_that("'updated' norms version data contains a lookup_table with m and sd columns", {
  data <- get_version_data(DIGFORSL(), "norms", "updated")

  expect_true("lookup_table" %in% names(data))
  expect_s3_class(data$lookup_table, "data.frame")
  expect_true("m" %in% names(data$lookup_table))
  expect_true("sd" %in% names(data$lookup_table))
})

test_that("'updated' norms version data contains the expected covar_fns", {
  data <- get_version_data(DIGFORSL(), "norms", "updated")

  expect_true("covar_fns" %in% names(data))
  expect_true("age" %in% names(data$covar_fns))
  expect_true("educ" %in% names(data$covar_fns))
  expect_true("sex" %in% names(data$covar_fns))
})

# ---------------------------------------------------------------------------
# Regression versions
# ---------------------------------------------------------------------------

test_that(".setup_DIGFORSL_versions registers the 'updated_2024.06' regression version", {
  expect_true(
    "updated_2024.06" %in% list_method_versions(DIGFORSL(), "regression")
  )
})

test_that(".setup_DIGFORSL_versions registers the 'updated_2025.06' regression version", {
  expect_true(
    "updated_2025.06" %in% list_method_versions(DIGFORSL(), "regression")
  )
})

test_that(".setup_DIGFORSL_versions registers the 'nacc' regression version", {
  expect_true("nacc" %in% list_method_versions(DIGFORSL(), "regression"))
})

test_that("regression version data contains coefs with rmse", {
  for (version in c("updated_2024.06", "updated_2025.06", "nacc")) {
    data <- get_version_data(DIGFORSL(), "regression", version)

    expect_true("coefs" %in% names(data), info = version)
    expect_true("rmse" %in% names(data$coefs), info = version)
  }
})

test_that("regression version data contains covar_fns for age, sex, and educ", {
  for (version in c("updated_2024.06", "updated_2025.06", "nacc")) {
    data <- get_version_data(DIGFORSL(), "regression", version)

    expect_true("covar_fns" %in% names(data), info = version)
    expect_true("age" %in% names(data$covar_fns), info = version)
    expect_true("sex" %in% names(data$covar_fns), info = version)
    expect_true("educ" %in% names(data$covar_fns), info = version)
  }
})

# ---------------------------------------------------------------------------
# Default method
# ---------------------------------------------------------------------------

test_that(".setup_DIGFORSL_versions sets the default method to 'regression' / 'updated_2025.06'", {
  default <- get_std_defaults(DIGFORSL())

  expect_equal(default$method, "regression")
  expect_equal(default$version, "updated_2025.06")
})
