# ---------------------------------------------------------------------------
# UDSVERFC()
# ---------------------------------------------------------------------------

test_that("UDSVERFC returns an object inheriting both UDSVERFC and npsych_scores", {
  result <- UDSVERFC()

  expect_s3_class(result, "UDSVERFC")
  expect_s3_class(result, "npsych_scores")
})

test_that("UDSVERFC sets class with UDSVERFC first, npsych_scores second", {
  result <- UDSVERFC()

  expect_equal(class(result), c("UDSVERFC", "npsych_scores"))
})

test_that("UDSVERFC returns an object with the correct values", {
  result <- UDSVERFC(c(0, 0, 40))

  expect_equal(as.numeric(result), c(0, 0, 40))
})

test_that("UDSVERFC sets label to 'F Words'", {
  result <- UDSVERFC()

  expect_equal(attr(result, "label"), "F Words")
})

test_that("UDSVERFC sets range to c(0, 40)", {
  result <- UDSVERFC()

  expect_equal(attr(result, "range"), c(0, 40))
})

test_that("UDSVERFC sets codes with the correct values", {
  result <- UDSVERFC()
  codes <- attr(result, "codes")

  expect_true(95 %in% codes)
  expect_true(96 %in% codes)
  expect_true(97 %in% codes)
  expect_true(98 %in% codes)
  expect_true(-4 %in% codes)
})

test_that("UDSVERFC codes are named", {
  result <- UDSVERFC()

  expect_false(is.null(names(attr(result, "codes"))))
})

test_that("UDSVERFC accepts scores at the range boundaries (0 and 40)", {
  expect_no_error(UDSVERFC(c(0, 40)))
})

test_that("UDSVERFC accepts valid error code 95", {
  expect_no_error(UDSVERFC(c(0, 95)))
})

test_that("UDSVERFC accepts valid error code 96", {
  expect_no_error(UDSVERFC(c(0, 96)))
})

test_that("UDSVERFC accepts valid error code 97", {
  expect_no_error(UDSVERFC(c(0, 97)))
})

test_that("UDSVERFC accepts valid error code 98", {
  expect_no_error(UDSVERFC(c(0, 98)))
})

test_that("UDSVERFC accepts valid error code -4", {
  expect_no_error(UDSVERFC(c(0, -4)))
})

test_that("UDSVERFC errors when scores are out of range and not a code", {
  testthat::local_reproducible_output()

  expect_error(UDSVERFC(c(0, 41)), regexp = "scores")
})

test_that("UDSVERFC errors when scores is not numeric", {
  testthat::local_reproducible_output()

  expect_error(UDSVERFC(c("a", "b")), regexp = "scores")
})

test_that("UDSVERFC with no arguments returns an empty UDSVERFC object", {
  result <- UDSVERFC()

  expect_s3_class(result, "UDSVERFC")
  expect_equal(length(result), 0L)
})


# ---------------------------------------------------------------------------
# .setup_UDSVERFC_versions()
#
# .setup_*_versions() writes to the package-level .std_versions environment
# and errors on duplicate registration. The function is called once here
# outside any test_that() block; all tests below only read the registry.
# ---------------------------------------------------------------------------

lapply(
  c(.std_versions[["norms"]], .std_versions[["regression"]], .std_defaults),
  \(x) {
    rm(
      list = "UDSVERFC",
      envir = x
    )
  }
)

suppressMessages(.setup_UDSVERFC_versions())

test_that(".setup_UDSVERFC_versions registers the expected methods", {
  methods <- list_std_methods(UDSVERFC())

  expect_true("norms" %in% methods)
  expect_true("regression" %in% methods)
})

# ---------------------------------------------------------------------------
# Norms versions
# ---------------------------------------------------------------------------

test_that(".setup_UDSVERFC_versions registers the 'nacc' norms version", {
  expect_true("nacc" %in% list_method_versions(UDSVERFC(), "norms"))
})

test_that(".setup_UDSVERFC_versions registers the 'updated' norms version", {
  expect_true("updated" %in% list_method_versions(UDSVERFC(), "norms"))
})

test_that("'nacc' norms version data contains a lookup_table with m and sd columns", {
  data <- get_version_data(UDSVERFC(), "norms", "nacc")

  expect_true("lookup_table" %in% names(data))
  expect_s3_class(data$lookup_table, "data.frame")
  expect_true("m" %in% names(data$lookup_table))
  expect_true("sd" %in% names(data$lookup_table))
})

test_that("'nacc' norms version data contains the expected covar_fns", {
  data <- get_version_data(UDSVERFC(), "norms", "nacc")

  expect_true("covar_fns" %in% names(data))
  expect_true("age" %in% names(data$covar_fns))
  expect_true("educ" %in% names(data$covar_fns))
  expect_true("sex" %in% names(data$covar_fns))
})

test_that("'updated' norms version data contains a lookup_table with m and sd columns", {
  data <- get_version_data(UDSVERFC(), "norms", "updated")

  expect_true("lookup_table" %in% names(data))
  expect_s3_class(data$lookup_table, "data.frame")
  expect_true("m" %in% names(data$lookup_table))
  expect_true("sd" %in% names(data$lookup_table))
})

test_that("'updated' norms version data contains the expected covar_fns", {
  data <- get_version_data(UDSVERFC(), "norms", "updated")

  expect_true("covar_fns" %in% names(data))
  expect_true("age" %in% names(data$covar_fns))
  expect_true("educ" %in% names(data$covar_fns))
  expect_true("sex" %in% names(data$covar_fns))
})

# ---------------------------------------------------------------------------
# Regression versions
# ---------------------------------------------------------------------------

test_that(".setup_UDSVERFC_versions registers the 'updated_2024.06' regression version", {
  expect_true(
    "updated_2024.06" %in% list_method_versions(UDSVERFC(), "regression")
  )
})

test_that(".setup_UDSVERFC_versions registers the 'updated_2025.06' regression version", {
  expect_true(
    "updated_2025.06" %in% list_method_versions(UDSVERFC(), "regression")
  )
})

test_that(".setup_UDSVERFC_versions registers the 'nacc' regression version", {
  expect_true("nacc" %in% list_method_versions(UDSVERFC(), "regression"))
})

test_that("regression version data contains coefs with rmse", {
  for (version in c("updated_2024.06", "updated_2025.06", "nacc")) {
    data <- get_version_data(UDSVERFC(), "regression", version)

    expect_true("coefs" %in% names(data), info = version)
    expect_true("rmse" %in% names(data$coefs), info = version)
  }
})

test_that("regression version data contains covar_fns for age, sex, and educ", {
  for (version in c("updated_2024.06", "updated_2025.06", "nacc")) {
    data <- get_version_data(UDSVERFC(), "regression", version)

    expect_true("covar_fns" %in% names(data), info = version)
    expect_true("age" %in% names(data$covar_fns), info = version)
    expect_true("sex" %in% names(data$covar_fns), info = version)
    expect_true("educ" %in% names(data$covar_fns), info = version)
  }
})

# ---------------------------------------------------------------------------
# Default method
# ---------------------------------------------------------------------------

test_that(".setup_UDSVERFC_versions sets the default method to 'regression' / 'updated_2025.06'", {
  default <- get_std_defaults(UDSVERFC())

  expect_equal(default$method, "regression")
  expect_equal(default$version, "updated_2025.06")
})
