# ---------------------------------------------------------------------------
# TRAILB()
# ---------------------------------------------------------------------------

test_that("TRAILB returns an object inheriting both TRAILB and test_scores", {
  result <- TRAILB()

  expect_s3_class(result, "TRAILB")
  expect_s3_class(result, "test_scores")
})

test_that("TRAILB sets class with TRAILB first, test_scores second", {
  result <- TRAILB()

  expect_equal(class(result), c("TRAILB", "test_scores"))
})

test_that("TRAILB returns an object with the correct values", {
  result <- TRAILB(c(0, 0, 300))

  expect_equal(as.numeric(result), c(0, 0, 300))
})

test_that("TRAILB sets label to 'Trailmaking Part B'", {
  result <- TRAILB()

  expect_equal(attr(result, "label"), "Trailmaking Part B")
})

test_that("TRAILB sets range to c(0, 300)", {
  result <- TRAILB()

  expect_equal(attr(result, "range"), c(0, 300))
})

test_that("TRAILB sets codes with the correct values", {
  result <- TRAILB()
  codes <- attr(result, "codes")

  expect_true(995 %in% codes)
  expect_true(996 %in% codes)
  expect_true(997 %in% codes)
  expect_true(998 %in% codes)
  expect_true(-4 %in% codes)
})

test_that("TRAILB codes are named", {
  result <- TRAILB()

  expect_false(is.null(names(attr(result, "codes"))))
})

test_that("TRAILB accepts scores at the range boundaries (0 and 300)", {
  expect_no_error(TRAILB(c(0, 300)))
})

test_that("TRAILB accepts valid error code 995", {
  expect_no_error(TRAILB(c(0, 995)))
})

test_that("TRAILB accepts valid error code 996", {
  expect_no_error(TRAILB(c(0, 996)))
})

test_that("TRAILB accepts valid error code 997", {
  expect_no_error(TRAILB(c(0, 997)))
})

test_that("TRAILB accepts valid error code 998", {
  expect_no_error(TRAILB(c(0, 998)))
})

test_that("TRAILB accepts valid error code -4", {
  expect_no_error(TRAILB(c(0, -4)))
})

test_that("TRAILB errors when scores are out of range and not a code", {
  testthat::local_reproducible_output()

  expect_error(TRAILB(c(0, 301)), regexp = "scores")
})

test_that("TRAILB errors when scores is not numeric", {
  testthat::local_reproducible_output()

  expect_error(TRAILB(c("a", "b")), regexp = "scores")
})

test_that("TRAILB with no arguments returns an empty TRAILB object", {
  result <- TRAILB()

  expect_s3_class(result, "TRAILB")
  expect_equal(length(result), 0L)
})


# ---------------------------------------------------------------------------
# .setup_TRAILB_versions()
#
# .setup_*_versions() writes to the package-level .std_versions environment
# and errors on duplicate registration. The function is called once here
# outside any test_that() block; all tests below only read the registry.
# ---------------------------------------------------------------------------

rm(list = ls(envir = .std_versions), envir = .std_versions)
.setup_TRAILB_versions()

test_that(".setup_TRAILB_versions registers the expected methods", {
  methods <- get_std_methods(TRAILB())

  expect_true("norms" %in% methods)
  expect_true("regression" %in% methods)
})

# ---------------------------------------------------------------------------
# Norms versions
# ---------------------------------------------------------------------------

test_that(".setup_TRAILB_versions registers the 'nacc' norms version", {
  expect_true("nacc" %in% get_versions(TRAILB(), "norms"))
})

test_that(".setup_TRAILB_versions registers the 'updated' norms version", {
  expect_true("updated" %in% get_versions(TRAILB(), "norms"))
})

test_that("'nacc' norms version data contains a lookup_table with m and sd columns", {
  data <- get_version_data(TRAILB(), "norms", "nacc")

  expect_true("lookup_table" %in% names(data))
  expect_s3_class(data$lookup_table, "data.frame")
  expect_true("m" %in% names(data$lookup_table))
  expect_true("sd" %in% names(data$lookup_table))
})

test_that("'nacc' norms version data contains the expected covariate_prep_funs", {
  data <- get_version_data(TRAILB(), "norms", "nacc")

  expect_true("covariate_prep_funs" %in% names(data))
  expect_true("age" %in% names(data$covariate_prep_funs))
  expect_true("educ" %in% names(data$covariate_prep_funs))
  expect_true("sex" %in% names(data$covariate_prep_funs))
})

test_that("'updated' norms version data contains a lookup_table with m and sd columns", {
  data <- get_version_data(TRAILB(), "norms", "updated")

  expect_true("lookup_table" %in% names(data))
  expect_s3_class(data$lookup_table, "data.frame")
  expect_true("m" %in% names(data$lookup_table))
  expect_true("sd" %in% names(data$lookup_table))
})

test_that("'updated' norms version data contains the expected covariate_prep_funs", {
  data <- get_version_data(TRAILB(), "norms", "updated")

  expect_true("covariate_prep_funs" %in% names(data))
  expect_true("age" %in% names(data$covariate_prep_funs))
  expect_true("educ" %in% names(data$covariate_prep_funs))
  expect_true("sex" %in% names(data$covariate_prep_funs))
})

# ---------------------------------------------------------------------------
# Regression versions
# ---------------------------------------------------------------------------

test_that(".setup_TRAILB_versions registers the 'updated_2024.06' regression version", {
  expect_true("updated_2024.06" %in% get_versions(TRAILB(), "regression"))
})

test_that(".setup_TRAILB_versions registers the 'updated_2025.06' regression version", {
  expect_true("updated_2025.06" %in% get_versions(TRAILB(), "regression"))
})

test_that(".setup_TRAILB_versions registers the 'nacc' regression version", {
  expect_true("nacc" %in% get_versions(TRAILB(), "regression"))
})

test_that("regression version data contains coefs with rmse", {
  for (version in c("updated_2024.06", "updated_2025.06", "nacc")) {
    data <- get_version_data(TRAILB(), "regression", version)

    expect_true("coefs" %in% names(data), info = version)
    expect_true("rmse" %in% names(data$coefs), info = version)
  }
})

test_that("regression version data contains covariate_prep_funs for age, sex, and educ", {
  for (version in c("updated_2024.06", "updated_2025.06", "nacc")) {
    data <- get_version_data(TRAILB(), "regression", version)

    expect_true("covariate_prep_funs" %in% names(data), info = version)
    expect_true("age" %in% names(data$covariate_prep_funs), info = version)
    expect_true("sex" %in% names(data$covariate_prep_funs), info = version)
    expect_true("educ" %in% names(data$covariate_prep_funs), info = version)
  }
})

# ---------------------------------------------------------------------------
# Default method
# ---------------------------------------------------------------------------

test_that(".setup_TRAILB_versions sets the default method to 'regression' / 'updated_2025.06'", {
  default <- get_default_method(TRAILB())

  expect_equal(default$method, "regression")
  expect_equal(default$version, "updated_2025.06")
})
