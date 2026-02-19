# ---------------------------------------------------------------------------
# MOCATOTS()
# ---------------------------------------------------------------------------

test_that("MOCATOTS returns an object inheriting both MOCATOTS and npsych_scores", {
  result <- MOCATOTS()

  expect_s3_class(result, "MOCATOTS")
  expect_s3_class(result, "npsych_scores")
})

test_that("MOCATOTS sets class with MOCATOTS first, npsych_scores second", {
  result <- MOCATOTS()

  expect_equal(class(result), c("MOCATOTS", "npsych_scores"))
})

test_that("MOCATOTS returns an object with the correct values", {
  result <- MOCATOTS(c(0, 0, 30))

  expect_equal(as.numeric(result), c(0, 0, 30))
})

test_that("MOCATOTS sets label to 'MoCA'", {
  result <- MOCATOTS()

  expect_equal(attr(result, "label"), "MoCA")
})

test_that("MOCATOTS sets range to c(0, 30)", {
  result <- MOCATOTS()

  expect_equal(attr(result, "range"), c(0, 30))
})

test_that("MOCATOTS sets codes with the correct values", {
  result <- MOCATOTS()
  codes <- attr(result, "codes")

  expect_true(-4 %in% codes)
  expect_true(88 %in% codes)
})

test_that("MOCATOTS codes are named", {
  result <- MOCATOTS()

  expect_false(is.null(names(attr(result, "codes"))))
})

test_that("MOCATOTS accepts scores at the range boundaries (0 and 30)", {
  expect_no_error(MOCATOTS(c(0, 30)))
})

test_that("MOCATOTS accepts valid error code -4", {
  expect_no_error(MOCATOTS(c(0, -4)))
})

test_that("MOCATOTS accepts valid error code 88", {
  expect_no_error(MOCATOTS(c(0, 88)))
})

test_that("MOCATOTS errors when scores are out of range and not a code", {
  testthat::local_reproducible_output()

  expect_error(MOCATOTS(c(0, 31)), regexp = "scores")
})

test_that("MOCATOTS errors when scores is not numeric", {
  testthat::local_reproducible_output()

  expect_error(MOCATOTS(c("a", "b")), regexp = "scores")
})

test_that("MOCATOTS with no arguments returns an empty MOCATOTS object", {
  result <- MOCATOTS()

  expect_s3_class(result, "MOCATOTS")
  expect_equal(length(result), 0L)
})


# ---------------------------------------------------------------------------
# .setup_MOCATOTS_versions()
#
# .setup_*_versions() writes to the package-level .std_versions environment
# and errors on duplicate registration. The function is called once here
# outside any test_that() block; all tests below only read the registry.
# ---------------------------------------------------------------------------

rm(list = ls(envir = .std_versions), envir = .std_versions)
.setup_MOCATOTS_versions()

test_that(".setup_MOCATOTS_versions registers the expected methods", {
  methods <- list_std_methods(MOCATOTS())

  expect_true("norms" %in% methods)
  expect_true("regression" %in% methods)
})

# ---------------------------------------------------------------------------
# Norms versions
# ---------------------------------------------------------------------------

test_that(".setup_MOCATOTS_versions registers the 'nacc' norms version", {
  expect_true("nacc" %in% list_method_versions(MOCATOTS(), "norms"))
})

test_that(".setup_MOCATOTS_versions registers the 'updated' norms version", {
  expect_true("updated" %in% list_method_versions(MOCATOTS(), "norms"))
})

test_that("'nacc' norms version data contains a lookup_table with m and sd columns", {
  data <- get_version_data(MOCATOTS(), "norms", "nacc")

  expect_true("lookup_table" %in% names(data))
  expect_s3_class(data$lookup_table, "data.frame")
  expect_true("m" %in% names(data$lookup_table))
  expect_true("sd" %in% names(data$lookup_table))
})

test_that("'nacc' norms version data contains the expected covar_fns", {
  data <- get_version_data(MOCATOTS(), "norms", "nacc")

  expect_true("covar_fns" %in% names(data))
  expect_true("age" %in% names(data$covar_fns))
  expect_true("educ" %in% names(data$covar_fns))
  expect_true("sex" %in% names(data$covar_fns))
})

test_that("'updated' norms version data contains a lookup_table with m and sd columns", {
  data <- get_version_data(MOCATOTS(), "norms", "updated")

  expect_true("lookup_table" %in% names(data))
  expect_s3_class(data$lookup_table, "data.frame")
  expect_true("m" %in% names(data$lookup_table))
  expect_true("sd" %in% names(data$lookup_table))
})

test_that("'updated' norms version data contains the expected covar_fns", {
  data <- get_version_data(MOCATOTS(), "norms", "updated")

  expect_true("covar_fns" %in% names(data))
  expect_true("age" %in% names(data$covar_fns))
  expect_true("educ" %in% names(data$covar_fns))
  expect_true("sex" %in% names(data$covar_fns))
})

# ---------------------------------------------------------------------------
# Regression versions
# ---------------------------------------------------------------------------

test_that(".setup_MOCATOTS_versions registers the 'updated_2024.06' regression version", {
  expect_true(
    "updated_2024.06" %in% list_method_versions(MOCATOTS(), "regression")
  )
})

test_that(".setup_MOCATOTS_versions registers the 'updated_2025.06' regression version", {
  expect_true(
    "updated_2025.06" %in% list_method_versions(MOCATOTS(), "regression")
  )
})

test_that(".setup_MOCATOTS_versions registers the 'nacc' regression version", {
  expect_true("nacc" %in% list_method_versions(MOCATOTS(), "regression"))
})

test_that("regression version data contains coefs with rmse", {
  for (version in c("updated_2024.06", "updated_2025.06", "nacc")) {
    data <- get_version_data(MOCATOTS(), "regression", version)

    expect_true("coefs" %in% names(data), info = version)
    expect_true("rmse" %in% names(data$coefs), info = version)
  }
})

test_that("regression version data contains covar_fns for age, sex, and educ", {
  for (version in c("updated_2024.06", "updated_2025.06", "nacc")) {
    data <- get_version_data(MOCATOTS(), "regression", version)

    expect_true("covar_fns" %in% names(data), info = version)
    expect_true("age" %in% names(data$covar_fns), info = version)
    expect_true("sex" %in% names(data$covar_fns), info = version)
    expect_true("educ" %in% names(data$covar_fns), info = version)
  }
})

# ---------------------------------------------------------------------------
# Default method
# ---------------------------------------------------------------------------

test_that(".setup_MOCATOTS_versions sets the default method to 'regression' / 'updated_2025.06'", {
  default <- get_std_defaults(MOCATOTS())

  expect_equal(default$method, "regression")
  expect_equal(default$version, "updated_2025.06")
})
