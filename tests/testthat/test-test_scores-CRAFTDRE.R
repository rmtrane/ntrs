# ---------------------------------------------------------------------------
# CRAFTDRE()
# ---------------------------------------------------------------------------

test_that("CRAFTDRE returns an object inheriting both CRAFTDRE and test_scores", {
  result <- CRAFTDRE()

  expect_s3_class(result, "CRAFTDRE")
  expect_s3_class(result, "test_scores")
})

test_that("CRAFTDRE sets class with CRAFTDRE first, test_scores second", {
  result <- CRAFTDRE()

  expect_equal(class(result), c("CRAFTDRE", "test_scores"))
})

test_that("CRAFTDRE returns an object with the correct values", {
  result <- CRAFTDRE(c(0, 0, 25))

  expect_equal(as.numeric(result), c(0, 0, 25))
})

test_that("CRAFTDRE sets label to 'Craft Delay - Paraphrase'", {
  result <- CRAFTDRE()

  expect_equal(attr(result, "label"), "Craft Delay - Paraphrase")
})

test_that("CRAFTDRE sets range to c(0, 25)", {
  result <- CRAFTDRE()

  expect_equal(attr(result, "range"), c(0, 25))
})

test_that("CRAFTDRE sets codes with the correct values", {
  result <- CRAFTDRE()
  codes <- attr(result, "codes")

  expect_true(95 %in% codes)
  expect_true(96 %in% codes)
  expect_true(97 %in% codes)
  expect_true(98 %in% codes)
  expect_true(-4 %in% codes)
})

test_that("CRAFTDRE codes are named", {
  result <- CRAFTDRE()

  expect_false(is.null(names(attr(result, "codes"))))
})

test_that("CRAFTDRE accepts scores at the range boundaries (0 and 25)", {
  expect_no_error(CRAFTDRE(c(0, 25)))
})

test_that("CRAFTDRE accepts valid error code 95", {
  expect_no_error(CRAFTDRE(c(0, 95)))
})

test_that("CRAFTDRE accepts valid error code 96", {
  expect_no_error(CRAFTDRE(c(0, 96)))
})

test_that("CRAFTDRE accepts valid error code 97", {
  expect_no_error(CRAFTDRE(c(0, 97)))
})

test_that("CRAFTDRE accepts valid error code 98", {
  expect_no_error(CRAFTDRE(c(0, 98)))
})

test_that("CRAFTDRE accepts valid error code -4", {
  expect_no_error(CRAFTDRE(c(0, -4)))
})

test_that("CRAFTDRE errors when scores are out of range and not a code", {
  testthat::local_reproducible_output()

  expect_error(CRAFTDRE(c(0, 26)), regexp = "scores")
})

test_that("CRAFTDRE errors when scores is not numeric", {
  testthat::local_reproducible_output()

  expect_error(CRAFTDRE(c("a", "b")), regexp = "scores")
})

test_that("CRAFTDRE with no arguments returns an empty CRAFTDRE object", {
  result <- CRAFTDRE()

  expect_s3_class(result, "CRAFTDRE")
  expect_equal(length(result), 0L)
})


# ---------------------------------------------------------------------------
# .setup_CRAFTDRE_versions()
#
# .setup_*_versions() writes to the package-level .std_versions environment
# and errors on duplicate registration. The function is called once here
# outside any test_that() block; all tests below only read the registry.
# ---------------------------------------------------------------------------

rm(list = ls(envir = .std_versions), envir = .std_versions)
.setup_CRAFTDRE_versions()

test_that(".setup_CRAFTDRE_versions registers the expected methods", {
  methods <- get_std_methods(CRAFTDRE())

  expect_true("norms" %in% methods)
  expect_true("regression" %in% methods)
})

# ---------------------------------------------------------------------------
# Norms versions
# ---------------------------------------------------------------------------

test_that(".setup_CRAFTDRE_versions registers the 'nacc' norms version", {
  expect_true("nacc" %in% get_versions(CRAFTDRE(), "norms"))
})

test_that(".setup_CRAFTDRE_versions registers the 'updated' norms version", {
  expect_true("updated" %in% get_versions(CRAFTDRE(), "norms"))
})

test_that("'nacc' norms version data contains a lookup_table with m and sd columns", {
  data <- get_version_data(CRAFTDRE(), "norms", "nacc")

  expect_true("lookup_table" %in% names(data))
  expect_s3_class(data$lookup_table, "data.frame")
  expect_true("m" %in% names(data$lookup_table))
  expect_true("sd" %in% names(data$lookup_table))
})

test_that("'nacc' norms version data contains the expected covariate_prep_funs", {
  data <- get_version_data(CRAFTDRE(), "norms", "nacc")

  expect_true("covariate_prep_funs" %in% names(data))
  expect_true("age" %in% names(data$covariate_prep_funs))
  expect_true("educ" %in% names(data$covariate_prep_funs))
  expect_true("sex" %in% names(data$covariate_prep_funs))
})

test_that("'updated' norms version data contains a lookup_table with m and sd columns", {
  data <- get_version_data(CRAFTDRE(), "norms", "updated")

  expect_true("lookup_table" %in% names(data))
  expect_s3_class(data$lookup_table, "data.frame")
  expect_true("m" %in% names(data$lookup_table))
  expect_true("sd" %in% names(data$lookup_table))
})

test_that("'updated' norms version data contains the expected covariate_prep_funs", {
  data <- get_version_data(CRAFTDRE(), "norms", "updated")

  expect_true("covariate_prep_funs" %in% names(data))
  expect_true("age" %in% names(data$covariate_prep_funs))
  expect_true("educ" %in% names(data$covariate_prep_funs))
  expect_true("sex" %in% names(data$covariate_prep_funs))
})

# ---------------------------------------------------------------------------
# Regression versions
# ---------------------------------------------------------------------------

test_that(".setup_CRAFTDRE_versions registers the 'updated_2024.06' regression version", {
  expect_true("updated_2024.06" %in% get_versions(CRAFTDRE(), "regression"))
})

test_that(".setup_CRAFTDRE_versions registers the 'updated_2025.06' regression version", {
  expect_true("updated_2025.06" %in% get_versions(CRAFTDRE(), "regression"))
})

test_that(".setup_CRAFTDRE_versions registers the 'nacc' regression version", {
  expect_true("nacc" %in% get_versions(CRAFTDRE(), "regression"))
})

test_that("regression version data contains coefs with rmse", {
  for (version in c("updated_2024.06", "updated_2025.06", "nacc")) {
    data <- get_version_data(CRAFTDRE(), "regression", version)

    expect_true("coefs" %in% names(data), info = version)
    expect_true("rmse" %in% names(data$coefs), info = version)
  }
})

test_that("regression version data contains covariate_prep_funs for age, sex, and educ", {
  for (version in c("updated_2024.06", "updated_2025.06", "nacc")) {
    data <- get_version_data(CRAFTDRE(), "regression", version)

    expect_true("covariate_prep_funs" %in% names(data), info = version)
    expect_true("age" %in% names(data$covariate_prep_funs), info = version)
    expect_true("sex" %in% names(data$covariate_prep_funs), info = version)
    expect_true("educ" %in% names(data$covariate_prep_funs), info = version)
  }
})

# ---------------------------------------------------------------------------
# Default method
# ---------------------------------------------------------------------------

test_that(".setup_CRAFTDRE_versions sets the default method to 'regression' / 'updated_2025.06'", {
  default <- get_default_method(CRAFTDRE())

  expect_equal(default$method, "regression")
  expect_equal(default$version, "updated_2025.06")
})
