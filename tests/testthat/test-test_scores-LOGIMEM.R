# ---------------------------------------------------------------------------
# LOGIMEM()
# ---------------------------------------------------------------------------

test_that("LOGIMEM returns an object inheriting both LOGIMEM and test_scores", {
  result <- LOGIMEM()

  expect_s3_class(result, "LOGIMEM")
  expect_s3_class(result, "test_scores")
})

test_that("LOGIMEM sets class with LOGIMEM first, test_scores second", {
  result <- LOGIMEM()

  expect_equal(class(result), c("LOGIMEM", "test_scores"))
})

test_that("LOGIMEM returns an object with the correct values", {
  result <- LOGIMEM(c(0, 0, 25))

  expect_equal(as.numeric(result), c(0, 0, 25))
})

test_that("LOGIMEM sets label to 'Logical Memory, Immediate'", {
  result <- LOGIMEM()

  expect_equal(attr(result, "label"), "Logical Memory, Immediate")
})

test_that("LOGIMEM sets range to c(0, 25)", {
  result <- LOGIMEM()

  expect_equal(attr(result, "range"), c(0, 25))
})

test_that("LOGIMEM sets codes with the correct values", {
  result <- LOGIMEM()
  codes <- attr(result, "codes")

  expect_true(95 %in% codes)
  expect_true(96 %in% codes)
  expect_true(97 %in% codes)
  expect_true(98 %in% codes)
  expect_true(-4 %in% codes)
})

test_that("LOGIMEM codes are named", {
  result <- LOGIMEM()

  expect_false(is.null(names(attr(result, "codes"))))
})

test_that("LOGIMEM accepts scores at the range boundaries (0 and 25)", {
  expect_no_error(LOGIMEM(c(0, 25)))
})

test_that("LOGIMEM accepts valid error code 95", {
  expect_no_error(LOGIMEM(c(0, 95)))
})

test_that("LOGIMEM accepts valid error code 96", {
  expect_no_error(LOGIMEM(c(0, 96)))
})

test_that("LOGIMEM accepts valid error code 97", {
  expect_no_error(LOGIMEM(c(0, 97)))
})

test_that("LOGIMEM accepts valid error code 98", {
  expect_no_error(LOGIMEM(c(0, 98)))
})

test_that("LOGIMEM accepts valid error code -4", {
  expect_no_error(LOGIMEM(c(0, -4)))
})

test_that("LOGIMEM errors when scores are out of range and not a code", {
  testthat::local_reproducible_output()

  expect_error(LOGIMEM(c(0, 26)), regexp = "scores")
})

test_that("LOGIMEM errors when scores is not numeric", {
  testthat::local_reproducible_output()

  expect_error(LOGIMEM(c("a", "b")), regexp = "scores")
})

test_that("LOGIMEM with no arguments returns an empty LOGIMEM object", {
  result <- LOGIMEM()

  expect_s3_class(result, "LOGIMEM")
  expect_equal(length(result), 0L)
})


# ---------------------------------------------------------------------------
# .setup_LOGIMEM_versions()
#
# .setup_*_versions() writes to the package-level .std_versions environment
# and errors on duplicate registration. The function is called once here
# outside any test_that() block; all tests below only read the registry.
# ---------------------------------------------------------------------------

rm(list = ls(envir = .std_versions), envir = .std_versions)
.setup_LOGIMEM_versions()

test_that(".setup_LOGIMEM_versions registers the expected methods", {
  methods <- get_std_methods(LOGIMEM())

  expect_true("regression" %in% methods)
})

# ---------------------------------------------------------------------------
# Regression versions
# ---------------------------------------------------------------------------

test_that(".setup_LOGIMEM_versions registers the 'updated_2024.06' regression version", {
  expect_true("updated_2024.06" %in% get_versions(LOGIMEM(), "regression"))
})

test_that(".setup_LOGIMEM_versions registers the 'updated_2025.06' regression version", {
  expect_true("updated_2025.06" %in% get_versions(LOGIMEM(), "regression"))
})

test_that(".setup_LOGIMEM_versions registers the 'nacc_legacy' regression version", {
  expect_true("nacc_legacy" %in% get_versions(LOGIMEM(), "regression"))
})

test_that("regression version data contains coefs with rmse", {
  for (version in c("updated_2024.06", "updated_2025.06", "nacc_legacy")) {
    data <- get_version_data(LOGIMEM(), "regression", version)

    expect_true("coefs" %in% names(data), info = version)
    expect_true("rmse" %in% names(data$coefs), info = version)
  }
})

test_that("regression version data contains covariate_prep_funs for age, sex, and educ", {
  for (version in c("updated_2024.06", "updated_2025.06", "nacc_legacy")) {
    data <- get_version_data(LOGIMEM(), "regression", version)

    expect_true("covariate_prep_funs" %in% names(data), info = version)
    expect_true("age" %in% names(data$covariate_prep_funs), info = version)
    expect_true("sex" %in% names(data$covariate_prep_funs), info = version)
    expect_true("educ" %in% names(data$covariate_prep_funs), info = version)
  }
})

# ---------------------------------------------------------------------------
# Default method
# ---------------------------------------------------------------------------

test_that(".setup_LOGIMEM_versions sets the default method to 'regression' / 'nacc_legacy'", {
  default <- get_default_method(LOGIMEM())

  expect_equal(default$method, "regression")
  expect_equal(default$version, "nacc_legacy")
})
