# ---------------------------------------------------------------------------
# REYAREC()
# ---------------------------------------------------------------------------

# NOTE: REYAREC() does not pass `codes`, so npsych_scores() uses its
# default codes = numeric(). The current validate_npsych_scores() requires
# a *named* numeric vector for codes, so calling REYAREC() with any
# scores may currently error. The constructor tests below document the
# intended behaviour; they will fail until either the constructor is
# updated to pass a valid codes argument or validate_npsych_scores() is
# relaxed to allow an empty unnamed numeric vector.

test_that("REYAREC returns an object inheriting both REYAREC and npsych_scores", {
  result <- REYAREC()

  expect_s3_class(result, "REYAREC")
  expect_s3_class(result, "npsych_scores")
})

test_that("REYAREC sets class with REYAREC first, npsych_scores second", {
  result <- REYAREC()

  expect_equal(class(result), c("REYAREC", "npsych_scores"))
})

test_that("REYAREC returns an object with the correct values", {
  result <- REYAREC(c(0, 0, 100))

  expect_equal(as.numeric(result), c(0, 0, 100))
})

test_that("REYAREC sets label to 'RAVLT Recognition'", {
  result <- REYAREC()

  expect_equal(attr(result, "label"), "RAVLT Recognition")
})

test_that("REYAREC sets range to c(0, 100)", {
  result <- REYAREC()

  expect_equal(attr(result, "range"), c(0, 100))
})

test_that("REYAREC accepts scores at the range boundaries (0 and 100)", {
  expect_no_error(REYAREC(c(0, 100)))
})

test_that("REYAREC errors when scores are out of range and not a code", {
  testthat::local_reproducible_output()

  expect_error(REYAREC(c(0, 101)), regexp = "scores")
})

test_that("REYAREC errors when scores is not numeric", {
  testthat::local_reproducible_output()

  expect_error(REYAREC(c("a", "b")), regexp = "scores")
})

test_that("REYAREC with no arguments returns an empty REYAREC object", {
  result <- REYAREC()

  expect_s3_class(result, "REYAREC")
  expect_equal(length(result), 0L)
})


# ---------------------------------------------------------------------------
# .setup_REYAREC_versions()
#
# .setup_*_versions() writes to the package-level .std_versions environment
# and errors on duplicate registration. The function is called once here
# outside any test_that() block; all tests below only read the registry.
# ---------------------------------------------------------------------------

lapply(
  c(.std_versions[["norms"]], .std_versions[["regression"]], .std_defaults),
  \(x) {
    suppressWarnings(rm(
      list = "REYAREC",
      envir = x
    ))
  }
)

suppressMessages(.setup_REYAREC_versions())


test_that(".setup_REYAREC_versions registers the expected methods", {
  methods <- list_std_methods(REYAREC())

  expect_true("regression" %in% methods)
})

# ---------------------------------------------------------------------------
# Regression versions
# ---------------------------------------------------------------------------

test_that(".setup_REYAREC_versions registers the 'updated_2024.06' regression version", {
  expect_true(
    "updated_2024.06" %in% list_method_versions(REYAREC(), "regression")
  )
})

test_that(".setup_REYAREC_versions registers the 'updated_2025.06' regression version", {
  expect_true(
    "updated_2025.06" %in% list_method_versions(REYAREC(), "regression")
  )
})

test_that("regression version data contains coefs with rmse", {
  for (version in c("updated_2024.06", "updated_2025.06")) {
    data <- get_version_data(REYAREC(), "regression", version)

    expect_true("coefs" %in% names(data), info = version)
    expect_true("rmse" %in% names(data$coefs), info = version)
  }
})

test_that("regression version data contains covar_fns for age, sex, and educ", {
  for (version in c("updated_2024.06", "updated_2025.06")) {
    data <- get_version_data(REYAREC(), "regression", version)

    expect_true("covar_fns" %in% names(data), info = version)
    expect_true("age" %in% names(data$covar_fns), info = version)
    expect_true("sex" %in% names(data$covar_fns), info = version)
    expect_true("educ" %in% names(data$covar_fns), info = version)
  }
})

# No default method is set by .setup_REYAREC_versions().
