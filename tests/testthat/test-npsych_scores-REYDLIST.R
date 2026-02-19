# ---------------------------------------------------------------------------
# REYDLIST()
# ---------------------------------------------------------------------------

# NOTE: REYDLIST() does not pass `codes`, so npsych_scores() uses its
# default codes = numeric(). The current validate_npsych_scores() requires
# a *named* numeric vector for codes, so calling REYDLIST() with any
# scores may currently error. The constructor tests below document the
# intended behaviour; they will fail until the constructor or
# validate_npsych_scores() is updated.

test_that("REYDLIST returns an object inheriting both REYDLIST and npsych_scores", {
  result <- REYDLIST()

  expect_s3_class(result, "REYDLIST")
  expect_s3_class(result, "npsych_scores")
})

test_that("REYDLIST sets class with REYDLIST first, npsych_scores second", {
  result <- REYDLIST()

  expect_equal(class(result), c("REYDLIST", "npsych_scores"))
})

test_that("REYDLIST returns an object with the correct values", {
  result <- REYDLIST(c(0, 0, 15))

  expect_equal(as.numeric(result), c(0, 0, 15))
})

test_that("REYDLIST sets label to 'RAVLT Distractor List'", {
  result <- REYDLIST()

  expect_equal(attr(result, "label"), "RAVLT Distractor List")
})

test_that("REYDLIST sets range to c(0, 15)", {
  result <- REYDLIST()

  expect_equal(attr(result, "range"), c(0, 15))
})

test_that("REYDLIST accepts scores at the range boundaries (0 and 15)", {
  expect_no_error(REYDLIST(c(0, 15)))
})

test_that("REYDLIST errors when scores are out of range and not a code", {
  testthat::local_reproducible_output()

  expect_error(REYDLIST(c(0, 16)), regexp = "scores")
})

test_that("REYDLIST errors when scores is not numeric", {
  testthat::local_reproducible_output()

  expect_error(REYDLIST(c("a", "b")), regexp = "scores")
})

test_that("REYDLIST with no arguments returns an empty REYDLIST object", {
  result <- REYDLIST()

  expect_s3_class(result, "REYDLIST")
  expect_equal(length(result), 0L)
})


# ---------------------------------------------------------------------------
# .setup_REYDLIST_versions()
#
# .setup_*_versions() writes to the package-level .std_versions environment
# and errors on duplicate registration. The function is called once here
# outside any test_that() block; all tests below only read the registry.
# ---------------------------------------------------------------------------

lapply(
  c(.std_versions[["norms"]], .std_versions[["regression"]], .std_defaults),
  \(x) {
    suppressWarnings(rm(
      list = "REYDLIST",
      envir = x
    ))
  }
)

suppressMessages(.setup_REYDLIST_versions())

test_that(".setup_REYDLIST_versions registers the expected methods", {
  methods <- list_std_methods(REYDLIST())

  expect_true("norms" %in% methods)
})

# ---------------------------------------------------------------------------
# Norms versions
# ---------------------------------------------------------------------------

test_that(".setup_REYDLIST_versions registers the 'ravlt_trials' norms version", {
  expect_true("ravlt_trials" %in% list_method_versions(REYDLIST(), "norms"))
})

test_that("'ravlt_trials' norms version data contains a lookup_table with m and sd columns", {
  data <- get_version_data(REYDLIST(), "norms", "ravlt_trials")

  expect_true("lookup_table" %in% names(data))
  expect_s3_class(data$lookup_table, "data.frame")
  expect_true("m" %in% names(data$lookup_table))
  expect_true("sd" %in% names(data$lookup_table))
})

test_that("'ravlt_trials' norms version data contains the expected covar_fns", {
  data <- get_version_data(REYDLIST(), "norms", "ravlt_trials")

  expect_true("covar_fns" %in% names(data))
  expect_true("age" %in% names(data$covar_fns))
})

# No default method is set by .setup_REYDLIST_versions().
