# Tests for std() — the user-facing standardization wrapper
#
# std() dispatches to std_using_norms() or std_using_regression() based on
# registered defaults or explicit method/version arguments.
#
# State isolation:
# Tests that modify .std_defaults use local_restore_default() from
# helper-restore-defaults.R.

# ---------------------------------------------------------------------------
# S3 generic structure
# ---------------------------------------------------------------------------

test_that("std() is an S7 generic", {
  expect_no_error(S7::check_is_S7(std))
})

test_that("method(std, npsych_scores) exists", {
  expect_no_error(S7::method(std, npsych_scores))
})

# ---------------------------------------------------------------------------
# Default resolution — uses get_std_defaults() when method/version are NULL
# ---------------------------------------------------------------------------

test_that("std() uses default norms method when method/version are NULL", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
  )

  scores <- MOCATOTS(c(25, 28))

  result <- std(scores, age = 72, sex = 1, educ = 16)
  expected <- std_using_norms(
    scores,
    age = 72,
    sex = 1,
    educ = 16,
    version = "nacc"
  )

  expect_equal(result, expected)
})

test_that("std() uses default regression method when method/version are NULL", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(
      MOCATOTS(),
      method = "regression",
      version = "updated_2025.06"
    )
  )

  scores <- MOCATOTS(c(25, 28))

  result <- std(scores, age = 72, sex = 1, educ = 16, race = 1)
  expected <- std_using_regression(
    scores,
    age = 72,
    sex = 1,
    educ = 16,
    race = 1,
    version = "updated_2025.06"
  )

  expect_equal(result, expected)
})

# ---------------------------------------------------------------------------
# Explicit method + version override defaults
# ---------------------------------------------------------------------------

test_that("std() with explicit method + version overrides defaults", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
  )

  scores <- MOCATOTS(c(25, 28))

  result <- std(
    scores,
    method = "regression",
    version = "updated_2025.06",
    age = 72,
    sex = 1,
    educ = 16,
    race = 1
  )
  expected <- std_using_regression(
    scores,
    age = 72,
    sex = 1,
    educ = 16,
    race = 1,
    version = "updated_2025.06"
  )

  expect_equal(result, expected)
})

# ---------------------------------------------------------------------------
# Explicit method with NULL version — skips default version resolution
# ---------------------------------------------------------------------------

test_that("std() with explicit method but NULL version passes version through as NULL", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(
      MOCATOTS(),
      method = "regression",
      version = "updated_2025.06"
    )
  )

  scores <- MOCATOTS(c(25, 28))

  # When method is explicitly provided, the is.null(method) branch is skipped,

  # so version stays NULL and is NOT passed to the underlying fn.
  # std_using_norms requires version, so this should error.

  expect_error(
    std(scores, method = "norms", age = 72, sex = 1, educ = 16)
  )
})

# ---------------------------------------------------------------------------
# Error: no defaults set and method/version omitted
# ---------------------------------------------------------------------------

test_that("std() errors when no defaults are set and method is NULL", {
  local_restore_default("REYTOTAL")

  testthat::local_reproducible_output()

  expect_error(
    std(REYTOTAL(c(10, 20))),
    "No default method registered"
  )
})

# ---------------------------------------------------------------------------
# Error: invalid method name
# ---------------------------------------------------------------------------

test_that("std() errors for a non-existent method", {
  testthat::local_reproducible_output()

  expect_error(
    std(MOCATOTS(25), method = "nonexistent", version = "nacc"),
    "not available"
  )
})

test_that("std() error for invalid method mentions available methods", {
  testthat::local_reproducible_output()

  expect_error(
    std(MOCATOTS(25), method = "nonexistent", version = "nacc"),
    "Available methods"
  )
})

# ---------------------------------------------------------------------------
# Return type and vectorization
# ---------------------------------------------------------------------------

test_that("std() returns a numeric vector", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(
      MOCATOTS(),
      method = "regression",
      version = "updated_2025.06"
    )
  )

  result <- std(MOCATOTS(c(25, 28)), age = 72, sex = 1, educ = 16, race = 1)

  expect_type(result, "double")
})

test_that("std() output length matches input length", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(
      MOCATOTS(),
      method = "regression",
      version = "updated_2025.06"
    )
  )

  scores <- MOCATOTS(c(20, 22, 25, 28))
  result <- std(scores, age = 72, sex = 1, educ = 16, race = 1)

  expect_length(result, 4)
})
