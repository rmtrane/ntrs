# Tests for get_version_data()

# ---------------------------------------------------------------------------
# Function exists
# ---------------------------------------------------------------------------

test_that("get_version_data() is a function", {
  expect_true(is.function(get_version_data))
})

# ---------------------------------------------------------------------------
# method = NULL, version = NULL — uses defaults
# ---------------------------------------------------------------------------

test_that("get_version_data() returns data when method and version are both NULL and a default is set", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
  )

  result <- get_version_data(MOCATOTS())

  expect_true(S7::S7_inherits(result, std_version))
})

test_that("get_version_data() errors when method is NULL but version is not NULL", {
  testthat::local_reproducible_output()

  expect_error(
    get_version_data(MOCATOTS(), method = NULL, version = "nacc"),
    "must be.*NULL.*when.*method.*NULL"
  )
})

test_that("get_version_data() error message includes the supplied version when method is NULL", {
  testthat::local_reproducible_output()

  expect_error(
    get_version_data(MOCATOTS(), method = NULL, version = "nacc"),
    "nacc"
  )
})

test_that("get_version_data() errors when no default is set and method is NULL", {
  local_restore_default("MOCATOTS")

  expect_error(
    suppressMessages(get_version_data(MOCATOTS()))
  )
})

# ---------------------------------------------------------------------------
# method supplied, invalid method
# ---------------------------------------------------------------------------

test_that("get_version_data() errors for a method not implemented on the scores class", {
  testthat::local_reproducible_output()

  expect_error(
    get_version_data(MOCATOTS(), method = "nonexistent", version = "nacc"),
    "nonexistent"
  )
})

test_that("get_version_data() error for invalid method mentions the scores class", {
  testthat::local_reproducible_output()

  expect_error(
    get_version_data(MOCATOTS(), method = "nonexistent", version = "nacc"),
    "MOCATOTS"
  )
})

# ---------------------------------------------------------------------------
# method supplied, version = NULL
# ---------------------------------------------------------------------------

test_that("get_version_data() errors when method is supplied but version is NULL", {
  testthat::local_reproducible_output()

  expect_error(
    get_version_data(MOCATOTS(), method = "norms", version = NULL)
  )
})

# ---------------------------------------------------------------------------
# method supplied, invalid version
# ---------------------------------------------------------------------------

test_that("get_version_data() errors when the requested version does not exist", {
  testthat::local_reproducible_output()

  expect_error(
    get_version_data(MOCATOTS(), method = "norms", version = "no_such_version"),
    "no_such_version"
  )
})

test_that("get_version_data() error for missing version mentions available versions", {
  testthat::local_reproducible_output()

  expect_error(
    get_version_data(MOCATOTS(), method = "norms", version = "no_such_version"),
    "Available versions"
  )
})

# ---------------------------------------------------------------------------
# Successful retrieval — norms method
# ---------------------------------------------------------------------------

test_that("get_version_data() returns a norms_version S7 object for a valid norms version", {
  result <- get_version_data(MOCATOTS(), method = "norms", version = "nacc")

  expect_true(S7::S7_inherits(result, norms_version))
  expect_equal(result@scores_class, "MOCATOTS")
  expect_equal(result@method_name, "norms")
  expect_equal(result@version_id, "nacc")
})

test_that("get_version_data() returns the same data regardless of how the version is resolved", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
  )

  via_default <- get_version_data(MOCATOTS())
  via_explicit <- get_version_data(MOCATOTS(), method = "norms", version = "nacc")

  expect_identical(via_default, via_explicit)
})

# ---------------------------------------------------------------------------
# Successful retrieval — regression method
# ---------------------------------------------------------------------------

test_that("get_version_data() returns a regression_version S7 object for a valid regression version", {
  result <- get_version_data(
    MOCATOTS(),
    method = "regression",
    version = "updated_2025.06"
  )

  expect_true(S7::S7_inherits(result, regression_version))
  expect_equal(result@scores_class, "MOCATOTS")
  expect_equal(result@method_name, "regression")
})

test_that("get_version_data() regression result has coefs and covar_fns properties", {
  result <- get_version_data(
    MOCATOTS(),
    method = "regression",
    version = "updated_2025.06"
  )

  expect_true(length(result@coefs) > 0)
  expect_true(is.list(result@covar_fns))
})
