# Tests for get_version_data()
#
# State isolation:
# Tests that set a default write to .std_defaults. Use local_restore_default()
# (defined in helper-restore-defaults.R) to clean up after each such test.

# ---------------------------------------------------------------------------
# S3 generic
# ---------------------------------------------------------------------------

test_that("get_version_data() is an S3 generic", {
  expect_true(sloop::is_s3_generic("get_version_data"))
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

  expect_type(result, "list")
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

  testthat::local_reproducible_output()

  expect_error(
    get_version_data(MOCATOTS()),
    "No default method set"
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

test_that("get_version_data() returns a list for a valid norms version", {
  result <- get_version_data(MOCATOTS(), method = "norms", version = "nacc")

  expect_type(result, "list")
})

test_that("get_version_data() returns the same data regardless of how the version is resolved (explicit vs default)", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
  )

  via_default <- get_version_data(MOCATOTS())
  via_explicit <- get_version_data(
    MOCATOTS(),
    method = "norms",
    version = "nacc"
  )

  expect_identical(via_default, via_explicit)
})

# ---------------------------------------------------------------------------
# Successful retrieval — regression method
# ---------------------------------------------------------------------------

test_that("get_version_data() returns a list for a valid regression version", {
  result <- get_version_data(
    MOCATOTS(),
    method = "regression",
    version = "updated_2025.06"
  )

  expect_type(result, "list")
})

test_that("get_version_data() regression result contains expected coefs element", {
  result <- get_version_data(
    MOCATOTS(),
    method = "regression",
    version = "updated_2025.06"
  )

  expect_true("coefs" %in% names(result))
})

# ---------------------------------------------------------------------------
# Return value structure
# ---------------------------------------------------------------------------

test_that("get_version_data() does not return the full registry entry (only $data)", {
  result <- get_version_data(MOCATOTS(), method = "norms", version = "nacc")

  # Top-level registry fields (subclass, method, version) should NOT be present
  expect_false("subclass" %in% names(result))
  expect_false("method" %in% names(result))
  expect_false("version" %in% names(result))
})
