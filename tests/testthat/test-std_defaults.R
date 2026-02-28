# Tests for set_std_defaults() and get_std_defaults()
#
# State isolation:
# Both functions read/write to the package-internal `.std_defaults` environment.
# Each test that writes state must clean up via withr::defer().
# local_restore_default() is defined in helper-restore-defaults.R.

# ---------------------------------------------------------------------------
# get_std_defaults() — error when no default set
# ---------------------------------------------------------------------------

test_that("get_std_defaults() returns NULL when no default has been set for the class", {
  bare_cls <- new_npsych_scores(
    "bare_class_defaults_test",
    label = "Bare",
    range = c(0, 10)
  )

  result <- suppressMessages(get_std_defaults(bare_cls()))

  expect_null(result)
})

test_that("get_std_defaults() message names the class when no default set", {
  bare_cls <- new_npsych_scores(
    "named_bare_defaults_test",
    label = "Named Bare",
    range = c(0, 10)
  )

  expect_message(
    get_std_defaults(bare_cls()),
    "named_bare_defaults_test"
  )
})

# ---------------------------------------------------------------------------
# set_std_defaults() — basic success
# ---------------------------------------------------------------------------

test_that("set_std_defaults() returns invisibly", {
  local_restore_default("MOCATOTS")

  expect_invisible(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
  )
})

test_that("set_std_defaults() sets the default retrievable by get_std_defaults()", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
  )

  result <- get_std_defaults(MOCATOTS())

  expect_equal(result$method, "norms")
  expect_equal(result$version, "nacc")
})

test_that("get_std_defaults() returns a list with 'method' and 'version' names", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
  )

  result <- get_std_defaults(MOCATOTS())

  expect_type(result, "list")
  expect_named(result, c("method", "version"))
})

test_that("set_std_defaults() works with regression method", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(
      MOCATOTS(),
      method = "regression",
      version = "updated_2025.06"
    )
  )

  result <- get_std_defaults(MOCATOTS())

  expect_equal(result$method, "regression")
  expect_equal(result$version, "updated_2025.06")
})

# ---------------------------------------------------------------------------
# set_std_defaults() — invalid method / version
# ---------------------------------------------------------------------------

test_that("set_std_defaults() errors for a non-existent method", {
  testthat::local_reproducible_output()

  expect_error(
    set_std_defaults(MOCATOTS(), method = "nonexistent", version = "nacc"),
    regexp = "nonexistent"
  )
})

test_that("set_std_defaults() errors for a non-existent version", {
  testthat::local_reproducible_output()

  expect_error(
    set_std_defaults(MOCATOTS(), method = "norms", version = "no_such_version"),
    regexp = "no_such_version"
  )
})

test_that("set_std_defaults() errors for non-existent version on valid method", {
  testthat::local_reproducible_output()

  expect_error(
    set_std_defaults(
      MOCATOTS(),
      method = "regression",
      version = "does_not_exist"
    )
  )
})

# ---------------------------------------------------------------------------
# set_std_defaults() — overwrite behaviour
# ---------------------------------------------------------------------------

test_that("set_std_defaults() errors when default already set and overwrite = FALSE", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
  )

  testthat::local_reproducible_output()

  expect_error(
    set_std_defaults(
      MOCATOTS(),
      method = "norms",
      version = "updated",
      overwrite = FALSE
    ),
    "overwrite"
  )
})

test_that("set_std_defaults() error when already set references overwrite = TRUE as fix", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
  )

  testthat::local_reproducible_output()

  expect_error(
    set_std_defaults(
      MOCATOTS(),
      method = "norms",
      version = "updated",
      overwrite = FALSE
    ),
    "overwrite = TRUE"
  )
})

test_that("set_std_defaults() overwrites when overwrite = TRUE", {
  local_restore_default("MOCATOTS")

  suppressMessages({
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
    set_std_defaults(
      MOCATOTS(),
      method = "norms",
      version = "updated",
      overwrite = TRUE
    )
  })

  result <- get_std_defaults(MOCATOTS())

  expect_equal(result$method, "norms")
  expect_equal(result$version, "updated")
})

test_that("set_std_defaults() does not error when same method + version re-set (already current)", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
  )

  expect_no_error(
    suppressMessages(
      set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
    )
  )
})

test_that("set_std_defaults() informs when same method + version is already current", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
  )

  expect_message(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc"),
    "already set"
  )
})

# ---------------------------------------------------------------------------
# Defaults are class-specific (no cross-class contamination)
# ---------------------------------------------------------------------------

test_that("defaults are stored per subclass, not shared across classes", {
  local_restore_default("MOCATOTS")
  local_restore_default("ANIMALS")

  suppressMessages(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
  )

  defaults_env <- get(
    ".std_defaults",
    envir = asNamespace("ntrs")
  )

  mocatots_default <- get_std_defaults(MOCATOTS())
  expect_equal(mocatots_default$method, "norms")
  expect_equal(mocatots_default$version, "nacc")

  # ANIMALS has its own independent slot — absent or different
  if (exists("ANIMALS", envir = defaults_env, inherits = FALSE)) {
    animals_default <- get_std_defaults(ANIMALS())
    expect_false(
      identical(
        list(method = "norms", version = "nacc"),
        animals_default
      )
    )
  } else {
    expect_null(suppressMessages(get_std_defaults(ANIMALS())))
  }
})
