# ---------------------------------------------------------------------------
# Happy path
# ---------------------------------------------------------------------------

test_that("calc_MOCACLOCK sums valid 0/1 inputs", {
  result <- calc_MOCACLOCK(
    MOCACLOC = c(1, 0, 1),
    MOCACLON = c(1, 1, 0),
    MOCACLOH = c(0, 1, 1)
  )

  expect_true(S7::S7_inherits(result, MOCACLOCK))
  expect_equal(as.numeric(result), c(2, 2, 2))
})

test_that("calc_MOCACLOCK returns 3 for all perfect scores", {
  result <- calc_MOCACLOCK(
    MOCACLOC = 1,
    MOCACLON = 1,
    MOCACLOH = 1
  )

  expect_equal(as.numeric(result), 3)
})

test_that("calc_MOCACLOCK returns 0 for all zeros", {
  result <- calc_MOCACLOCK(
    MOCACLOC = 0,
    MOCACLON = 0,
    MOCACLOH = 0
  )

  expect_equal(as.numeric(result), 0)
})

# ---------------------------------------------------------------------------
# Error code handling
# ---------------------------------------------------------------------------

test_that("calc_MOCACLOCK replaces error codes with NA", {
  result <- calc_MOCACLOCK(
    MOCACLOC = c(1, 95),
    MOCACLON = c(1, 1),
    MOCACLOH = c(1, 0)
  )

  expect_equal(as.numeric(result), c(3, NA))
})

test_that("calc_MOCACLOCK handles NA inputs", {
  result <- calc_MOCACLOCK(
    MOCACLOC = c(1, NA),
    MOCACLON = c(1, 1),
    MOCACLOH = c(0, 0)
  )

  expect_equal(as.numeric(result), c(2, NA))
})

# ---------------------------------------------------------------------------
# Validation errors
# ---------------------------------------------------------------------------

test_that("calc_MOCACLOCK errors on invalid MOCACLOC values", {
  testthat::local_reproducible_output()

  expect_error(
    calc_MOCACLOCK(MOCACLOC = 5, MOCACLON = 1, MOCACLOH = 1),
    regexp = "MOCACLOC"
  )
})

test_that("calc_MOCACLOCK errors on invalid MOCACLON values", {
  testthat::local_reproducible_output()

  expect_error(
    calc_MOCACLOCK(MOCACLOC = 1, MOCACLON = 5, MOCACLOH = 1),
    regexp = "MOCACLON"
  )
})

test_that("calc_MOCACLOCK errors on invalid MOCACLOH values", {
  testthat::local_reproducible_output()

  expect_error(
    calc_MOCACLOCK(MOCACLOC = 1, MOCACLON = 1, MOCACLOH = 5),
    regexp = "MOCACLOH"
  )
})
