# ---------------------------------------------------------------------------
# Happy path
# ---------------------------------------------------------------------------

test_that("calc_REYAREC computes accuracy correctly", {
  # Formula: (reytcor + 15 - reyfpos) * 100 / 30
  # (10 + 15 - 3) * 100 / 30 = 73.33...
  result <- calc_REYAREC(
    reytcor = REYTCOR(10),
    reyfpos = REYFPOS(3)
  )

  expect_true(S7::S7_inherits(result, REYAREC))
  expect_equal(as.numeric(result), (10 + 15 - 3) * 100 / 30)
})

test_that("calc_REYAREC returns 100 for perfect score (15 correct, 0 false pos)", {
  result <- calc_REYAREC(
    reytcor = REYTCOR(15),
    reyfpos = REYFPOS(0)
  )

  expect_equal(as.numeric(result), 100)
})

test_that("calc_REYAREC works with vectorized input", {
  result <- calc_REYAREC(
    reytcor = REYTCOR(c(15, 10, 5)),
    reyfpos = REYFPOS(c(0, 5, 10))
  )

  expected <- (c(15, 10, 5) + 15 - c(0, 5, 10)) * 100 / 30
  expect_equal(as.numeric(result), expected)
})

# ---------------------------------------------------------------------------
# Error code handling
# ---------------------------------------------------------------------------

test_that("calc_REYAREC propagates NA from error codes", {
  result <- calc_REYAREC(
    reytcor = REYTCOR(c(10, 88)),
    reyfpos = REYFPOS(c(3, 0))
  )

  expect_equal(as.numeric(result)[1], (10 + 15 - 3) * 100 / 30)
  expect_true(is.na(as.numeric(result)[2]))
})

# ---------------------------------------------------------------------------
# Validation errors
# ---------------------------------------------------------------------------

test_that("calc_REYAREC errors when reytcor is not REYTCOR class", {
  testthat::local_reproducible_output()

  expect_error(
    calc_REYAREC(reytcor = 10, reyfpos = REYFPOS(3)),
    regexp = "reytcor"
  )
})

test_that("calc_REYAREC errors when reyfpos is not REYFPOS class", {
  testthat::local_reproducible_output()

  expect_error(
    calc_REYAREC(reytcor = REYTCOR(10), reyfpos = 3),
    regexp = "reyfpos"
  )
})
