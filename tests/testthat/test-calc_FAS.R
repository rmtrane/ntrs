# ---------------------------------------------------------------------------
# Happy path
# ---------------------------------------------------------------------------

test_that("calc_FAS sums valid scores correctly", {
  result <- calc_FAS(
    BILLS = 0, TAXES = 0, SHOPPING = 1, GAMES = 0, STOVE = 0,
    MEALPREP = 1, EVENTS = 0, PAYATTN = 0, REMDATES = 1, TRAVEL = 0
  )

  expect_equal(result, 3)
})

test_that("calc_FAS returns 0 when all scores are 0", {
  result <- calc_FAS(
    BILLS = 0, TAXES = 0, SHOPPING = 0, GAMES = 0, STOVE = 0,
    MEALPREP = 0, EVENTS = 0, PAYATTN = 0, REMDATES = 0, TRAVEL = 0
  )

  expect_equal(result, 0)
})

test_that("calc_FAS works with vectorized input", {
  result <- calc_FAS(
    BILLS = c(0, 1), TAXES = c(0, 2), SHOPPING = c(0, 1),
    GAMES = c(0, 0), STOVE = c(0, 0), MEALPREP = c(0, 1),
    EVENTS = c(0, 0), PAYATTN = c(0, 0), REMDATES = c(0, 0),
    TRAVEL = c(0, 0)
  )

  expect_equal(result, c(0, 5))
})

# ---------------------------------------------------------------------------
# Special code handling
# ---------------------------------------------------------------------------

test_that("calc_FAS treats code 8 and 9 as 0 in the sum", {

  # 8 = Not applicable, 9 = Unknown — counted as 0
  result <- calc_FAS(
    BILLS = 8, TAXES = 9, SHOPPING = 1, GAMES = 0, STOVE = 0,
    MEALPREP = 0, EVENTS = 0, PAYATTN = 0, REMDATES = 0, TRAVEL = 0
  )

  expect_equal(result, 1)
})

test_that("calc_FAS returns NA when any score is -4 (not collected)", {
  result <- calc_FAS(
    BILLS = -4, TAXES = 0, SHOPPING = 0, GAMES = 0, STOVE = 0,
    MEALPREP = 0, EVENTS = 0, PAYATTN = 0, REMDATES = 0, TRAVEL = 0
  )

  expect_true(is.na(result))
})

test_that("calc_FAS returns NA when any score is NA", {
  result <- calc_FAS(
    BILLS = NA, TAXES = 0, SHOPPING = 0, GAMES = 0, STOVE = 0,
    MEALPREP = 0, EVENTS = 0, PAYATTN = 0, REMDATES = 0, TRAVEL = 0
  )

  expect_true(is.na(result))
})

test_that("calc_FAS handles mix of valid and missing across rows", {
  result <- calc_FAS(
    BILLS = c(0, -4), TAXES = c(1, 0), SHOPPING = c(0, 0),
    GAMES = c(0, 0), STOVE = c(0, 0), MEALPREP = c(0, 0),
    EVENTS = c(0, 0), PAYATTN = c(0, 0), REMDATES = c(0, 0),
    TRAVEL = c(0, 0)
  )

  expect_equal(result[1], 1)
  expect_true(is.na(result[2]))
})

# ---------------------------------------------------------------------------
# Validation errors
# ---------------------------------------------------------------------------

test_that("calc_FAS errors on invalid BILLS values", {
  testthat::local_reproducible_output()

  expect_error(
    calc_FAS(
      BILLS = 5, TAXES = 0, SHOPPING = 0, GAMES = 0, STOVE = 0,
      MEALPREP = 0, EVENTS = 0, PAYATTN = 0, REMDATES = 0, TRAVEL = 0
    ),
    regexp = "BILLS"
  )
})

test_that("calc_FAS errors on invalid TAXES values", {
  testthat::local_reproducible_output()

  expect_error(
    calc_FAS(
      BILLS = 0, TAXES = 5, SHOPPING = 0, GAMES = 0, STOVE = 0,
      MEALPREP = 0, EVENTS = 0, PAYATTN = 0, REMDATES = 0, TRAVEL = 0
    ),
    regexp = "TAXES"
  )
})
