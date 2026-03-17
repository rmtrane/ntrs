# ---------------------------------------------------------------------------
# Happy path
# ---------------------------------------------------------------------------

test_that("calc_REYTOTAL sums five trials correctly", {
  result <- calc_REYTOTAL(
    rey1rec = REY1REC(5),
    rey2rec = REY2REC(7),
    rey3rec = REY3REC(9),
    rey4rec = REY4REC(11),
    rey5rec = REY5REC(13)
  )

  expect_true(S7::S7_inherits(result, REYTOTAL))
  expect_equal(as.numeric(result), 5 + 7 + 9 + 11 + 13)
})

test_that("calc_REYTOTAL works with vectorized input", {
  result <- calc_REYTOTAL(
    rey1rec = REY1REC(c(3, 5)),
    rey2rec = REY2REC(c(5, 7)),
    rey3rec = REY3REC(c(7, 9)),
    rey4rec = REY4REC(c(9, 11)),
    rey5rec = REY5REC(c(11, 13))
  )

  expect_equal(as.numeric(result), c(35, 45))
})

test_that("calc_REYTOTAL accepts plain numeric inputs (auto-coercion)", {
  result <- calc_REYTOTAL(
    rey1rec = 5,
    rey2rec = 7,
    rey3rec = 9,
    rey4rec = 11,
    rey5rec = 13
  )

  expect_true(S7::S7_inherits(result, REYTOTAL))
  expect_equal(as.numeric(result), 45)
})

# ---------------------------------------------------------------------------
# Error code handling
# ---------------------------------------------------------------------------

test_that("calc_REYTOTAL propagates NA from error codes", {
  result <- calc_REYTOTAL(
    rey1rec = REY1REC(c(5, 88)),
    rey2rec = REY2REC(c(7, 7)),
    rey3rec = REY3REC(c(9, 9)),
    rey4rec = REY4REC(c(11, 11)),
    rey5rec = REY5REC(c(13, 13))
  )

  expect_equal(as.numeric(result)[1], 45)
  expect_true(is.na(as.numeric(result)[2]))
})

test_that("calc_REYTOTAL returns all zeros when all trials are zero", {
  result <- calc_REYTOTAL(
    rey1rec = REY1REC(0),
    rey2rec = REY2REC(0),
    rey3rec = REY3REC(0),
    rey4rec = REY4REC(0),
    rey5rec = REY5REC(0)
  )

  expect_equal(as.numeric(result), 0)
})
