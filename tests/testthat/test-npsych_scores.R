# ---------------------------------------------------------------------------
# Happy path: base class
# ---------------------------------------------------------------------------

test_that("npsych_scores creates a valid object", {
  result <- npsych_scores(
    c(1, 5, 10),
    label = "My Test",
    range = c(1, 10),
    codes = c(missing = -99)
  )

  expect_true(S7::S7_inherits(result, npsych_scores))
  expect_equal(as.numeric(result), c(1, 5, 10))
})

test_that("npsych_scores properties are accessible via @", {
  result <- npsych_scores(
    c(1, 5, 10),
    label = "My Test",
    range = c(1, 10),
    codes = c(missing = -99)
  )

  expect_equal(result@label, "My Test")
  expect_equal(result@range, c(1, 10))
  expect_equal(result@codes, c(missing = -99))
})

test_that("npsych_scores accepts error codes as valid scores", {
  expect_no_error(
    npsych_scores(
      c(1, 5, -99),
      label = "My Test",
      range = c(1, 10),
      codes = c(missing = -99)
    )
  )
})

test_that("npsych_scores accepts NA as valid scores", {
  expect_no_error(
    npsych_scores(
      c(1, NA, 10),
      label = "My Test",
      range = c(1, 10),
      codes = c(missing = -99)
    )
  )
})

test_that("npsych_scores allows empty codes argument", {
  result <- npsych_scores(
    c(1, 5),
    label = "My Test",
    range = c(1, 10)
  )

  expect_equal(as.numeric(result), c(1, 5))
  expect_equal(result@label, "My Test")
  expect_equal(result@range, c(1, 10))
  expect_equal(result@codes, numeric())
})


# ---------------------------------------------------------------------------
# Validation errors
# ---------------------------------------------------------------------------

test_that("npsych_scores errors when scores are out of range and not a code", {
  testthat::local_reproducible_output()

  expect_error(
    npsych_scores(
      c(1, 999),
      label = "My Test",
      range = c(1, 10),
      codes = c(missing = -99)
    ),
    regexp = "scores"
  )
})

test_that("npsych_scores errors when label is not a single string", {
  testthat::local_reproducible_output()

  expect_error(
    npsych_scores(
      c(1, 5),
      label = c("foo", "bar"),
      range = c(1, 10),
      codes = c(missing = -99)
    ),
    regexp = "label"
  )
})

test_that("npsych_scores errors when range is not a numeric vector of length 2", {
  testthat::local_reproducible_output()

  expect_error(
    npsych_scores(
      c(1, 5),
      label = "My Test",
      range = c(1, 10, 20),
      codes = c(missing = -99)
    ),
    regexp = "range"
  )
})

test_that("npsych_scores errors when codes is unnamed", {
  testthat::local_reproducible_output()

  expect_error(
    npsych_scores(
      c(1, 5),
      label = "My Test",
      range = c(1, 10),
      codes = c(-99, -98)
    ),
    regexp = "codes"
  )
})


# ---------------------------------------------------------------------------
# Validation errors: new_npsych_scores factory
# ---------------------------------------------------------------------------

test_that("new_npsych_scores errors when name is empty", {
  testthat::local_reproducible_output()

  expect_error(
    new_npsych_scores(
      "",
      label = "My Test",
      range = c(1, 10)
    ),
    regexp = "name"
  )
})

test_that("new_npsych_scores errors when label is not a single string", {
  testthat::local_reproducible_output()

  expect_error(
    new_npsych_scores(
      "bad_test",
      label = c("foo", "bar"),
      range = c(1, 10)
    ),
    regexp = "label"
  )
})

test_that("new_npsych_scores errors when range is not length 2", {
  testthat::local_reproducible_output()

  expect_error(
    new_npsych_scores(
      "bad_test",
      label = "My Test",
      range = c(1, 10, 20)
    ),
    regexp = "range"
  )
})

test_that("new_npsych_scores errors when codes is unnamed", {
  testthat::local_reproducible_output()

  expect_error(
    new_npsych_scores(
      "bad_test",
      label = "My Test",
      range = c(1, 10),
      codes = c(-99, -98)
    ),
    regexp = "codes"
  )
})