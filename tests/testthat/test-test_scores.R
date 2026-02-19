# ---------------------------------------------------------------------------
# Happy path
# ---------------------------------------------------------------------------

test_that("test_scores returns an object with the correct values", {
  result <- test_scores(
    scores = c(1, 5, 10),
    label = "My Test",
    range = c(1, 10),
    codes = c(missing = -99),
    class = "my_test"
  )

  expect_equal(as.numeric(result), c(1, 5, 10))
})

test_that("test_scores returns an object inheriting test_scores", {
  result <- test_scores(
    scores = c(1, 5, 10),
    label = "My Test",
    range = c(1, 10),
    codes = c(missing = -99),
    class = "my_test"
  )

  expect_s3_class(result, "test_scores")
})

test_that("test_scores sets class with custom class first, test_scores second", {
  result <- test_scores(
    scores = c(1, 5, 10),
    label = "My Test",
    range = c(1, 10),
    codes = c(missing = -99),
    class = "my_test"
  )

  expect_equal(class(result), c("my_test", "test_scores"))
})

test_that("test_scores attaches attributes correctly", {
  result <- test_scores(
    scores = c(1, 5, 10),
    label = "My Test",
    range = c(1, 10),
    codes = c(missing = -99),
    class = "my_test"
  )

  expect_equal(attr(result, "label"), "My Test")
  expect_equal(attr(result, "range"), c(1, 10))
  expect_equal(attr(result, "codes"), c(missing = -99))
})

test_that("test_scores accepts error codes as valid scores", {
  expect_no_error(
    test_scores(
      scores = c(1, 5, -99),
      label = "My Test",
      range = c(1, 10),
      codes = c(missing = -99),
      class = "my_test"
    )
  )
})


# ---------------------------------------------------------------------------
# Equivalence with new_test_scores + validate_test_scores
# ---------------------------------------------------------------------------

test_that("test_scores produces the same result as new_test_scores + validate_test_scores", {
  args <- list(
    scores = c(1, 5, 10),
    label = "My Test",
    range = c(1, 10),
    codes = c(missing = -99),
    class = "my_test"
  )

  result_combined <- validate_test_scores(
    new_test_scores(args$scores, args$label, args$range, args$codes, args$class)
  )
  result_public <- test_scores(
    scores = args$scores,
    label = args$label,
    range = args$range,
    codes = args$codes,
    class = args$class
  )

  expect_equal(result_public, result_combined)
})


# ---------------------------------------------------------------------------
# Errors inherited from new_test_scores
# ---------------------------------------------------------------------------

test_that("test_scores errors when scores is not numeric", {
  testthat::local_reproducible_output()

  expect_error(
    test_scores(
      scores = c("a", "b"),
      label = "My Test",
      range = c(1, 10),
      codes = c(missing = -99),
      class = "my_test"
    ),
    regexp = "scores"
  )
})

test_that("test_scores errors when class is not a character vector", {
  testthat::local_reproducible_output()

  expect_error(
    test_scores(
      scores = c(1, 5),
      label = "My Test",
      range = c(1, 10),
      codes = c(missing = -99),
      class = 123
    ),
    regexp = "class"
  )
})


# ---------------------------------------------------------------------------
# Errors inherited from validate_test_scores
# ---------------------------------------------------------------------------

test_that("test_scores errors when class is empty", {
  testthat::local_reproducible_output()

  expect_error(
    test_scores(
      scores = c(1, 5),
      label = "My Test",
      range = c(1, 10),
      codes = c(missing = -99),
      class = character()
    ),
    regexp = "class"
  )
})

test_that("test_scores errors when scores are out of range and not a code", {
  testthat::local_reproducible_output()

  expect_error(
    test_scores(
      scores = c(1, 999),
      label = "My Test",
      range = c(1, 10),
      codes = c(missing = -99),
      class = "my_test"
    ),
    regexp = "scores"
  )
})

test_that("test_scores errors when label is not a single string", {
  testthat::local_reproducible_output()

  expect_error(
    test_scores(
      scores = c(1, 5),
      label = c("foo", "bar"),
      range = c(1, 10),
      codes = c(missing = -99),
      class = "my_test"
    ),
    regexp = "label"
  )
})

test_that("test_scores errors when range is not a numeric vector of length 2", {
  testthat::local_reproducible_output()

  expect_error(
    test_scores(
      scores = c(1, 5),
      label = "My Test",
      range = c(1, 10, 20),
      codes = c(missing = -99),
      class = "my_test"
    ),
    regexp = "range"
  )
})

test_that("test_scores errors when codes is unnamed", {
  testthat::local_reproducible_output()

  expect_error(
    test_scores(
      scores = c(1, 5),
      label = "My Test",
      range = c(1, 10),
      codes = c(-99, -98),
      class = "my_test"
    ),
    regexp = "codes"
  )
})


# ---------------------------------------------------------------------------
# Default argument behaviour (empty vectors)
# Each omitted argument flows through as an empty vector and should trigger
# a specific validation error, confirming the defaults are intentionally
# "must be supplied" rather than silently usable.
# ---------------------------------------------------------------------------

test_that("test_scores errors when label is omitted (defaults to character())", {
  testthat::local_reproducible_output()

  expect_error(
    test_scores(
      scores = c(1, 5),
      range = c(1, 10),
      codes = c(missing = -99),
      class = "my_test"
    ),
    regexp = "label"
  )
})

test_that("test_scores errors when range is omitted (defaults to numeric())", {
  testthat::local_reproducible_output()

  expect_error(
    test_scores(
      scores = c(1, 5),
      label = "My Test",
      codes = c(missing = -99),
      class = "my_test"
    ),
    regexp = "range"
  )
})

test_that("test_scores errors when class is omitted (defaults to character())", {
  testthat::local_reproducible_output()

  expect_error(
    test_scores(
      scores = c(1, 5),
      label = "My Test",
      range = c(1, 10),
      codes = c(missing = -99)
    ),
    regexp = "class"
  )
})

test_that("test_scores allows for empty codes argument", {
  new_ts <- test_scores(
    scores = c(1, 5),
    label = "My Test",
    range = c(1, 10),
    class = "my_test"
  )

  expect_equal(
    as.numeric(new_ts),
    c(1, 5)
  )
  expect_equal(
    attr(new_ts, "label"),
    "My Test"
  )
  expect_equal(
    attr(new_ts, "range"),
    c(1, 10)
  )

  expect_equal(
    class(new_ts),
    c("my_test", "test_scores")
  )

  expect_equal(
    attr(new_ts, "codes"),
    numeric()
  )
})
