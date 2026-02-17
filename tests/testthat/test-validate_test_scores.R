test_that("validate_test_scores returns x invisibly on success", {
  x <- new_test_scores(
    scores = c(1, 5, 10),
    label = "My Test",
    range = c(1, 10),
    codes = c(missing = -99),
    class = "my_test"
  )

  expect_equal(x, validate_test_scores(x))

  result <- validate_test_scores(x)
  expect_equal(as.numeric(result), c(1, 5, 10))
})


# ---------------------------------------------------------------------------
# Class validation
# ---------------------------------------------------------------------------

test_that("validate_test_scores errors when no additional class is present", {
  # Manually build an object whose only class is "test_scores", so that
  # setdiff(class(x), "test_scores") returns character(0).
  x <- new_test_scores(
    scores = c(1, 5),
    label = "My Test",
    range = c(1, 10),
    codes = c(missing = -99),
    class = character() # results in class = "test_scores" only
  )

  expect_error(validate_test_scores(x), regexp = "class")
})

test_that("validate_test_scores errors when cls has length > 1", {
  # new_test_scores prepends all values in `class`, so passing two values
  # produces a three-element class vector; setdiff drops "test_scores" leaving 2.
  x <- new_test_scores(
    scores = c(1, 5),
    label = "My Test",
    range = c(1, 10),
    codes = c(missing = -99),
    class = c("foo", "bar")
  )

  expect_error(validate_test_scores(x), regexp = "length one")
})


# ---------------------------------------------------------------------------
# Label validation
# ---------------------------------------------------------------------------

test_that("validate_test_scores errors when label is not a character", {
  x <- new_test_scores(
    scores = c(1, 5),
    label = 42, # numeric, not character
    range = c(1, 10),
    codes = c(missing = -99),
    class = "my_test"
  )

  expect_error(validate_test_scores(x), regexp = "label")
})

test_that("validate_test_scores errors when label has length > 1", {
  x <- new_test_scores(
    scores = c(1, 5),
    label = c("foo", "bar"), # length 2
    range = c(1, 10),
    codes = c(missing = -99),
    class = "my_test"
  )

  expect_error(validate_test_scores(x), regexp = "label")
})


# ---------------------------------------------------------------------------
# Range validation
# ---------------------------------------------------------------------------

test_that("validate_test_scores errors when range is not numeric", {
  x <- new_test_scores(
    scores = c(1, 5),
    label = "My Test",
    range = c("1", "10"), # character, not numeric
    codes = c(missing = -99),
    class = "my_test"
  )

  expect_error(validate_test_scores(x), regexp = "range")
})

test_that("validate_test_scores errors when range is not length 2", {
  x_too_long <- new_test_scores(
    scores = c(1, 5),
    label = "My Test",
    range = c(0, 10, 20), # length 3
    codes = c(missing = -99),
    class = "my_test"
  )
  expect_error(validate_test_scores(x_too_long), regexp = "range")

  x_too_short <- new_test_scores(
    scores = 5,
    label = "My Test",
    range = 10, # length 1
    codes = c(missing = -99),
    class = "my_test"
  )
  expect_error(validate_test_scores(x_too_short), regexp = "range")
})


# ---------------------------------------------------------------------------
# Codes validation
# ---------------------------------------------------------------------------

test_that("validate_test_scores errors when codes is not numeric", {
  x <- new_test_scores(
    scores = c(1, 5),
    label = "My Test",
    range = c(1, 10),
    codes = c(missing = "N/A"), # named but character
    class = "my_test"
  )

  expect_error(validate_test_scores(x), regexp = "codes")
})

test_that("validate_test_scores errors when codes is unnamed", {
  x <- new_test_scores(
    scores = c(1, 5),
    label = "My Test",
    range = c(1, 10),
    codes = c(-99, -98), # numeric but no names
    class = "my_test"
  )

  expect_error(validate_test_scores(x), regexp = "codes")
})


# ---------------------------------------------------------------------------
# Score-range validation
# ---------------------------------------------------------------------------

test_that("validate_test_scores passes when all scores are within range", {
  x <- new_test_scores(
    scores = c(1, 5, 10),
    label = "My Test",
    range = c(1, 10),
    codes = c(missing = -99),
    class = "my_test"
  )

  expect_no_error(validate_test_scores(x))
})

test_that("validate_test_scores passes when scores equal the boundary values exactly", {
  x <- new_test_scores(
    scores = c(0, 100), # exactly the range endpoints
    label = "My Test",
    range = c(0, 100),
    codes = c(missing = -99),
    class = "my_test"
  )

  expect_no_error(validate_test_scores(x))
})

test_that("validate_test_scores passes when out-of-range scores are valid error codes", {
  x <- new_test_scores(
    scores = c(5, -99), # -99 is outside range but is a code
    label = "My Test",
    range = c(0, 10),
    codes = c(missing = -99),
    class = "my_test"
  )

  expect_no_error(validate_test_scores(x))
})

test_that("validate_test_scores passes when multiple codes are present in scores", {
  x <- new_test_scores(
    scores = c(5, -99, -98),
    label = "My Test",
    range = c(0, 10),
    codes = c(missing = -99, refused = -98),
    class = "my_test"
  )

  expect_no_error(validate_test_scores(x))
})

test_that("validate_test_scores errors when a score is out of range and not a code", {
  x <- new_test_scores(
    scores = c(5, 999), # 999 is neither in range nor a code
    label = "My Test",
    range = c(0, 10),
    codes = c(missing = -99),
    class = "my_test"
  )

  expect_error(validate_test_scores(x), regexp = "scores")
})

test_that("validate_test_scores error message mentions the range and the codes", {
  x <- new_test_scores(
    scores = c(5, 999),
    label = "My Test",
    range = c(0, 10),
    codes = c(missing = -99),
    class = "my_test"
  )

  # The error should reference the valid range and the code value
  expect_error(validate_test_scores(x), regexp = "0")
  expect_error(validate_test_scores(x), regexp = "10")
  expect_error(validate_test_scores(x), regexp = "-99")
})
