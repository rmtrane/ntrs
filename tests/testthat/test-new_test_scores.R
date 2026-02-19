# ---------------------------------------------------------------------------
# Happy path
# ---------------------------------------------------------------------------

test_that("new_test_scores returns a numeric vector with correct values", {
  result <- new_test_scores(
    scores = c(1, 5, 10),
    label = "My Test",
    range = c(1, 10),
    codes = c(missing = -99),
    class = "my_test"
  )

  expect_equal(as.numeric(result), c(1, 5, 10))
})

test_that("new_test_scores attaches label attribute correctly", {
  result <- new_test_scores(
    scores = c(1, 5),
    label = "My Test",
    range = c(1, 10),
    codes = c(missing = -99),
    class = "my_test"
  )

  expect_equal(attr(result, "label"), "My Test")
})

test_that("new_test_scores attaches range attribute correctly", {
  result <- new_test_scores(
    scores = c(1, 5),
    label = "My Test",
    range = c(1, 10),
    codes = c(missing = -99),
    class = "my_test"
  )

  expect_equal(attr(result, "range"), c(1, 10))
})

test_that("new_test_scores attaches codes attribute correctly", {
  result <- new_test_scores(
    scores = c(1, 5),
    label = "My Test",
    range = c(1, 10),
    codes = c(missing = -99),
    class = "my_test"
  )

  expect_equal(attr(result, "codes"), c(missing = -99))
})

test_that("new_test_scores sets class with custom class first, test_scores second", {
  result <- new_test_scores(
    scores = c(1, 5),
    label = "My Test",
    range = c(1, 10),
    codes = c(missing = -99),
    class = "my_test"
  )

  expect_equal(class(result), c("my_test", "test_scores"))
})

test_that("new_test_scores inherits test_scores", {
  result <- new_test_scores(
    scores = c(1, 5),
    label = "My Test",
    range = c(1, 10),
    codes = c(missing = -99),
    class = "my_test"
  )

  expect_s3_class(result, "test_scores")
})

test_that("new_test_scores accepts class = character() without erroring", {
  # new_test_scores has no guard against an empty class — that is
  # validate_test_scores' responsibility. Confirm the constructor itself
  # does not raise an error.
  expect_no_error(
    new_test_scores(
      scores = c(1, 5),
      label = "My Test",
      range = c(1, 10),
      codes = c(missing = -99),
      class = character()
    )
  )
})

test_that("new_test_scores accepts multiple class values without erroring", {
  # Again, the constructor does not enforce length — validate_test_scores does.
  expect_no_error(
    new_test_scores(
      scores = c(1, 5),
      label = "My Test",
      range = c(1, 10),
      codes = c(missing = -99),
      class = c("foo", "bar")
    )
  )
})


# ---------------------------------------------------------------------------
# scores validation
# ---------------------------------------------------------------------------

test_that("new_test_scores errors when scores is a character vector", {
  testthat::local_reproducible_output()

  expect_error(
    new_test_scores(
      scores = c("a", "b"),
      label = "My Test",
      range = c(1, 10),
      codes = c(missing = -99),
      class = "my_test"
    ),
    regexp = "scores"
  )
})

test_that("new_test_scores errors when scores is a logical vector", {
  testthat::local_reproducible_output()

  expect_error(
    new_test_scores(
      scores = c(TRUE, FALSE),
      label = "My Test",
      range = c(1, 10),
      codes = c(missing = -99),
      class = "my_test"
    ),
    regexp = "scores"
  )
})

test_that("new_test_scores errors when scores is NULL", {
  testthat::local_reproducible_output()

  expect_error(
    new_test_scores(
      scores = NULL,
      label = "My Test",
      range = c(1, 10),
      codes = c(missing = -99),
      class = "my_test"
    ),
    regexp = "scores"
  )
})


# ---------------------------------------------------------------------------
# class validation
# ---------------------------------------------------------------------------

test_that("new_test_scores errors when class is not a character vector", {
  testthat::local_reproducible_output()

  expect_error(
    new_test_scores(
      scores = c(1, 5),
      label = "My Test",
      range = c(1, 10),
      codes = c(missing = -99),
      class = 123
    ),
    regexp = "class"
  )
})

test_that("new_test_scores errors when class is NULL", {
  testthat::local_reproducible_output()

  expect_error(
    new_test_scores(
      scores = c(1, 5),
      label = "My Test",
      range = c(1, 10),
      codes = c(missing = -99),
      class = NULL
    ),
    regexp = "class"
  )
})
