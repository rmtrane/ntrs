# Helper to quickly build a valid test_scores object
make_test_scores <- function(scores, codes = c(missing = -99)) {
  new_test_scores(
    scores = scores,
    label = "My Test",
    range = c(0, 10),
    codes = codes,
    class = "my_test"
  )
}


# ---------------------------------------------------------------------------
# Happy path
# ---------------------------------------------------------------------------

test_that("remove_errorcodes returns a plain numeric vector", {
  x <- make_test_scores(c(1, 5, 10))
  result <- remove_errorcodes(x)

  expect_true(is.numeric(result))
  expect_false(inherits(result, "test_scores"))
})

test_that("remove_errorcodes replaces a single code with NA", {
  x <- make_test_scores(c(1, 5, -99))
  result <- remove_errorcodes(x)

  expect_equal(result, c(1, 5, NA))
})

test_that("remove_errorcodes replaces multiple distinct codes with NA", {
  x <- new_test_scores(
    scores = c(1, 5, -99, -98),
    label = "My Test",
    range = c(0, 10),
    codes = c("missing" = -99, "refused" = -98),
    class = "my_test"
  )

  result <- remove_errorcodes(x)

  expect_equal(result, c(1, 5, NA, NA))
})

test_that("remove_errorcodes replaces all values when all are codes", {
  x <- new_test_scores(
    scores = c(-99, -99, -98),
    label = "My Test",
    range = c(0, 10),
    codes = c(missing = -99, refused = -98),
    class = "my_test"
  )
  result <- remove_errorcodes(x)

  expect_equal(result, c(NA_integer_, NA_integer_, NA_integer_))
})

test_that("remove_errorcodes leaves valid scores untouched when no codes are present", {
  x <- make_test_scores(c(1, 5, 10))
  result <- remove_errorcodes(x)

  expect_equal(result, c(1, 5, 10))
})

test_that("remove_errorcodes does not introduce NAs when scores contain no coded values", {
  x <- make_test_scores(c(1, 5, 10))
  result <- remove_errorcodes(x)

  expect_false(anyNA(result))
})

test_that("remove_errorcodes strips test_scores attributes from the result", {
  x <- make_test_scores(c(1, 5, -99))
  result <- remove_errorcodes(x)

  expect_null(attr(result, "label"))
  expect_null(attr(result, "range"))
  expect_null(attr(result, "codes"))
})


# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("remove_errorcodes errors when x is a plain numeric vector", {
  testthat::local_reproducible_output()

  expect_error(
    remove_errorcodes(c(1, 5, 10)),
    regexp = "test_scores"
  )
})

test_that("remove_errorcodes errors when x is a data frame", {
  testthat::local_reproducible_output()

  expect_error(
    remove_errorcodes(data.frame(x = 1:3)),
    regexp = "test_scores"
  )
})

test_that("remove_errorcodes errors when x is NULL", {
  testthat::local_reproducible_output()

  expect_error(
    remove_errorcodes(NULL),
    regexp = "test_scores"
  )
})
