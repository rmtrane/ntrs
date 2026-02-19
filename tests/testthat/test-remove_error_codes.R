# Helper to quickly build a valid npsych_scores object
make_npsych_scores <- function(scores, codes = c(missing = -99)) {
  new_npsych_scores(
    scores = scores,
    label = "My Test",
    range = c(0, 10),
    codes = codes,
    subclass = "my_test"
  )
}


# ---------------------------------------------------------------------------
# Happy path
# ---------------------------------------------------------------------------

test_that("remove_error_codes returns a plain numeric vector", {
  x <- make_npsych_scores(c(1, 5, 10))
  result <- remove_error_codes(x)

  expect_true(is.numeric(result))
  expect_false(inherits(result, "npsych_scores"))
})

test_that("remove_error_codes replaces a single code with NA", {
  x <- make_npsych_scores(c(1, 5, -99))
  result <- remove_error_codes(x)

  expect_equal(result, c(1, 5, NA))
})

test_that("remove_error_codes replaces multiple distinct codes with NA", {
  x <- new_npsych_scores(
    scores = c(1, 5, -99, -98),
    label = "My Test",
    range = c(0, 10),
    codes = c("missing" = -99, "refused" = -98),
    subclass = "my_test"
  )

  result <- remove_error_codes(x)

  expect_equal(result, c(1, 5, NA, NA))
})

test_that("remove_error_codes replaces all values when all are codes", {
  x <- new_npsych_scores(
    scores = c(-99, -99, -98),
    label = "My Test",
    range = c(0, 10),
    codes = c(missing = -99, refused = -98),
    subclass = "my_test"
  )
  result <- remove_error_codes(x)

  expect_equal(result, c(NA_integer_, NA_integer_, NA_integer_))
})

test_that("remove_error_codes leaves valid scores untouched when no codes are present", {
  x <- make_npsych_scores(c(1, 5, 10))
  result <- remove_error_codes(x)

  expect_equal(result, c(1, 5, 10))
})

test_that("remove_error_codes does not introduce NAs when scores contain no coded values", {
  x <- make_npsych_scores(c(1, 5, 10))
  result <- remove_error_codes(x)

  expect_false(anyNA(result))
})

test_that("remove_error_codes strips npsych_scores attributes from the result", {
  x <- make_npsych_scores(c(1, 5, -99))
  result <- remove_error_codes(x)

  expect_null(attr(result, "label"))
  expect_null(attr(result, "range"))
  expect_null(attr(result, "codes"))
})


# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("remove_error_codes errors when x is a plain numeric vector", {
  testthat::local_reproducible_output()

  expect_error(
    remove_error_codes(c(1, 5, 10)),
    regexp = "npsych_scores"
  )
})

test_that("remove_error_codes errors when x is a data frame", {
  testthat::local_reproducible_output()

  expect_error(
    remove_error_codes(data.frame(x = 1:3)),
    regexp = "npsych_scores"
  )
})

test_that("remove_error_codes errors when x is NULL", {
  testthat::local_reproducible_output()

  expect_error(
    remove_error_codes(NULL),
    regexp = "npsych_scores"
  )
})
