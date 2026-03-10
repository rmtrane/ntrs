# ---------------------------------------------------------------------------
# Happy path
# ---------------------------------------------------------------------------

test_that("new_npsych_scores creates a callable S7 class", {
  my_test <- new_npsych_scores(
    "my_test_factory1",
    label = "My Test",
    range = c(1, 10),
    codes = c(missing = -99)
  )

  expect_true(is(my_test, "S7_class"))
  expect_no_error(my_test(c(1, 5, 10)))
})

test_that("new_npsych_scores instances inherit from npsych_scores", {
  my_test <- new_npsych_scores(
    "my_test_factory2",
    label = "My Test",
    range = c(1, 10),
    codes = c(missing = -99)
  )

  result <- my_test(c(1, 5, 10))

  expect_true(S7::S7_inherits(result, npsych_scores))
})

test_that("new_npsych_scores instances have correct values", {
  my_test <- new_npsych_scores(
    "my_test_factory3",
    label = "My Test",
    range = c(1, 10),
    codes = c(missing = -99)
  )

  result <- my_test(c(1, 5, 10))

  expect_equal(as.numeric(result), c(1, 5, 10))
})

test_that("new_npsych_scores fixes properties at definition time", {
  my_test <- new_npsych_scores(
    "my_test_factory4",
    label = "My Test",
    range = c(0, 50),
    codes = c(refused = 98)
  )

  result <- my_test(c(0, 25, 50, 98))

  expect_equal(result@label, "My Test")
  expect_equal(result@range, c(0, 50))
  expect_equal(result@codes, c(refused = 98))
})

test_that("new_npsych_scores defaults codes to numeric()", {
  my_test <- new_npsych_scores(
    "my_test_factory5",
    label = "My Test",
    range = c(0, 100)
  )

  result <- my_test(c(0, 50, 100))

  expect_equal(result@codes, numeric())
})

test_that("new_npsych_scores creates empty instance by default", {
  my_test <- new_npsych_scores(
    "my_test_factory6",
    label = "My Test",
    range = c(0, 100)
  )

  result <- my_test()

  expect_equal(as.numeric(result), numeric())
  expect_true(S7::S7_inherits(result, npsych_scores))
})

test_that("new_npsych_scores instances validate scores against range", {
  my_test <- new_npsych_scores(
    "my_test_factory8",
    label = "My Test",
    range = c(1, 10),
    codes = c(missing = -99)
  )

  expect_error(my_test(c(1, 999)), regexp = "scores")
})

test_that("new_npsych_scores instances accept error codes", {
  my_test <- new_npsych_scores(
    "my_test_factory9",
    label = "My Test",
    range = c(1, 10),
    codes = c(missing = -99)
  )

  expect_no_error(my_test(c(1, 5, -99)))
})


# ---------------------------------------------------------------------------
# Validation errors: factory arguments
# ---------------------------------------------------------------------------

test_that("new_npsych_scores errors when name is empty string", {
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

test_that("new_npsych_scores errors when name is not a single string", {
  testthat::local_reproducible_output()

  expect_error(
    new_npsych_scores(
      c("foo", "bar"),
      label = "My Test",
      range = c(1, 10)
    ),
    regexp = "name"
  )
})

test_that("new_npsych_scores errors when name is not character", {
  testthat::local_reproducible_output()

  expect_error(
    new_npsych_scores(
      123,
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

test_that("new_npsych_scores errors when range is not numeric", {
  testthat::local_reproducible_output()

  expect_error(
    new_npsych_scores(
      "bad_test",
      label = "My Test",
      range = c("a", "b")
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
