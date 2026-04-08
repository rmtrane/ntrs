# ===========================================================================
# replace_codes
# ===========================================================================

test_that("replace_codes replaces error codes with their labels", {
  x <- MOCATOTS(c(25, 88, -4, 28))
  result <- replace_codes(x)

  expect_type(result, "character")
  expect_equal(result[1], "25")
  expect_equal(result[4], "28")
  # Error code positions should now contain label strings
  expect_true(result[2] != "88")
  expect_true(result[3] != "-4")
})

test_that("replace_codes leaves values unchanged when no codes are present", {
  x <- MOCATOTS(c(25, 28))
  result <- replace_codes(x)

  expect_equal(result, c("25", "28"))
})

test_that("replace_codes errors on non-npsych_scores input", {
  testthat::local_reproducible_output()

  expect_error(
    replace_codes(42),
    regexp = "npsych_scores"
  )
})

# ===========================================================================
# is_npsych_scores
# ===========================================================================

test_that("is_npsych_scores returns TRUE for npsych_scores objects", {
  expect_true(is_npsych_scores(MOCATOTS(25)))
})

test_that("is_npsych_scores returns FALSE for numeric", {
  expect_false(is_npsych_scores(42))
})

test_that("is_npsych_scores returns FALSE for character", {
  expect_false(is_npsych_scores("foo"))
})
