# ---------------------------------------------------------------------------
# Happy path
# ---------------------------------------------------------------------------

test_that("get_npsych_scores returns the S7 class for a valid name", {
  cls <- get_npsych_scores("MOCATOTS")

  expect_true(is(cls, "S7_class"))
  expect_equal(cls@name, "MOCATOTS")
})

test_that("get_npsych_scores returns a callable constructor", {
  cls <- get_npsych_scores("MOCATOTS")
  result <- cls(c(25, 28))

  expect_true(S7::S7_inherits(result, MOCATOTS))
  expect_equal(as.numeric(result), c(25, 28))
})

# ---------------------------------------------------------------------------
# Error path
# ---------------------------------------------------------------------------

test_that("get_npsych_scores errors for a nonexistent subclass", {
  testthat::local_reproducible_output()

  expect_error(
    get_npsych_scores("DOESNOTEXIST"),
    regexp = "npsych_scores"
  )
})
