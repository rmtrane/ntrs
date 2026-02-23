# .is_valid_npsych_scores() tests ----

test_that(".is_valid_npsych_scores() errors when cls not string", {
  testthat::local_reproducible_output()

  expect_error(
    .is_valid_npsych_scores(1),
    ".+cls.+must be of class.+character.+"
  )

  testthat::local_reproducible_output()

  expect_error(
    .is_valid_npsych_scores(character()),
    ".+cls.+must be a single string, but is a character vector of length.+0+"
  )

  testthat::local_reproducible_output()

  expect_error(
    .is_valid_npsych_scores(c("MOCATOTS", "MINTTOTS")),
    ".+cls.+must be a single string, but is a character vector of length.+2+"
  )
})

test_that(".is_valid_npsych_scores() returns TRUE for a known internal subclass", {
  expect_true(.is_valid_npsych_scores("MOCATOTS"))
})

test_that(".is_valid_npsych_scores() returns FALSE for a string with no corresponding function", {
  expect_false(.is_valid_npsych_scores("nonexistent_class"))
})

test_that(".is_valid_npsych_scores() returns FALSE for a function that does not return a npsych_scores object", {
  expect_false(.is_valid_npsych_scores("mean"))
})

test_that(".is_valid_npsych_scores() returns FALSE for a constructor whose class does not match the string", {
  assign(
    "wrong_class",
    function() {
      npsych_scores(
        numeric(),
        label = "Wrong",
        range = c(0, 10),
        codes = numeric(),
        subclass = "actually_different_class"
      )
    },
    envir = .GlobalEnv
  )
  withr::defer(rm("wrong_class", envir = .GlobalEnv))

  expect_false(.is_valid_npsych_scores("wrong_class"))
})

test_that(".is_valid_npsych_scores() returns FALSE for 'npsych_scores' itself", {
  expect_false(.is_valid_npsych_scores("npsych_scores"))
})
