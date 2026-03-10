# ---------------------------------------------------------------------------
# Happy path
# ---------------------------------------------------------------------------

test_that("list_npsych_scores() returns a character vector", {
  result <- list_npsych_scores()
  expect_type(result, "character")
})

test_that("list_npsych_scores() returns all known internal subclasses", {
  internal_classes <- c(
    "ANIMALS",
    "BOSTON",
    "CRAFTDRE",
    "CRAFTDVR",
    "CRAFTURS",
    "CRAFTVRS",
    "DIGBACCT",
    "DIGBACLS",
    "DIGFORCT",
    "DIGFORSL",
    "DIGIB",
    "DIGIBLEN",
    "DIGIF",
    "DIGIFLEN",
    "LOGIMEM",
    "MEMUNITS",
    "MINTTOTS",
    "MOCATOTS",
    "NACCMMSE",
    "OTRAILA",
    "OTRAILB",
    "OTRLBRR",
    "REY6REC",
    "REYAREC",
    "REYDLIST",
    "REYDREC",
    "REYTOTAL",
    "TRAILA",
    "TRAILB",
    "UDSBENTC",
    "UDSBENTD",
    "UDSVERFC",
    "UDSVERLC",
    "UDSVERTN",
    "VEG",
    "WAIS"
  )

  result <- list_npsych_scores()

  expect_true(all(internal_classes %in% result))
})

test_that("list_npsych_scores() picks up dynamically registered classes", {
  new_scores <- new_npsych_scores(
    "DynamicListTest1",
    label = "Dynamic Test",
    range = c(0, 100)
  )

  assign("new_scores", new_scores, envir = .GlobalEnv)
  withr::defer(rm("new_scores", envir = .GlobalEnv))

  expect_true("DynamicListTest1" %in% list_npsych_scores())
})

test_that("list_npsych_scores() does not include unregistered names", {
  expect_false("nonexistent_class" %in% list_npsych_scores())
})
