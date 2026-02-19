test_that("'list_npsych_scores()' picks up new method", {
  assign(
    "std_using_new_method",
    function(x) UseMethod("std_using_new_method"),
    envir = .GlobalEnv
  )
  assign(
    "std_using_new_method.new_class",
    function(x) "result",
    envir = .GlobalEnv
  )

  # Clean up after the test regardless of pass/fail
  withr::defer({
    rm("std_using_new_method", envir = .GlobalEnv)
    rm("std_using_new_method.new_class", envir = .GlobalEnv)
  })

  # new_class has a method, but is not a function
  expect_false("new_class" %in% list_npsych_scores())

  assign(
    "new_class",
    function(scores = numeric()) {
      npsych_scores(
        scores,
        label = "New Class",
        range = c(0, 10),
        codes = c("Not Available" = 99),
        class = "new_class"
      )
    },
    envir = .GlobalEnv
  )

  withr::defer({
    rm("new_class", envir = .GlobalEnv)
  })

  expect_true("new_class" %in% list_npsych_scores())
})

# Registry source ----

test_that("list_npsych_scores() finds subclass with registered norms version", {
  # expect_true("MOCATOTS" %in% list_npsych_scores())
  expect_equal(
    "MOCATOTS",
    list_npsych_scores()
  )
})

test_that("list_npsych_scores() finds subclass with registered regression version", {
  # MOCATOTS has both, so use a class registered only under regression
  # to isolate this path. We assign a valid constructor and register directly.
  assign(
    "reg_only_class",
    function(scores = numeric()) {
      npsych_scores(
        scores,
        label = "Regression Only",
        range = c(0, 10),
        codes = numeric(),
        class = "reg_only_class"
      )
    },
    envir = .GlobalEnv
  )
  withr::defer(rm("reg_only_class", envir = .GlobalEnv))

  assign(
    "std_using_regression.reg_only_class",
    function(x, ...) "result",
    envir = .GlobalEnv
  )
  withr::defer(rm("std_using_regression.reg_only_class", envir = .GlobalEnv))

  .register_std_version(
    reg_only_class(),
    method = "regression",
    version = "v1",
    data = list(),
    description = "test"
  )
  withr::defer(rm("reg_only_class", envir = .std_versions[["regression"]]))

  expect_true("reg_only_class" %in% list_npsych_scores())
})

test_that("list_npsych_scores() does not return phantom registry entries failing .is_valid_npsych_scores()", {
  # Directly insert a fake class into the registry without a constructor
  if (!exists("norms", envir = .std_versions, inherits = FALSE)) {
    .std_versions[["norms"]] <- new.env(parent = emptyenv())
  }
  .std_versions[["norms"]][["phantom_class"]] <- new.env(parent = emptyenv())
  withr::defer(rm("phantom_class", envir = .std_versions[["norms"]]))

  expect_false("phantom_class" %in% list_npsych_scores())
})

# S3 scan source ----

test_that("list_npsych_scores() does not return class scraped from S3 name that fails .is_valid_npsych_scores()", {
  assign(
    "std_using_new_method.not_a_real_class",
    function(x) "result",
    envir = .GlobalEnv
  )
  withr::defer(rm("std_using_new_method.not_a_real_class", envir = .GlobalEnv))

  expect_false("not_a_real_class" %in% list_npsych_scores())
})

# Combined behaviour ----

test_that("list_npsych_scores() does not return duplicates for classes in both registry and S3 scan", {
  # MOCATOTS is in the registry and has S3 methods in the loaded namespace
  result <- list_npsych_scores()
  expect_equal(sum(result == "MOCATOTS"), 1L)
})

test_that("list_npsych_scores() returns all known internal subclasses", {
  result <- list_npsych_scores()

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

  expect_true(all(internal_classes %in% result))
})
