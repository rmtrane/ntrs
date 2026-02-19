# .is_valid_std_method() tests ----

test_that(".is_valid_std_method() returns TRUE for method implemented via npsych_scores generic", {
  # std_using_norms.npsych_scores exists, so this resolves via inheritance
  expect_true(.is_valid_std_method("norms", MOCATOTS()))
})

test_that(".is_valid_std_method() returns TRUE for method implemented via class-specific S3 method", {
  assign(
    "std_using_new_method",
    function(x, ...) UseMethod("std_using_new_method"),
    envir = .GlobalEnv
  )
  assign(
    "std_using_new_method.MOCATOTS",
    function(x, ...) "result",
    envir = .GlobalEnv
  )
  withr::defer({
    rm("std_using_new_method", envir = .GlobalEnv)
    rm("std_using_new_method.MOCATOTS", envir = .GlobalEnv)
  })

  expect_true(.is_valid_std_method("new_method", MOCATOTS()))
})

test_that(".is_valid_std_method() returns FALSE for a non-existent method", {
  expect_false(.is_valid_std_method("nonexistent_method", MOCATOTS()))
})

test_that(".is_valid_std_method() returns FALSE when generic exists but is not an S3 generic", {
  assign(
    "std_using_not_s3",
    function(x, ...) "not a generic",
    envir = .GlobalEnv
  )
  withr::defer(rm("std_using_not_s3", envir = .GlobalEnv))

  expect_false(.is_valid_std_method("not_s3", MOCATOTS()))
})

test_that(".is_valid_std_method() returns FALSE when generic exists but has no method for the class", {
  assign(
    "std_using_orphan_method",
    function(x, ...) UseMethod("std_using_orphan_method"),
    envir = .GlobalEnv
  )
  withr::defer(rm("std_using_orphan_method", envir = .GlobalEnv))

  expect_false(.is_valid_std_method("orphan_method", MOCATOTS()))
})

# list_std_methods() tests ----

test_that("list_std_methods() returns norms and regression for MOCATOTS", {
  result <- list_std_methods(MOCATOTS())
  expect_true("norms" %in% result)
  expect_true("regression" %in% result)
})

test_that("list_std_methods() finds method defined in .GlobalEnv for existing class", {
  assign(
    "std_using_new_method",
    function(x, ...) UseMethod("std_using_new_method"),
    envir = .GlobalEnv
  )
  assign(
    "std_using_new_method.MOCATOTS",
    function(x, ...) "result",
    envir = .GlobalEnv
  )
  withr::defer({
    rm("std_using_new_method", envir = .GlobalEnv)
    rm("std_using_new_method.MOCATOTS", envir = .GlobalEnv)
  })

  expect_true("new_method" %in% list_std_methods(MOCATOTS()))
})

test_that("list_std_methods() finds method implemented via npsych_scores generic for new class", {
  assign(
    "new_class",
    function(scores = numeric()) {
      npsych_scores(
        scores,
        label = "New Class",
        range = c(0, 10),
        codes = numeric(),
        subclass = "new_class"
      )
    },
    envir = .GlobalEnv
  )
  withr::defer(rm("new_class", envir = .GlobalEnv))

  # norms and regression are both implemented via std_using_*.npsych_scores,
  # but without versions. Hence, should not be surfaced by list_std_methods...
  result <- list_std_methods(new_class())

  expect_false("norms" %in% result)
  expect_false("regression" %in% result)

  # ... but should return TRUE by .is_valid_std_method
  expect_true(
    .is_valid_std_method("norms", new_class())
  )
  expect_true(
    .is_valid_std_method("regression", new_class())
  )
})

test_that("list_std_methods() does not return 'defaults' from registry", {
  expect_false("defaults" %in% list_std_methods(MOCATOTS()))
})

test_that("list_std_methods() does not return duplicates when method is in both registry and S3 scan", {
  result <- list_std_methods(MOCATOTS())
  expect_equal(sum(result == "norms"), 1L)
  expect_equal(sum(result == "regression"), 1L)
})

test_that("list_std_methods() returns empty character vector for class with no methods or registered versions", {
  assign(
    "bare_class",
    function(scores = numeric()) {
      npsych_scores(
        scores,
        label = "Bare Class",
        range = c(0, 10),
        codes = numeric(),
        subclass = "bare_class"
      )
    },
    envir = .GlobalEnv
  )
  withr::defer(rm("bare_class", envir = .GlobalEnv))

  expect_equal(list_std_methods(bare_class()), character(0))
})

test_that(".is_valid_std_method() errors when scores is not a npsych_scores object", {
  testthat::local_reproducible_output()

  expect_error(
    .is_valid_std_method("norms", "MOCATOTS"),
    "must be a"
  )

  testthat::local_reproducible_output()

  expect_error(
    .is_valid_std_method("norms", 42),
    "must be a"
  )
})


test_that(".is_valid_std_method() errors when method is not a non-empty character string", {
  testthat::local_reproducible_output()

  expect_error(
    .is_valid_std_method(NULL, MOCATOTS()),
    "non-empty character string"
  )
  testthat::local_reproducible_output()

  expect_error(
    .is_valid_std_method(123, MOCATOTS()),
    "non-empty character string"
  )
  testthat::local_reproducible_output()

  expect_error(
    .is_valid_std_method("", MOCATOTS()),
    "non-empty character string"
  )
  testthat::local_reproducible_output()

  expect_error(
    .is_valid_std_method(c("norms", "regression"), MOCATOTS()),
    "non-empty character string"
  )
})
