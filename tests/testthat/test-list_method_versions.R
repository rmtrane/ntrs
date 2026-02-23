# Test file for list_method_versions() / get_versions.npsych_scores()

# Test 1: S3 generic structure ----
test_that("list_method_versions() is an S3 generic", {
  skip_on_covr()
  expect_true(is.function(list_method_versions))
  expect_true(utils::isS3stdGeneric(list_method_versions))
})

# Test 2: Error handling for non-existent methods ----

test_that("list_method_versions() errors when method doesn't exist", {
  test_obj <- MOCATOTS(c(25, 28, 30))

  # When NO versions exist at all
  # This test requires a fresh environment or clearing .std_versions
  # For now, we test with a method that definitely doesn't exist

  testthat::local_reproducible_output()

  expect_error(
    list_method_versions(test_obj, "nonexistent_method"),
    "No versions registered for method"
  )

  testthat::local_reproducible_output()

  expect_error(
    list_method_versions(test_obj, "fake_standardization"),
    "No versions registered for method"
  )
})

test_that("list_method_versions() provides helpful error with available methods", {
  test_obj <- MOCATOTS(c(25, 28, 30))

  # Error should mention available methods when some exist
  testthat::local_reproducible_output()

  expect_error(
    list_method_versions(test_obj, "nonexistent_method"),
    "Available methods:"
  )
})

test_that("list_method_versions() errors when no versions registered yet", {
  # Create a new test class that hasn't been set up yet
  new_test_obj <- structure(
    c(10, 20, 30),
    class = c("BrandNewTestClass", "npsych_scores")
  )

  # Define std_using method for this class
  std_using_norms.BrandNewTestClass <- function(scores, ...) {
    scores
  }

  # Should error because no versions have been registered
  testthat::local_reproducible_output()

  expect_error(
    list_method_versions(new_test_obj, "norms"),
    "No versions registered for test class"
  )
})

# Test 3: Error handling for non-existent test classes ----

test_that("list_method_versions() errors when scores doesn't exist for method", {
  # Create a new test class
  new_test_obj <- structure(
    c(10, 20, 30),
    class = c("UnregisteredTestClass", "npsych_scores")
  )

  # Define std_using method
  std_using_norms.UnregisteredTestClass <- function(scores, ...) {
    scores
  }

  # Method 'norms' exists in .std_versions but not for this test class
  testthat::local_reproducible_output()

  expect_error(
    list_method_versions(new_test_obj, "norms"),
    "No versions registered for test class"
  )
})

test_that("list_method_versions() provides helpful error with available test classes", {
  # Create a new test class
  new_test_obj <- structure(
    c(10, 20, 30),
    class = c("UnregisteredTestClass2", "npsych_scores")
  )

  # Define std_using method
  std_using_norms.UnregisteredTestClass2 <- function(scores, ...) {
    scores
  }

  # Error should mention available test classes
  testthat::local_reproducible_output()

  expect_error(
    list_method_versions(new_test_obj, "norms"),
    "Available test classes:"
  )
})

# Test 4: Successful retrieval of versions ----

test_that("list_method_versions() returns character vector of version names", {
  test_obj <- MOCATOTS(c(25, 28, 30))

  # Get versions for a method that has registered versions
  versions <- list_method_versions(test_obj, "norms")

  expect_type(versions, "character")
  expect_true(length(versions) > 0)
})

test_that("list_method_versions() returns all registered versions for norms", {
  test_obj <- MOCATOTS(c(25, 28, 30))

  # MOCATOTS should have "nacc" and "updated" versions registered
  versions <- list_method_versions(test_obj, "norms")

  expect_true("nacc" %in% versions)
  expect_true("updated" %in% versions)
})

test_that("list_method_versions() returns all registered versions for regression", {
  test_obj <- MOCATOTS(c(25, 28, 30))

  # MOCATOTS should have multiple regression versions
  versions <- list_method_versions(test_obj, "regression")

  expect_true("updated_2024.06" %in% versions)
  expect_true("updated_2025.06" %in% versions)
  expect_true("nacc" %in% versions)
})

# Test 5: Correct class extraction ----

test_that("list_method_versions() correctly extracts scores from object", {
  test_obj <- MOCATOTS(c(25, 28, 30))

  # Should work even though test_obj has class c("MOCATOTS", "npsych_scores")
  # Function should use "MOCATOTS" after setdiff
  expect_silent(list_method_versions(test_obj, "norms"))
})

# Test 6: Integration with registration functions ----

test_that("list_method_versions() sees newly registered versions", {
  # Register a new version
  lookup_table <- data.frame(
    age = factor(c("60-69", "70-79")),
    sex = factor(c("m", "f"), levels = c("m", "f")),
    educ = factor(c("12-15", "16+")),
    m = c(25.0, 26.5),
    sd = c(3.0, 2.8)
  )

  suppressMessages(
    register_norms_version(
      scores = MOCATOTS(),
      version = "test_get_versions_v1",
      lookup_table = lookup_table,
      covar_fns = list(
        age = \(x) factor(ifelse(x < 70, "60-69", "70-79")),
        sex = \(x) factor(x, levels = c(1, 2), labels = c("m", "f")),
        educ = \(x) factor(ifelse(x < 16, "12-15", "16+"))
      )
    )
  )

  # Should now appear in versions list
  versions <- list_method_versions(MOCATOTS(), "norms")
  expect_true("test_get_versions_v1" %in% versions)
})

test_that("list_method_versions() reflects multiple new registrations", {
  # Get current count
  versions_before <- list_method_versions(MOCATOTS(), "regression")
  count_before <- length(versions_before)

  # Register two new regression versions
  coefs_v1 <- data.frame(
    intercept = 20.0,
    age = -0.1,
    sex = 1.5,
    educ = 0.5,
    rmse = 3.0
  )

  suppressMessages(
    register_regression_version(
      scores = MOCATOTS(),
      version = "test_get_versions_reg_v1",
      coefs = coefs_v1,
      covar_fns = list(
        age = \(x) pmin(pmax(x, 0), 110),
        sex = \(x) as.numeric(x == 2),
        educ = \(x) pmin(pmax(x, 0), 31)
      )
    )
  )

  coefs_v2 <- data.frame(
    intercept = 21.0,
    age = -0.12,
    sex = 1.6,
    educ = 0.52,
    rmse = 2.9
  )

  suppressMessages(
    register_regression_version(
      scores = MOCATOTS(),
      version = "test_get_versions_reg_v2",
      coefs = coefs_v2,
      covar_fns = list(
        age = \(x) pmin(pmax(x, 0), 110),
        sex = \(x) as.numeric(x == 2),
        educ = \(x) pmin(pmax(x, 0), 31)
      )
    )
  )

  # Should now have 2 more versions
  versions_after <- list_method_versions(MOCATOTS(), "regression")
  expect_equal(length(versions_after), count_before + 2)
  expect_true("test_get_versions_reg_v1" %in% versions_after)
  expect_true("test_get_versions_reg_v2" %in% versions_after)
})

# Test 7: Method name case sensitivity ----

test_that("list_method_versions() is case-sensitive for method names", {
  test_obj <- MOCATOTS(c(25, 28, 30))

  # Should error with wrong case
  testthat::local_reproducible_output()

  expect_error(
    list_method_versions(test_obj, "Norms"),
    "No versions registered for method"
  )

  testthat::local_reproducible_output()

  expect_error(
    list_method_versions(test_obj, "REGRESSION"),
    "No versions registered for method"
  )
})

# Test 8: Multiple test classes ----

test_that("list_method_versions() correctly distinguishes between test classes", {
  # Create two different test class objects
  test_obj1 <- MOCATOTS(c(25, 28, 30))

  test_obj2 <- structure(
    c(10, 20, 30),
    class = c("AnotherTestClass", "npsych_scores")
  )

  # Define std_using method for second class
  std_using_norms.AnotherTestClass <- function(scores, ...) {
    scores
  }

  # Register a version for the second class
  lookup_table <- data.frame(
    age = factor("60-69"),
    m = 15.0,
    sd = 2.0
  )

  suppressMessages(register_norms_version(
    scores = test_obj2,
    version = "another_class_version",
    lookup_table = lookup_table,
    covar_fns = list(
      age = \(x) factor("60-69")
    )
  ))

  # get_versions should return different results for each class
  versions1 <- list_method_versions(test_obj1, "norms")
  versions2 <- list_method_versions(test_obj2, "norms")

  expect_true("nacc" %in% versions1)
  expect_false("nacc" %in% versions2)
  expect_true("another_class_version" %in% versions2)
  expect_false("another_class_version" %in% versions1)
})

# Test 9: Empty version list edge case ----

test_that("list_method_versions() handles test class with no versions gracefully", {
  # This scenario should be caught by earlier validation
  # but we test the behavior at the boundary

  test_obj <- structure(
    c(10, 20, 30),
    class = c("EmptyVersionsTestClass", "npsych_scores")
  )

  std_using_norms.EmptyVersionsTestClass <- function(scores, ...) {
    scores
  }

  testthat::local_reproducible_output()

  expect_error(
    list_method_versions(test_obj, "norms"),
    "No versions registered for test class"
  )
})

# Test 10: No method edge case ----

test_that("list_method_versions() handles method without versions gracefully", {
  saved_keys <- ls(.std_versions)
  saved_envs <- mget(saved_keys, envir = .std_versions)

  # Wipe the registry
  rm(list = saved_keys, envir = .std_versions)

  # Restore by putting the saved references back — no re-registration needed
  withr::defer({
    rm(list = ls(.std_versions), envir = .std_versions)
    for (nm in names(saved_envs)) {
      assign(nm, saved_envs[[nm]], envir = .std_versions)
    }
  })

  # rm(list = ls(.std_versions), envir = .std_versions)

  # ## Rerun setup
  # withr::defer({
  #   suppressMessages(NpsychBatteryNormsS3:::.onLoad())
  # })

  testthat::local_reproducible_output()

  expect_error(
    list_method_versions(MOCATOTS(), "norms"),
    "No versions have been registered for any methods yet"
  )
})
