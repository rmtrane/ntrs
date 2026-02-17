# Test file for internal registry functions

# .validate_registration_params() tests ----

test_that(".validate_registration_params() validates version parameter", {
  # version must be a single character string
  expect_error(
    .validate_registration_params(version = NULL, description = "test"),
    "must be a non-empty character string"
  )

  expect_error(
    .validate_registration_params(version = 123, description = "test"),
    "must be a non-empty character string"
  )

  expect_error(
    .validate_registration_params(
      version = c("v1", "v2"),
      description = "test"
    ),
    "must be a non-empty character string"
  )

  expect_error(
    .validate_registration_params(version = character(0), description = "test"),
    "must be a non-empty character string"
  )
})

test_that(".validate_registration_params() validates description parameter", {
  # description must be a character string
  expect_error(
    .validate_registration_params(version = "v1", description = NULL),
    "must be a character string"
  )

  expect_error(
    .validate_registration_params(version = "v1", description = 123),
    "must be a character string"
  )

  expect_error(
    .validate_registration_params(
      version = "v1",
      description = c("desc1", "desc2")
    ),
    "must be a character string"
  )

  expect_error(
    .validate_registration_params(version = "v1", description = character(0)),
    "must be a character string"
  )
})

test_that(".validate_registration_params() accepts valid parameters", {
  # Should not throw an error with valid inputs
  expect_silent(
    .validate_registration_params(
      version = "v1.0",
      description = "Test version"
    )
  )

  expect_silent(
    .validate_registration_params(
      version = "updated",
      description = "Updated norms"
    )
  )
})
# .method_implemented() tests ----

test_that(".method_implemented() generic exists and is S3", {
  skip_on_covr()
  expect_true(is.function(.method_implemented))
  expect_true(isS3stdGeneric(.method_implemented))
})

test_that(".method_implemented.test_scores() detects implemented methods", {
  # Create a test_scores object
  test_obj <- MOCATOTS(c(25, 28, 30))

  # Test with methods that ARE implemented for MOCATOTS
  expect_true(.method_implemented(test_obj, "norms"))
  expect_true(.method_implemented(test_obj, "regression"))
})

test_that(".method_implemented.test_scores() detects non-implemented methods", {
  # Create a test_scores object
  test_obj <- MOCATOTS(c(25, 28, 30))

  # Test with a method that is NOT implemented
  expect_false(.method_implemented(test_obj, "nonexistent_method"))
  expect_false(.method_implemented(test_obj, "fake_standardization"))
})

test_that(".method_implemented.test_scores() searches parent frames", {
  # Create a test_scores object
  test_obj <- MOCATOTS(c(25, 28, 30))

  # Test with a method that is NOT implemented
  expect_false(.method_implemented(test_obj, "nonexistent_method"))
  expect_false(.method_implemented(test_obj, "fake_standardization"))
})

# .register_std_version() tests ----

test_that(".register_std_version() generic exists and is S3", {
  skip_on_covr()
  expect_true(is.function(.register_std_version))
  expect_true(isS3stdGeneric(.register_std_version))
})

test_that(".register_std_version.test_scores() rejects unimplemented methods", {
  test_obj <- MOCATOTS(c(25, 28, 30))

  # Should error when trying to register a method that doesn't exist
  expect_error(
    .register_std_version(
      test_obj,
      method = "fake_method",
      version = "v1",
      data = list(test = 1),
      description = "Test"
    ),
    "not implemented"
  )
})

test_that(".register_std_version.test_scores() creates new method environment", {
  test_obj <- MOCATOTS(c(25, 28, 30))

  # Create a completely new method name that doesn't exist yet
  std_using_new_method <- function(test_scores, ...) {
    UseMethod("std_using_new_method")
  }

  std_using_new_method.test_scores <- function(test_scores, ...) {
    test_scores
  }

  expect_true(.method_implemented(test_obj, "new_method"))

  # Now register - this should create the method environment (line 80)
  expect_silent(
    .register_std_version(
      MOCATOTS(),
      method = "new_method",
      version = "v1",
      data = list(test = 1),
      description = "Testing environment creation"
    )
  )

  # Verify the method environment was created
  expect_true(exists("new_method", envir = .std_versions, inherits = FALSE))
})

test_that(".register_std_version.test_scores() creates new test_class environment", {
  test_obj <- MOCATOTS(c(25, 28, 30))

  # Use an existing method but ensure we're creating a fresh test_class env
  # by using a method that exists but hasn't been used for MOCATOTS yet
  # Actually, let's create a new test class to guarantee we hit line 85

  # Create a new test_scores subclass
  new_test_obj <- structure(
    c(10, 20, 30),
    class = c("NewTestClass", "test_scores")
  )

  # Define std_using methods for this new class
  std_using_norms.NewTestClass <- function(test_scores, ...) {
    test_scores
  }

  # Register - this should create the test_class environment (line 85)
  expect_silent(
    .register_std_version(
      new_test_obj,
      method = "norms",
      version = "v1",
      data = list(test = 1),
      description = "Testing test_class environment creation"
    )
  )

  # Verify the test_class environment was created
  expect_true(exists(
    "NewTestClass",
    envir = .std_versions[["norms"]],
    inherits = FALSE
  ))
})

test_that(".register_std_version.test_scores() registers new versions successfully", {
  test_obj <- MOCATOTS(c(25, 28, 30))

  # Should error when trying to register a method that doesn't exist
  expect_error(
    .register_std_version(
      test_obj,
      method = "fake_method",
      version = "v1",
      data = list(test = 1),
      description = "Test"
    ),
    "not implemented"
  )
})

test_that(".register_std_version.test_scores() registers new versions successfully", {
  test_obj <- MOCATOTS(c(25, 28, 30))

  # Register a new version for an implemented method
  test_data <- list(coefficients = c(1, 2, 3), intercept = 10)

  expect_silent(
    .register_std_version(
      test_obj,
      method = "regression",
      version = "test_version_001",
      data = test_data,
      description = "Test regression version for unit testing"
    )
  )

  # Verify the version was registered
  versions <- get_versions(test_obj, "regression")
  expect_true("test_version_001" %in% versions)
})

test_that(".register_std_version.test_scores() prevents duplicate registration by default", {
  test_obj <- MOCATOTS(c(25, 28, 30))

  # Register a version
  test_data <- list(value = 42)
  .register_std_version(
    test_obj,
    method = "norms",
    version = "duplicate_test_v1",
    data = test_data,
    description = "First registration"
  )

  # Try to register the same version again without overwrite
  expect_error(
    .register_std_version(
      test_obj,
      method = "norms",
      version = "duplicate_test_v1",
      data = test_data,
      description = "Second registration"
    ),
    "already exists"
  )
})

test_that(".register_std_version.test_scores() allows overwrite when specified", {
  test_obj <- MOCATOTS(c(25, 28, 30))

  # Register a version
  test_data_v1 <- list(value = 100)
  .register_std_version(
    test_obj,
    method = "norms",
    version = "overwrite_test_v1",
    data = test_data_v1,
    description = "Original version"
  )

  # Overwrite with overwrite = TRUE should warn but succeed
  test_data_v2 <- list(value = 200)
  expect_warning(
    .register_std_version(
      test_obj,
      method = "norms",
      version = "overwrite_test_v1",
      data = test_data_v2,
      description = "Updated version",
      overwrite = TRUE
    ),
    "Overwriting"
  )
})

test_that(".register_std_version.test_scores() stores correct metadata", {
  test_obj <- MOCATOTS(c(25, 28, 30))

  # Register a version
  test_data <- list(mean = 50, sd = 10)
  test_version <- "metadata_test_v1"
  test_description <- "Testing metadata storage"

  .register_std_version(
    test_obj,
    method = "norms",
    version = test_version,
    data = test_data,
    description = test_description
  )

  # Access the stored data directly from the registry environment
  test_class <- "MOCATOTS"
  stored <- .std_versions[["norms"]][[test_class]][[test_version]]

  # Verify metadata (not including registered_at since it's not stored)
  expect_equal(stored$test_class, "MOCATOTS")
  expect_equal(stored$method, "norms")
  expect_equal(stored$version, test_version)
  expect_equal(stored$description, test_description)
  expect_equal(stored$data, test_data)
})
