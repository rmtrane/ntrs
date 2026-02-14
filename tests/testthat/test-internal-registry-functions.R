# Tests for internal registry functions

test_that("get_test_score_classes works correctly", {
  # Test with empty registry
  classes <- get_test_score_classes()
  expect_type(classes, "character")
  expect_true(length(classes) >= 0) # Could be empty or have existing classes

  # Test that the result is a character vector (even if empty)
  expect_vector(classes, ptype = character())
})

test_that(".validate_registration_params validates inputs correctly", {
  # Valid inputs should not error
  expect_silent(.validate_registration_params(
    "MOCATOTS",
    "v1.0",
    "test description"
  ))
  expect_silent(.validate_registration_params("MINTTOTS", "baseline", ""))

  # Invalid test_class
  expect_error(
    .validate_registration_params(123, "v1.0", "desc"),
    "test_class.*must be a non-empty character string"
  )
  expect_error(
    .validate_registration_params(c("A", "B"), "v1.0", "desc"),
    "test_class.*must be a non-empty character string"
  )
  expect_error(
    .validate_registration_params("", "v1.0", "desc"),
    "test_class.*must be a non-empty character string"
  )
  expect_error(
    .validate_registration_params(NULL, "v1.0", "desc"),
    "test_class.*must be a non-empty character string"
  )

  # Invalid version
  expect_error(
    .validate_registration_params("MOCATOTS", 123, "desc"),
    "version.*must be a non-empty character string"
  )
  expect_error(
    .validate_registration_params("MOCATOTS", c("v1", "v2"), "desc"),
    "version.*must be a non-empty character string"
  )
  expect_error(
    .validate_registration_params("MOCATOTS", "", "desc"),
    "version.*must be a non-empty character string"
  )
  expect_error(
    .validate_registration_params("MOCATOTS", NULL, "desc"),
    "version.*must be a non-empty character string"
  )

  # Invalid description
  expect_error(
    .validate_registration_params("MOCATOTS", "v1.0", 123),
    "description.*must be a character string"
  )
  expect_error(
    .validate_registration_params("MOCATOTS", "v1.0", c("a", "b")),
    "description.*must be a character string"
  )
  expect_error(
    .validate_registration_params("MOCATOTS", "v1.0", NULL),
    "description.*must be a character string"
  )
})

test_that(".method_implemented checks method availability correctly", {
  # Test with methods that should exist based on the std_using_* files
  expect_type(.method_implemented("test_scores", "norms"), "logical")
  expect_type(.method_implemented("test_scores", "regression"), "logical")

  # Test with a method that definitely doesn't exist
  expect_false(.method_implemented("nonexistent_class", "nonexistent_method"))

  # Test with test_scores class (generic fallback)
  expect_type(.method_implemented("test_scores", "norms"), "logical")
  expect_type(.method_implemented("test_scores", "regression"), "logical")

  # Test that it handles specific classes that may not have their own methods
  # but can fall back to test_scores methods
  result <- .method_implemented("MOCATOTS", "norms")
  expect_type(result, "logical")

  result2 <- .method_implemented("MOCATOTS", "regression")
  expect_type(result2, "logical")
})

test_that(".register_std_version works with valid inputs", {
  # Test data for registration
  test_data <- list(mean = 100, sd = 15, n = 1000)

  # This test assumes that "norms" is an implemented method
  # We'll use a unique version name to avoid conflicts
  test_version <- paste0("test_version_", as.numeric(Sys.time()))

  # Test successful registration (assuming norms method exists for test_scores)
  expect_silent({
    .register_std_version(
      test_class = "test_scores",
      method = "norms",
      version = test_version,
      data = test_data,
      description = "Test version for unit testing"
    )
  })

  # Verify the registration worked by checking the internal structure
  expect_true(exists("norms", envir = .std_versions, inherits = FALSE))
  expect_true(exists(
    "test_scores",
    envir = .std_versions[["norms"]],
    inherits = FALSE
  ))
  expect_true(exists(
    test_version,
    envir = .std_versions[["norms"]][["test_scores"]],
    inherits = FALSE
  ))

  # Check the stored data structure
  stored_data <- .std_versions[["norms"]][["test_scores"]][[test_version]]
  expect_equal(stored_data$test_class, "test_scores")
  expect_equal(stored_data$method, "norms")
  expect_equal(stored_data$version, test_version)
  expect_equal(stored_data$data, test_data)
  expect_equal(stored_data$description, "Test version for unit testing")
  expect_true("registered_at" %in% names(stored_data))
  expect_s3_class(stored_data$registered_at, "POSIXct")
})

test_that(".register_std_version handles duplicate versions correctly", {
  test_data <- list(mean = 100, sd = 15)
  dup_version <- paste0("dup_test_", as.numeric(Sys.time()))

  # First registration should succeed
  expect_silent({
    .register_std_version(
      test_class = "test_scores",
      method = "norms",
      version = dup_version,
      data = test_data,
      description = "First registration"
    )
  })

  # Second registration without overwrite should fail
  expect_error(
    {
      .register_std_version(
        test_class = "test_scores",
        method = "norms",
        version = dup_version,
        data = test_data,
        description = "Second registration"
      )
    },
    "Version.*already exists"
  )

  # Registration with overwrite=TRUE should succeed but warn
  expect_warning(
    {
      .register_std_version(
        test_class = "test_scores",
        method = "norms",
        version = dup_version,
        data = list(mean = 200, sd = 20),
        description = "Overwritten registration",
        overwrite = TRUE
      )
    },
    "Overwriting existing version"
  )

  # Verify the data was actually overwritten
  stored_data <- .std_versions[["norms"]][["test_scores"]][[dup_version]]
  expect_equal(stored_data$data$mean, 200)
  expect_equal(stored_data$description, "Overwritten registration")
})

test_that(".register_std_version fails with non-implemented methods", {
  test_data <- list(test = "data")
  fake_version <- paste0("fake_", as.numeric(Sys.time()))

  # Should fail when method doesn't exist for the class
  expect_error(
    {
      .register_std_version(
        test_class = "nonexistent_class",
        method = "nonexistent_method",
        version = fake_version,
        data = test_data
      )
    },
    "Method.*not implemented"
  )
})

test_that("Registry environments maintain isolation and structure", {
  # Test that environments are properly structured
  expect_true(is.environment(.std_versions))
  expect_true(is.environment(.test_score_classes))

  # Test that the environments are isolated (parent is emptyenv())
  expect_identical(parent.env(.std_versions), emptyenv())
  expect_identical(parent.env(.test_score_classes), emptyenv())

  # Test registration creates proper nested structure
  unique_version <- paste0("structure_test_", as.numeric(Sys.time()))
  .register_std_version(
    test_class = "test_scores",
    method = "norms",
    version = unique_version,
    data = list(test = TRUE)
  )

  # Verify nested environment structure
  expect_true(is.environment(.std_versions[["norms"]]))
  expect_true(is.environment(.std_versions[["norms"]][["test_scores"]]))
  expect_type(
    .std_versions[["norms"]][["test_scores"]][[unique_version]],
    "list"
  )
})

test_that("Edge cases and boundary conditions", {
  # Test with minimal valid description (empty string)
  edge_version <- paste0("edge_test_", as.numeric(Sys.time()))
  expect_silent({
    .register_std_version(
      test_class = "test_scores",
      method = "norms",
      version = edge_version,
      data = NULL, # NULL data should be allowed
      description = ""
    )
  })

  # Verify NULL data was stored
  stored_data <- .std_versions[["norms"]][["test_scores"]][[edge_version]]
  expect_null(stored_data$data)
  expect_equal(stored_data$description, "")

  # Test with complex nested data structures
  complex_version <- paste0("complex_test_", as.numeric(Sys.time()))
  complex_data <- list(
    params = list(a = 1, b = 2),
    matrix_data = matrix(1:6, nrow = 2),
    nested_list = list(inner = list(value = "test"))
  )

  expect_silent({
    .register_std_version(
      test_class = "test_scores",
      method = "norms",
      version = complex_version,
      data = complex_data,
      description = "Complex data test"
    )
  })

  # Verify complex data was stored correctly
  stored_complex <- .std_versions[["norms"]][["test_scores"]][[complex_version]]
  expect_equal(stored_complex$data$params$a, 1)
  expect_equal(stored_complex$data$nested_list$inner$value, "test")
  expect_equal(dim(stored_complex$data$matrix_data), c(2, 3))
})

test_that("Specify new method", {
  std_using_new <- function(test_scores, ...) {
    UseMethod("std_using_new")
  }

  std_using_new.test_scores <- function(test_scores) {
    print("TESTING!!")
  }

  expect_true(
    .method_implemented(
      "MOCATOTS",
      "new"
    )
  )

  version_data <- list(
    "tests" = "version data",
    "data" = mtcars
  )

  expect_silent(
    .register_std_version(
      test_class = "test_scores",
      method = "new",
      version = "new_version",
      data = version_data
    )
  )

  expect_equal(
    get_version_data("test_scores", method = "new", version = "new_version"),
    version_data
  )

  # print(get_std_methods(test_class = "MOCATOTS"))
})
