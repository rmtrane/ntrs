# ---------------------------------------------------------------------------
# Happy path
# ---------------------------------------------------------------------------

test_that("std_version creates a valid object with all required fields", {
  v <- std_version(
    scores_class = "MOCATOTS",
    method_name = "norms",
    version_id = "v1",
    description = "A test version"
  )

  expect_true(S7::S7_inherits(v, std_version))
  expect_equal(v@scores_class, "MOCATOTS")
  expect_equal(v@method_name, "norms")
  expect_equal(v@version_id, "v1")
  expect_equal(v@description, "A test version")
})

test_that("std_version allows empty string description", {
  v <- std_version(
    scores_class = "MOCATOTS",
    method_name = "norms",
    version_id = "v1",
    description = ""
  )

  expect_true(S7::S7_inherits(v, std_version))
  expect_equal(v@description, "")
})

test_that("std_version allows zero-length description", {
  v <- std_version(
    scores_class = "MOCATOTS",
    method_name = "norms",
    version_id = "v1",
    description = character(0)
  )

  expect_true(S7::S7_inherits(v, std_version))
  expect_length(v@description, 0)
})

# ---------------------------------------------------------------------------
# Validator: scores_class
# ---------------------------------------------------------------------------

test_that("std_version rejects empty string scores_class", {
  expect_error(
    std_version(
      scores_class = "",
      method_name = "norms",
      version_id = "v1",
      description = ""
    ),
    "scores_class must be a non-empty single string"
  )
})

test_that("std_version rejects multi-element scores_class", {
  expect_error(
    std_version(
      scores_class = c("A", "B"),
      method_name = "norms",
      version_id = "v1",
      description = ""
    ),
    "scores_class must be a non-empty single string"
  )
})

test_that("std_version rejects zero-length scores_class", {
  expect_error(
    std_version(
      scores_class = character(0),
      method_name = "norms",
      version_id = "v1",
      description = ""
    ),
    "scores_class must be a non-empty single string"
  )
})

# ---------------------------------------------------------------------------
# Validator: method_name
# ---------------------------------------------------------------------------

test_that("std_version rejects empty string method_name", {
  expect_error(
    std_version(
      scores_class = "MOCATOTS",
      method_name = "",
      version_id = "v1",
      description = ""
    ),
    "method_name must be a non-empty single string"
  )
})

test_that("std_version rejects multi-element method_name", {
  expect_error(
    std_version(
      scores_class = "MOCATOTS",
      method_name = c("norms", "regression"),
      version_id = "v1",
      description = ""
    ),
    "method_name must be a non-empty single string"
  )
})

test_that("std_version rejects zero-length method_name", {
  expect_error(
    std_version(
      scores_class = "MOCATOTS",
      method_name = character(0),
      version_id = "v1",
      description = ""
    ),
    "method_name must be a non-empty single string"
  )
})

# ---------------------------------------------------------------------------
# Validator: version_id
# ---------------------------------------------------------------------------

test_that("std_version rejects empty string version_id", {
  expect_error(
    std_version(
      scores_class = "MOCATOTS",
      method_name = "norms",
      version_id = "",
      description = ""
    ),
    "version_id must be a non-empty single string"
  )
})

test_that("std_version rejects multi-element version_id", {
  expect_error(
    std_version(
      scores_class = "MOCATOTS",
      method_name = "norms",
      version_id = c("v1", "v2"),
      description = ""
    ),
    "version_id must be a non-empty single string"
  )
})

test_that("std_version rejects zero-length version_id", {
  expect_error(
    std_version(
      scores_class = "MOCATOTS",
      method_name = "norms",
      version_id = character(0),
      description = ""
    ),
    "version_id must be a non-empty single string"
  )
})

# ---------------------------------------------------------------------------
# Validator: description
# ---------------------------------------------------------------------------

test_that("std_version rejects multi-element description", {
  expect_error(
    std_version(
      scores_class = "MOCATOTS",
      method_name = "norms",
      version_id = "v1",
      description = c("desc1", "desc2")
    ),
    "description must empty or a single string"
  )
})

# ---------------------------------------------------------------------------
# Validator: multiple errors at once
# ---------------------------------------------------------------------------

test_that("std_version reports multiple validation errors", {
  expect_error(
    std_version(
      scores_class = "",
      method_name = "",
      version_id = "",
      description = c("a", "b")
    ),
    "scores_class must be a non-empty single string"
  )
})
