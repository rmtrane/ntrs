# Test file for register_norms_version() / register_norms_version.test_scores()

# Test 1: Delegation to .validate_registration_params() ----

test_that("register_norms_version() rejects invalid version parameter", {
  valid_lt <- data.frame(m = 50, sd = 10)

  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = NULL,
      lookup_table = valid_lt,
      covariate_prep_funs = list()
    ),
    "must be a non-empty character string"
  )

  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = 123,
      lookup_table = valid_lt,
      covariate_prep_funs = list()
    ),
    "must be a non-empty character string"
  )

  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "",
      lookup_table = valid_lt,
      covariate_prep_funs = list()
    ),
    "must be a non-empty character string"
  )

  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = c("v1", "v2"),
      lookup_table = valid_lt,
      covariate_prep_funs = list()
    ),
    "must be a non-empty character string"
  )
})

test_that("register_norms_version() rejects invalid description parameter", {
  valid_lt <- data.frame(m = 50, sd = 10)

  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "v1",
      lookup_table = valid_lt,
      covariate_prep_funs = list(),
      description = NULL
    ),
    "must be a character string"
  )

  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "v1",
      lookup_table = valid_lt,
      covariate_prep_funs = list(),
      description = 123
    ),
    "must be a character string"
  )
})

# Test 2: lookup_table must be a data.frame ----

test_that("register_norms_version() rejects non-data.frame lookup_table", {
  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v",
      lookup_table = list(m = 50, sd = 10),
      covariate_prep_funs = list()
    ),
    "must be a.*data.frame"
  )

  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v",
      lookup_table = matrix(
        c(50, 10),
        ncol = 2,
        dimnames = list(NULL, c("m", "sd"))
      ),
      covariate_prep_funs = list()
    ),
    "must be a.*data.frame"
  )

  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v",
      lookup_table = c(m = 50, sd = 10),
      covariate_prep_funs = list()
    ),
    "must be a.*data.frame"
  )
})

# Test 3: lookup_table must contain required columns m and sd ----

test_that("register_norms_version() rejects lookup_table missing column m", {
  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v",
      lookup_table = data.frame(sd = 10),
      covariate_prep_funs = list()
    ),
    "missing required columns"
  )
})


# Test 4: lookup_table must have unique rows in grouping columns ----

test_that("register_norms_version() rejects duplicate rows in grouping columns", {
  # Duplicate when only grouping col is age
  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v",
      lookup_table = data.frame(
        age = factor(c("60-69", "60-69")),
        m = c(25, 26),
        sd = c(3, 4)
      ),
      covariate_prep_funs = list()
    ),
    "must unique rows"
  )

  # Duplicate across two grouping cols (age + sex)
  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v",
      lookup_table = data.frame(
        age = factor(c("60-69", "60-69")),
        sex = factor(c("Male", "Male")),
        m = c(25, 26),
        sd = c(3, 4)
      ),
      covariate_prep_funs = list()
    ),
    "must unique rows"
  )

  # No error when grouping cols differ
  expect_no_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_dup_ok",
      description = "unique groups",
      lookup_table = data.frame(
        age = factor(c("60-69", "70-79")),
        m = c(25, 26),
        sd = c(3, 4)
      ),
      covariate_prep_funs = list(
        age = function(x) {
          cut(
            x,
            breaks = c(60, 70, 80),
            right = FALSE,
            labels = c("60-69", "70-79")
          )
        }
      )
    )
  )
})

test_that("register_norms_version() rejects lookup_table missing column sd", {
  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v",
      lookup_table = data.frame(m = 50),
      covariate_prep_funs = list()
    ),
    "missing required columns"
  )
})

test_that("register_norms_version() rejects lookup_table missing both m and sd", {
  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v",
      lookup_table = data.frame(age = factor("60-69")),
      covariate_prep_funs = list()
    ),
    "missing required columns"
  )
})

# Test 5: lookup_table must not contain disallowed columns ----

test_that("register_norms_version() rejects lookup_table with disallowed columns", {
  # Single disallowed column
  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v",
      lookup_table = data.frame(m = 50, sd = 10, race = factor("White")),
      covariate_prep_funs = list()
    ),
    "not allowed"
  )

  # Multiple disallowed columns
  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v",
      lookup_table = data.frame(
        m = 50,
        sd = 10,
        race = factor("White"),
        delay = factor("short")
      ),
      covariate_prep_funs = list()
    ),
    "not allowed"
  )

  # Allowed optional columns should not trigger error
  expect_no_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_allowed_cols",
      description = "all optional cols",
      lookup_table = data.frame(
        age = factor("60-69"),
        sex = factor("Male"),
        educ = factor("12-15"),
        n = 100L,
        m = 25,
        sd = 3
      ),
      covariate_prep_funs = list(
        age = function(x) factor("60-69"),
        sex = function(x) factor("Male"),
        educ = function(x) factor("12-15")
      )
    )
  )
})

# Test 6: No NA values in any present allowed column ----

test_that("register_norms_version() rejects NA in m column", {
  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v",
      lookup_table = data.frame(m = NA_real_, sd = 10),
      covariate_prep_funs = list()
    ),
    "contains.*NA.*values"
  )
})

test_that("register_norms_version() rejects NA in sd column", {
  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v",
      lookup_table = data.frame(m = 50, sd = NA_real_),
      covariate_prep_funs = list()
    ),
    "contains.*NA.*values"
  )
})

test_that("register_norms_version() rejects NA in grouping column", {
  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v",
      lookup_table = data.frame(
        age = factor(NA),
        m = 50,
        sd = 10
      ),
      covariate_prep_funs = list()
    ),
    "contains.*NA.*values"
  )
})

# Test 7: m must be numeric ----

test_that("register_norms_version() rejects non-numeric m column", {
  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v",
      lookup_table = data.frame(m = "fifty", sd = 10),
      covariate_prep_funs = list()
    ),
    "must be numeric"
  )

  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v",
      lookup_table = data.frame(m = factor("50"), sd = 10),
      covariate_prep_funs = list()
    ),
    "must be numeric"
  )
})

# Test 8: sd must be numeric ----

test_that("register_norms_version() rejects non-numeric sd column", {
  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v",
      lookup_table = data.frame(m = 50, sd = "ten"),
      covariate_prep_funs = list()
    ),
    "must be numeric"
  )

  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v",
      lookup_table = data.frame(m = 50, sd = factor("10")),
      covariate_prep_funs = list()
    ),
    "must be numeric"
  )
})

# Test 9: sd must be positive ----

test_that("register_norms_version() rejects zero sd", {
  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v",
      lookup_table = data.frame(m = 50, sd = 0),
      covariate_prep_funs = list()
    ),
    "only positive values"
  )
})

test_that("register_norms_version() rejects negative sd", {
  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v",
      lookup_table = data.frame(m = 50, sd = -5),
      covariate_prep_funs = list()
    ),
    "only positive values"
  )
})

test_that("register_norms_version() rejects mixed positive/non-positive sd", {
  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v",
      lookup_table = data.frame(
        age = factor(c("60-69", "70-79")),
        m = c(25, 26),
        sd = c(3, 0)
      ),
      covariate_prep_funs = list()
    ),
    "only positive values"
  )
})

# Test 10: age, sex, educ must be factors ----

test_that("register_norms_version() rejects non-factor age column", {
  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v",
      lookup_table = data.frame(
        age = c("60-69", "70-79"),
        m = c(25, 26),
        sd = c(3, 4)
      ),
      covariate_prep_funs = list()
    ),
    "must be factors"
  )
})

test_that("register_norms_version() rejects non-factor sex column", {
  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v",
      lookup_table = data.frame(
        sex = c(1, 2),
        m = c(25, 26),
        sd = c(3, 4)
      ),
      covariate_prep_funs = list()
    ),
    "must be factor"
  )
})

test_that("register_norms_version() rejects non-factor educ column", {
  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v",
      lookup_table = data.frame(
        educ = c("12-15", "16-20"),
        m = c(25, 26),
        sd = c(3, 4)
      ),
      covariate_prep_funs = list()
    ),
    "must be factor"
  )
})

# Test 11: covariate_prep_funs must be a list ----
test_that("register_norms_version() rejects covariate_prep_funs when no covariate columns are present", {
  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v",
      lookup_table = data.frame(m = 50, sd = 10),
      covariate_prep_funs = list(age = function(x) x)
    ),
    "should not be provided when no covariate columns"
  )
})

test_that("register_norms_version() rejects non-list covariate_prep_funs", {
  valid_lt <- data.frame(age = factor(60), sex = factor(1), m = 50, sd = 10)

  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v1",
      lookup_table = valid_lt,
      covariate_prep_funs = "not a list"
    ),
    "must be a.*list"
  )

  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v2",
      lookup_table = valid_lt,
      covariate_prep_funs = matrix(1:4, ncol = 2)
    ),
    "must be a.*list"
  )

  expect_message(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v3",
      lookup_table = valid_lt,
      covariate_prep_funs = list(age = function(x) factor(60))
    ),
    "Registered.+norms.+version.+test_v3.+for.+MOCATOTS.+"
  )
})

# Test 12: covariate_prep_funs names must be subset of lookup_table columns ----
test_that("register_norms_version() rejects covariate_prep_funs with names not in lookup_table", {
  valid_lt <- data.frame(age = factor(60), sex = factor(1), m = 50, sd = 10)
  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v",
      lookup_table = valid_lt,
      covariate_prep_funs = list(race = function(x) factor("White"))
    ),
    "must have names that are a subset of the columns"
  )

  if (exists("test_v", envir = .std_versions[["norms"]][["MOCATOTS"]])) {
    rm("test_v", envir = .std_versions[["norms"]][["MOCATOTS"]])
  }
})

# Test 13: covariate_prep_funs functions must produce appropriate outputs ----
test_that("register_norms_version() rejects covariate_prep_funs that produce outputs not matching lookup_table values", {
  valid_lt <- data.frame(
    age = factor(c("60-69", "70-79")),
    m = c(25, 26),
    sd = c(3, 4)
  )

  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v",
      lookup_table = valid_lt,
      covariate_prep_funs = list(age = function(x) {
        factor(c("60-69", "70-79", "80-89"))
      })
    ),
    "must be a function taking a single argument,.+and the output of"
  )

  if (exists("test_v", envir = .std_versions[["norms"]][["MOCATOTS"]])) {
    rm("test_v", envir = .std_versions[["norms"]][["MOCATOTS"]])
  }
})

# Test 14: Successful registration with valid inputs ----
test_that("register_norms_version() successfully registers norms version with valid inputs", {
  valid_lt <- data.frame(
    age = factor(c("60-69", "70-79")),
    sex = factor(c("Male", "Female")),
    educ = factor(c("12-15", "16-20")),
    n = c(100L, 150L),
    m = c(25, 26),
    sd = c(3, 4)
  )

  covar_prep_funs <- list(
    age = function(x) {
      cut(
        x,
        breaks = c(60, 70, 80),
        right = FALSE,
        labels = c("60-69", "70-79")
      )
    },
    sex = function(x) factor(ifelse(x == 1, "Male", "Female")),
    educ = function(x) {
      cut(
        x,
        breaks = c(11, 15, 20),
        right = FALSE,
        labels = c("12-15", "16-20")
      )
    }
  )

  expect_no_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_v",
      description = "Test norms version",
      lookup_table = valid_lt,
      covariate_prep_funs = covar_prep_funs
    )
  )

  # Check that the version is registered
  expect_contains(
    get_versions(MOCATOTS(), "norms"),
    "test_v"
  )

  # Check that we can recover lookup table and covariate prep funs
  test_v_data <- get_version_data(MOCATOTS(), "norms", "test_v")

  expect_equal(
    test_v_data,
    list(
      lookup_table = valid_lt,
      covariate_prep_funs = covar_prep_funs
    )
  )

  # Clean up after test
  rm("test_v", envir = .std_versions[["norms"]][["MOCATOTS"]])
})

# Test 15: overwrite parameter is passed through ----

test_that("register_norms_version() errors when re-registering without overwrite = TRUE", {
  valid_lt <- data.frame(m = 50, sd = 10)

  # Register initial version
  register_norms_version(
    test_class = MOCATOTS(),
    version = "test_overwrite",
    lookup_table = valid_lt,
    covariate_prep_funs = list()
  )

  # Attempting to re-register without overwrite should error
  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_overwrite",
      lookup_table = data.frame(m = 55, sd = 12),
      covariate_prep_funs = list()
    ),
    "already exists for.*overwrite =.+TRUE"
  )

  # Clean up
  if (
    exists("test_overwrite", envir = .std_versions[["norms"]][["MOCATOTS"]])
  ) {
    rm("test_overwrite", envir = .std_versions[["norms"]][["MOCATOTS"]])
  }
})

test_that("register_norms_version() warns and succeeds when re-registering with overwrite = TRUE", {
  valid_lt <- data.frame(m = 50, sd = 10)

  # Register initial version
  register_norms_version(
    test_class = MOCATOTS(),
    version = "test_overwrite2",
    lookup_table = valid_lt,
    covariate_prep_funs = list()
  )

  # Re-registering with overwrite = TRUE should warn but succeed
  expect_warning(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_overwrite2",
      lookup_table = data.frame(m = 55, sd = 12),
      covariate_prep_funs = list(),
      overwrite = TRUE
    ),
    "Overwriting existing version"
  )

  # Verify the data was actually updated
  new_data <- get_version_data(MOCATOTS(), "norms", "test_overwrite2")
  expect_equal(new_data$lookup_table$m, 55)
  expect_equal(new_data$lookup_table$sd, 12)

  # Clean up
  rm("test_overwrite2", envir = .std_versions[["norms"]][["MOCATOTS"]])
})

# Test 15: overwrite parameter is passed through ----

test_that("register_norms_version() errors when re-registering without overwrite = TRUE", {
  valid_lt <- data.frame(m = 50, sd = 10)

  # Register initial version
  register_norms_version(
    test_class = MOCATOTS(),
    version = "test_overwrite",
    lookup_table = valid_lt,
    covariate_prep_funs = list()
  )

  # Attempting to re-register without overwrite should error
  expect_error(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_overwrite",
      lookup_table = data.frame(m = 55, sd = 12),
      covariate_prep_funs = list()
    ),
    "already exists.+overwrite.+=.+TRUE"
  )

  # Clean up
  rm("test_overwrite", envir = .std_versions[["norms"]][["MOCATOTS"]])
})

test_that("register_norms_version() warns and succeeds when re-registering with overwrite = TRUE", {
  valid_lt <- data.frame(m = 50, sd = 10)

  # Register initial version
  register_norms_version(
    test_class = MOCATOTS(),
    version = "test_overwrite2",
    lookup_table = valid_lt,
    covariate_prep_funs = list()
  )

  # Re-registering with overwrite = TRUE should warn but succeed
  expect_warning(
    register_norms_version(
      test_class = MOCATOTS(),
      version = "test_overwrite2",
      lookup_table = data.frame(m = 55, sd = 12),
      covariate_prep_funs = list(),
      overwrite = TRUE
    ),
    "Overwriting existing version"
  )

  # Verify the data was actually updated
  new_data <- get_version_data(MOCATOTS(), "norms", "test_overwrite2")
  expect_equal(new_data$lookup_table$m, 55)
  expect_equal(new_data$lookup_table$sd, 12)

  # Clean up
  rm("test_overwrite2", envir = .std_versions[["norms"]][["MOCATOTS"]])
})
