# Tests for std_npsych_scores S7 class
#
# These tests define the expected behavior of the std_npsych_scores class
# BEFORE implementation. They cover:
# 1. Class construction and property access
# 2. Numeric vector behavior (inherits class_double)
# 3. Survival in data.frame and data.table columns
# 4. Survival through common operations (rbind, cbind, merge, subset, etc.)

# ---------------------------------------------------------------------------
# Construction and properties
# ---------------------------------------------------------------------------

test_that("std_npsych_scores can be constructed with method, version, description", {
  x <- std_npsych_scores(
    c(0.5, -1.2, 0.8),
    method = "norms",
    version = "nacc",
    description = "Standardized using norms grouped by age and sex"
  )

  expect_true(S7::S7_inherits(x, std_npsych_scores))
})

test_that("std_npsych_scores properties are accessible via @", {
  x <- std_npsych_scores(
    c(0.5, -1.2),
    method = "regression",
    version = "updated_2025.06",
    description = "Standardized using regression adjusting for age, sex, education"
  )

  expect_equal(x@method, "regression")
  expect_equal(x@version, "updated_2025.06")
  expect_equal(
    x@description,
    "Standardized using regression adjusting for age, sex, education"
  )
})

test_that("std_npsych_scores stores the numeric z-scores as underlying data", {
  vals <- c(0.5, -1.2, 0.8)
  x <- std_npsych_scores(
    vals,
    method = "norms",
    version = "nacc",
    description = "Norms"
  )

  expect_equal(as.numeric(x), vals)
  expect_type(S7::S7_data(x), "double")
})

test_that("std_npsych_scores has correct length", {
  x <- std_npsych_scores(
    c(0.5, -1.2, 0.8, NA),
    method = "norms",
    version = "nacc",
    description = "Norms"
  )

  expect_length(x, 4)
})

# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------

test_that("std_npsych_scores requires method to be a non-empty string", {
  testthat::local_reproducible_output()

  expect_error(
    std_npsych_scores(c(0.5), method = "", version = "v1", description = "d")
  )

  expect_error(
    std_npsych_scores(
      c(0.5),
      method = character(),
      version = "v1",
      description = "d"
    ),
    ".+method.+ must be a non-empty string"
  )
})

test_that("std_npsych_scores requires version to be a string", {
  testthat::local_reproducible_output()

  expect_error(
    std_npsych_scores(
      c(0.5),
      method = "norms",
      version = 42,
      description = "d"
    ),
    "version must be .+character.+"
  )
})

test_that("std_npsych_scores requires description to be a string", {
  expect_error(
    std_npsych_scores(
      c(0.5),
      method = "norms",
      version = "v1",
      description = 42
    ),
    "description must be .+character.+"
  )
})

test_that("std_npsych_scores rejects empty description", {
  expect_error(
    std_npsych_scores(
      c(0.5),
      method = "norms",
      version = "v1",
      description = ""
    ),
    "description.+must be a non-empty string"
  )
})

test_that("std_npsych_scores rejects empty version string", {
  expect_error(
    std_npsych_scores(
      c(0.5),
      method = "norms",
      version = "",
      description = "d"
    ),
    "version.+must be a non-empty string"
  )
})

test_that("std_npsych_scores allows NULL version", {
  x <- std_npsych_scores(
    c(0.5),
    method = "norms",
    version = NULL,
    description = "Norms"
  )

  expect_null(x@version)
})

test_that("std_npsych_scores rejects invalid scores_subclass", {
  expect_error(
    std_npsych_scores(
      c(0.5),
      scores_subclass = "NOT_A_REAL_SUBCLASS",
      method = "norms",
      version = "v1",
      description = "d"
    ),
    "scores_subclass.+must be the name"
  )
})

test_that("std_npsych_scores rejects multi-element scores_subclass", {
  expect_error(
    std_npsych_scores(
      c(0.5),
      scores_subclass = c("MOCATOTS", "TRAILA"),
      method = "norms",
      version = "v1",
      description = "d"
    ),
    "scores_subclass.+must be a string"
  )
})

test_that("std_npsych_scores allows NULL scores_subclass", {
  x <- std_npsych_scores(
    c(0.5),
    scores_subclass = NULL,
    method = "norms",
    version = "v1",
    description = "Norms"
  )

  expect_null(x@scores_subclass)
})

test_that("std_npsych_scores accepts valid scores_subclass", {
  x <- std_npsych_scores(
    c(0.5),
    scores_subclass = "MOCATOTS",
    method = "norms",
    version = "nacc",
    description = "Norms"
  )

  expect_equal(x@scores_subclass, "MOCATOTS")
})

test_that("std_npsych_scores allows NA values in the numeric data", {
  x <- std_npsych_scores(
    c(0.5, NA, -1.2),
    method = "norms",
    version = "nacc",
    description = "Norms"
  )

  expect_length(x, 3)
  expect_true(is.na(x[2]))
})

# ---------------------------------------------------------------------------
# Numeric vector behavior
# ---------------------------------------------------------------------------

test_that("std_npsych_scores works with arithmetic operations", {
  x <- std_npsych_scores(
    c(1.0, 2.0),
    method = "norms",
    version = "nacc",
    description = "Norms"
  )

  # Arithmetic should produce plain numeric (not std_npsych_scores)
  expect_equal(as.numeric(x + 1), c(2.0, 3.0))
  expect_equal(as.numeric(x * 2), c(2.0, 4.0))
})

test_that("std_npsych_scores works with mean, sd, and summary functions", {
  x <- std_npsych_scores(
    c(0.5, -1.0, 1.5),
    method = "norms",
    version = "nacc",
    description = "Norms"
  )

  expect_equal(mean(x), mean(c(0.5, -1.0, 1.5)))
  expect_equal(sd(x), sd(c(0.5, -1.0, 1.5)))
})

# ---------------------------------------------------------------------------
# Subsetting preserves class
# ---------------------------------------------------------------------------

test_that("subsetting std_npsych_scores with [ preserves class and properties", {
  x <- std_npsych_scores(
    c(0.5, -1.2, 0.8),
    method = "norms",
    version = "nacc",
    description = "Norms"
  )

  subset <- x[1:2]

  expect_true(S7::S7_inherits(subset, std_npsych_scores))
  expect_equal(subset@method, "norms")
  expect_equal(subset@version, "nacc")
  expect_equal(subset@description, "Norms")
  expect_length(subset, 2)
})

# ---------------------------------------------------------------------------
# data.frame column survival
# ---------------------------------------------------------------------------

test_that("std_npsych_scores survives as a data.frame column", {
  x <- std_npsych_scores(
    c(0.5, -1.2, 0.8),
    method = "norms",
    version = "nacc",
    description = "Norms"
  )

  df <- data.frame(id = 1:3)
  df$z_moca <- x

  expect_true(S7::S7_inherits(df$z_moca, std_npsych_scores))
  expect_equal(df$z_moca@method, "norms")
})

test_that("std_npsych_scores survives as a data.table column", {
  x <- std_npsych_scores(
    c(0.5, -1.2, 0.8),
    method = "norms",
    version = "nacc",
    description = "Norms"
  )

  dt <- data.table::data.table(id = 1:3)
  dt[, z_moca := x]

  expect_true(S7::S7_inherits(dt$z_moca, std_npsych_scores))
  expect_equal(dt$z_moca@method, "norms")
})

test_that("std_npsych_scores properties survive data.table := assignment", {
  dt <- data.table::data.table(id = 1:3)

  dt[,
    z_moca := std_npsych_scores(
      c(0.5, -1.2, 0.8),
      method = "regression",
      version = "updated_2025.06",
      description = "Regression model"
    )
  ]

  expect_true(S7::S7_inherits(dt$z_moca, std_npsych_scores))
  expect_equal(dt$z_moca@version, "updated_2025.06")
})

# ---------------------------------------------------------------------------
# Survival through rbind, cbind, merge
# ---------------------------------------------------------------------------

test_that("std_npsych_scores survives rbind of data.frames", {
  make_df <- function(ids, vals) {
    df <- data.frame(id = ids)
    df$z_moca <- std_npsych_scores(
      vals,
      method = "norms",
      version = "nacc",
      description = "Norms"
    )
    df
  }

  df1 <- make_df(1:2, c(0.5, -1.2))
  df2 <- make_df(3:4, c(0.8, -0.3))

  combined <- rbind(df1, df2)

  expect_true(S7::S7_inherits(combined$z_moca, std_npsych_scores))
  expect_length(combined$z_moca, 4)

  expect_equal(
    combined$z_moca[0],
    df1$z_moca[0]
  )
  expect_equal(
    combined$z_moca[0],
    df2$z_moca[0]
  )
})

test_that("std_npsych_scores survives data.table rbindlist", {
  make_dt <- function(ids, vals) {
    dt <- data.table::data.table(id = ids)
    dt[,
      z_moca := std_npsych_scores(
        vals,
        method = "norms",
        version = "nacc",
        description = "Norms"
      )
    ]
    dt
  }

  dt1 <- make_dt(1:2, c(0.5, -1.2))
  dt2 <- make_dt(3:4, c(0.8, -0.3))

  combined <- data.table::rbindlist(list(dt1, dt2))

  expect_true(S7::S7_inherits(combined$z_moca, std_npsych_scores))
  expect_equal(combined$z_moca@method, "norms")
})

test_that("std_npsych_scores survives merge of data.frames", {
  df1 <- data.frame(id = 1:3)
  df1$z_moca <- std_npsych_scores(
    c(0.5, -1.2, 0.8),
    method = "norms",
    version = "nacc",
    description = "Norms"
  )

  df2 <- data.frame(id = 1:3, group = c("a", "b", "a"))

  merged <- merge(df1, df2, by = "id")

  expect_true(S7::S7_inherits(merged$z_moca, std_npsych_scores))
  expect_equal(merged$z_moca@method, "norms")
})

test_that("std_npsych_scores survives cbind-style column addition", {
  df <- data.frame(id = 1:3)
  df$z_moca <- std_npsych_scores(
    c(0.5, -1.2, 0.8),
    method = "norms",
    version = "nacc",
    description = "Norms"
  )

  df$group <- c("a", "b", "a")

  expect_true(S7::S7_inherits(df$z_moca, std_npsych_scores))
})

# ---------------------------------------------------------------------------
# Survival through row subsetting
# ---------------------------------------------------------------------------

test_that("std_npsych_scores survives data.frame row subsetting", {
  df <- data.frame(id = 1:4)
  df$z_moca <- std_npsych_scores(
    c(0.5, -1.2, 0.8, -0.3),
    method = "norms",
    version = "nacc",
    description = "Norms"
  )

  subset_df <- df[1:2, ]

  expect_true(S7::S7_inherits(subset_df$z_moca, std_npsych_scores))
  expect_equal(subset_df$z_moca@method, "norms")
  expect_length(subset_df$z_moca, 2)
})

test_that("std_npsych_scores survives data.table row subsetting", {
  dt <- data.table::data.table(id = 1:4)
  dt[,
    z_moca := std_npsych_scores(
      c(0.5, -1.2, 0.8, -0.3),
      method = "norms",
      version = "nacc",
      description = "Norms"
    )
  ]

  subset_dt <- dt[1:2]

  expect_true(S7::S7_inherits(subset_dt$z_moca, std_npsych_scores))
  expect_equal(subset_dt$z_moca@method, "norms")
})

# ---------------------------------------------------------------------------
# Survival through dplyr-style operations (if dplyr is available)
# ---------------------------------------------------------------------------

test_that("std_npsych_scores survives dplyr::filter", {
  skip_if_not_installed("dplyr")

  df <- data.frame(id = 1:3, group = c("a", "b", "a"))
  df$z_moca <- std_npsych_scores(
    c(0.5, -1.2, 0.8),
    method = "norms",
    version = "nacc",
    description = "Norms"
  )

  result <- dplyr::filter(df, group == "a")

  expect_true(S7::S7_inherits(result$z_moca, std_npsych_scores))
  expect_equal(result$z_moca@method, "norms")
})

test_that("std_npsych_scores survives dplyr::select", {
  skip_if_not_installed("dplyr")

  df <- data.frame(id = 1:3)
  df$z_moca <- std_npsych_scores(
    c(0.5, -1.2, 0.8),
    method = "norms",
    version = "nacc",
    description = "Norms"
  )

  result <- dplyr::select(df, z_moca)

  expect_true(S7::S7_inherits(result$z_moca, std_npsych_scores))
})

test_that("std_npsych_scores survives dplyr::mutate (adding other columns)", {
  skip_if_not_installed("dplyr")

  df <- data.frame(id = 1:3)
  df$z_moca <- std_npsych_scores(
    c(0.5, -1.2, 0.8),
    method = "norms",
    version = "nacc",
    description = "Norms"
  )

  result <- dplyr::mutate(df, doubled_id = id * 2)

  expect_true(S7::S7_inherits(result$z_moca, std_npsych_scores))
})

# ---------------------------------------------------------------------------
# Integration: std() returns std_npsych_scores
# ---------------------------------------------------------------------------

test_that("std() returns an std_npsych_scores object", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
  )

  result <- std(MOCATOTS(c(25, 28)), age = 72, sex = 1, educ = 16)

  expect_true(S7::S7_inherits(result, std_npsych_scores))
})

test_that("std() result has correct method and version properties", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
  )

  result <- std(MOCATOTS(c(25, 28)), age = 72, sex = 1, educ = 16)

  expect_equal(result@method, "norms")
  expect_equal(result@version, "nacc")
})

test_that("std() result has a non-empty description", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
  )

  result <- std(MOCATOTS(c(25, 28)), age = 72, sex = 1, educ = 16)

  expect_true(nzchar(result@description))
})

test_that("std_using_norms returns std_npsych_scores (not decorated by std())", {
  result <- std_using_norms(
    MOCATOTS(c(25, 28)),
    age = 72,
    sex = 1,
    educ = 16,
    version = "nacc"
  )

  expect_true(S7::S7_inherits(result, std_npsych_scores))
  expect_equal(result@method, "norms")
})

test_that("std_using_regression returns std_npsych_scores", {
  result <- std_using_regression(
    MOCATOTS(c(25, 28)),
    age = 72,
    sex = 1,
    educ = 16,
    race = 1,
    version = "updated_2025.06"
  )

  expect_true(S7::S7_inherits(result, std_npsych_scores))
  expect_equal(result@method, "regression")
})

# ---------------------------------------------------------------------------
# Integration: std_data() columns are std_npsych_scores
# ---------------------------------------------------------------------------

test_that("std_data() z_ columns are std_npsych_scores objects", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
  )

  df <- data.frame(age = c(72, 75), sex = c(1, 2), educ = c(16, 12))
  df$moca <- MOCATOTS(c(25, 28))

  result <- std_data(df, age = df$age, sex = df$sex, educ = df$educ)

  expect_true(S7::S7_inherits(result$z_moca, std_npsych_scores))
  expect_equal(result$z_moca@method, "norms")
  expect_equal(result$z_moca@version, "nacc")
})

test_that("std_data() z_ columns have non-empty descriptions", {
  local_restore_default("MOCATOTS")

  suppressMessages(
    set_std_defaults(MOCATOTS(), method = "norms", version = "nacc")
  )

  df <- data.frame(age = c(72, 75), sex = c(1, 2), educ = c(16, 12))
  df$moca <- MOCATOTS(c(25, 28))

  result <- std_data(df, age = df$age, sex = df$sex, educ = df$educ)

  expect_true(nzchar(result$z_moca@description))
})

# ---------------------------------------------------------------------------
# Subset-assignment with [<-
# ---------------------------------------------------------------------------

test_that("[<- modifies values in std_npsych_scores and preserves class", {
  x <- std_npsych_scores(
    c(0.5, -1.2, 0.8),
    method = "norms",
    version = "nacc",
    description = "Norms"
  )

  x[2] <- 0.0

  expect_true(S7::S7_inherits(x, std_npsych_scores))
  expect_equal(as.numeric(x), c(0.5, 0.0, 0.8))
  expect_equal(x@method, "norms")
})

test_that("[<- can assign NA in std_npsych_scores", {
  x <- std_npsych_scores(
    c(0.5, -1.2, 0.8),
    method = "norms",
    version = "nacc",
    description = "Norms"
  )

  x[1] <- NA

  expect_true(S7::S7_inherits(x, std_npsych_scores))
  expect_true(is.na(as.numeric(x)[1]))
})

# ---------------------------------------------------------------------------
# Concatenation with c()
# ---------------------------------------------------------------------------

test_that("c() combines std_npsych_scores with matching metadata", {
  x <- std_npsych_scores(
    c(0.5, -1.2),
    method = "norms",
    version = "nacc",
    description = "Norms"
  )

  y <- std_npsych_scores(
    c(0.8, -0.3),
    method = "norms",
    version = "nacc",
    description = "Norms"
  )

  combined <- c(x, y)

  expect_true(S7::S7_inherits(combined, std_npsych_scores))
  expect_equal(as.numeric(combined), c(0.5, -1.2, 0.8, -0.3))
  expect_equal(combined@method, "norms")
  expect_equal(combined@version, "nacc")
})

test_that("c() errors when combining std_npsych_scores with different methods", {
  x <- std_npsych_scores(
    c(0.5),
    method = "norms",
    version = "nacc",
    description = "Norms"
  )

  y <- std_npsych_scores(
    c(0.8),
    method = "regression",
    version = "nacc",
    description = "Regression"
  )

  expect_error(c(x, y), "Cannot combine")
})

test_that("c() errors when combining std_npsych_scores with different versions", {
  x <- std_npsych_scores(
    c(0.5),
    method = "norms",
    version = "nacc",
    description = "Norms"
  )

  y <- std_npsych_scores(
    c(0.8),
    method = "norms",
    version = "updated",
    description = "Norms"
  )

  expect_error(c(x, y), "Cannot combine")
})

# ---------------------------------------------------------------------------
# Print / format
# ---------------------------------------------------------------------------

test_that("std_npsych_scores has an informative print representation", {
  x <- std_npsych_scores(
    c(0.5, -1.2, 0.8),
    method = "norms",
    version = "nacc",
    description = "Standardized using norms grouped by age and sex"
  )

  output <- capture.output(print(x))
  output_str <- paste(output, collapse = "\n")

  # Should mention the method somewhere in the output
  expect_true(grepl("norms", output_str, ignore.case = TRUE))
})
