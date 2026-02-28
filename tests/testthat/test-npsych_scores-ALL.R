# Parameterized tests for all npsych_scores subclasses.
# Specs are defined in helper-npsych-scores-specs.R.

for (spec in npsych_scores_specs) {
  nm <- spec$name
  factory <- match.fun(nm)

  # =========================================================================
  # Constructor tests
  # =========================================================================

  test_that(
    paste0(nm, " returns an object inheriting both ", nm, " and npsych_scores"),
    {
      result <- factory()
      S7::check_is_S7(result)
      expect_equal(S7::S7_class(result)@name, nm)
    }
  )

  test_that(
    paste0(nm, " sets class with ", nm, " first, npsych_scores second"),
    {
      result <- factory()
      expect_equal(
        class(result),
        c(
          paste("ntrs", c(nm, "npsych_scores"), sep = "::"),
          "double",
          "S7_object"
        )
      )
    }
  )

  test_that(paste0(nm, " returns an object with the correct values"), {
    vals <- c(spec$range[1], spec$range[1], spec$range[2])
    result <- factory(vals)
    expect_equal(as.numeric(result), vals)
  })

  test_that(paste0(nm, " sets label to '", spec$label, "'"), {
    result <- factory()
    expect_equal(attr(result, "label"), spec$label)
  })

  test_that(
    paste0(nm, " sets range to c(", spec$range[1], ", ", spec$range[2], ")"),
    {
      result <- factory()
      expect_equal(attr(result, "range"), spec$range)
    }
  )

  if (length(spec$codes) > 0) {
    test_that(paste0(nm, " sets codes with the correct values"), {
      result <- factory()
      actual_codes <- attr(result, "codes")
      for (code in spec$codes) {
        expect_true(code %in% actual_codes, info = paste("code", code))
      }
    })

    test_that(paste0(nm, " codes are named"), {
      result <- factory()
      expect_false(is.null(names(attr(result, "codes"))))
    })

    test_that(paste0(nm, " accepts valid error codes"), {
      for (code in spec$codes) {
        expect_no_error(factory(c(spec$range[1], code)))
      }
    })
  }

  test_that(paste0(nm, " accepts scores at the range boundaries"), {
    expect_no_error(factory(c(spec$range[1], spec$range[2])))
  })

  test_that(paste0(nm, " errors when scores are out of range and not a code"), {
    testthat::local_reproducible_output()
    expect_error(
      factory(c(spec$range[1], spec$out_of_range)),
      regexp = "scores"
    )
  })

  test_that(paste0(nm, " errors when scores is not numeric"), {
    testthat::local_reproducible_output()
    expect_error(factory(c("a", "b")), regexp = "scores")
  })

  test_that(paste0(nm, " with no arguments returns an empty ", nm, " object"), {
    result <- factory()
    S7::check_is_S7(result)
    expect_equal(S7::S7_class(result)@name, nm)
    expect_equal(length(result), 0L)
  })

  # =========================================================================
  # Version registration tests
  # =========================================================================

  # Clean registry for this class, then re-run setup
  lapply(
    c(.std_versions[["norms"]], .std_versions[["regression"]], .std_defaults),
    \(x) {
      suppressWarnings(rm(list = nm, envir = x))
    }
  )

  setup_fn_name <- paste0(".setup_", nm, "_versions")
  suppressMessages(do.call(setup_fn_name, list()))

  # Expected methods
  has_norms <- !is.null(spec$norms_versions)
  has_regression <- !is.null(spec$regression_versions)

  test_that(paste0(".setup_", nm, "_versions registers the expected methods"), {
    methods <- list_std_methods(factory())
    if (has_norms) {
      expect_true("norms" %in% methods)
    }
    if (has_regression) expect_true("regression" %in% methods)
  })

  # --- Norms version tests ---
  if (has_norms) {
    for (ver in spec$norms_versions) {
      test_that(
        paste0(
          ".setup_",
          nm,
          "_versions registers the '",
          ver,
          "' norms version"
        ),
        {
          expect_true(ver %in% list_method_versions(factory(), "norms"))
        }
      )

      test_that(
        paste0(
          nm,
          " '",
          ver,
          "' norms version data contains lookup_table with m and sd"
        ),
        {
          data <- get_version_data(factory(), "norms", ver)
          expect_true("lookup_table" %in% names(S7::props(data)))

          expect_s3_class(data@lookup_table, "data.frame")

          expect_true("m" %in% names(data@lookup_table))
          expect_true("sd" %in% names(data@lookup_table))
        }
      )

      test_that(
        paste0(
          nm,
          " '",
          ver,
          "' norms version data contains expected covar_fns"
        ),
        {
          data <- get_version_data(factory(), "norms", ver)
          expect_true("covar_fns" %in% names(S7::props(data)))
          for (cv in spec$norms_covars) {
            expect_true(cv %in% names(data@covar_fns), info = cv)
          }
        }
      )
    }
  }

  # --- Regression version tests ---
  if (has_regression) {
    for (ver in spec$regression_versions) {
      test_that(
        paste0(
          ".setup_",
          nm,
          "_versions registers the '",
          ver,
          "' regression version"
        ),
        {
          expect_true(ver %in% list_method_versions(factory(), "regression"))
        }
      )
    }

    test_that(paste0(nm, " regression version data contains coefs with rmse"), {
      for (ver in spec$regression_versions) {
        data <- get_version_data(factory(), "regression", ver)
        expect_true("coefs" %in% names(S7::props(data)), info = ver)
        expect_true("rmse" %in% names(data@coefs), info = ver)
      }
    })

    test_that(
      paste0(
        nm,
        " regression version data contains covar_fns for age, sex, and educ"
      ),
      {
        for (ver in spec$regression_versions) {
          data <- get_version_data(factory(), "regression", ver)
          expect_true("covar_fns" %in% names(S7::props(data)), info = ver)
          expect_true("age" %in% names(data@covar_fns), info = ver)
          expect_true("sex" %in% names(data@covar_fns), info = ver)
          expect_true("educ" %in% names(data@covar_fns), info = ver)
        }
      }
    )
  }

  # =========================================================================
  # Standardization tests (exercises covar_fns)
  # =========================================================================

  mid_score <- floor(mean(spec$range))
  reg_test_covars <- list(age = 70, sex = 1, educ = 16, race = 1, delay = 0)
  norms_test_covars <- list(age = 70, educ = 16, sex = 1)

  if (has_regression) {
    for (ver in spec$regression_versions) {
      test_that(
        # fmt: skip
        paste0(nm, " std_using_regression returns numeric for version '", ver, "'"),
        {
          scores <- factory(rep(mid_score, 2))
          version_data <- get_version_data(scores, "regression", ver)
          covars_needed <- setdiff(
            names(version_data@coefs),
            c("intercept", "rmse")
          )
          test_covars <- reg_test_covars[
            intersect(names(reg_test_covars), covars_needed)
          ]

          result <- do.call(
            std_using_regression,
            c(list(scores = scores, version = ver), test_covars)
          )
          expect_type(result, "double")
          expect_length(result, length(scores))
        }
      )
    }
  }

  if (has_norms) {
    for (ver in spec$norms_versions) {
      test_that(
        paste0(nm, " std_using_norms returns numeric for version '", ver, "'"),
        {
          scores <- factory(rep(mid_score, 2))
          test_covars <- norms_test_covars[spec$norms_covars]
          test_covars <- lapply(test_covars, \(x) rep(x, length(scores)))

          result <- do.call(
            std_using_norms,
            c(list(scores = scores, version = ver), test_covars)
          )
          expect_type(result, "double")
          expect_length(result, length(scores))
        }
      )
    }
  }

  # --- Default tests ---
  if (!is.null(spec$default)) {
    test_that(
      paste0(
        ".setup_",
        nm,
        "_versions sets the default method to '",
        spec$default$method,
        "' / '",
        spec$default$version,
        "'"
      ),
      {
        default <- get_std_defaults(factory())
        expect_equal(default$method, spec$default$method)
        expect_equal(default$version, spec$default$version)
      }
    )
  }
}
