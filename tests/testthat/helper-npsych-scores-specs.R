# Specifications for parameterized npsych_scores tests.
# Each entry drives both constructor tests and version-registration tests.

npsych_scores_specs <- list(
  # --- Group 1: Standard norms (nacc, updated) + regression (3 versions) ---

  list(
    name = "ANIMALS",
    label = "Animal Fluency",
    range = c(0, 77),
    codes = c(-4, 95, 96, 97, 98),
    out_of_range = 78,
    norms_versions = c("nacc", "updated"),
    norms_covars = c("age", "educ", "sex"),
    regression_versions = c("updated_2024.06", "updated_2025.06", "nacc"),
    default = list(method = "regression", version = "updated_2025.06")
  ),
  list(
    name = "CRAFTDRE",
    label = "Craft Delay - Paraphrase",
    range = c(0, 25),
    codes = c(-4, 95, 96, 97, 98),
    out_of_range = 26,
    norms_versions = c("nacc", "updated"),
    norms_covars = c("age", "educ", "sex"),
    regression_versions = c("updated_2024.06", "updated_2025.06", "nacc"),
    default = list(method = "regression", version = "updated_2025.06")
  ),
  list(
    name = "CRAFTDVR",
    label = "Craft Delay - Verbatim",
    range = c(0, 44),
    codes = c(-4, 95, 96, 97, 98),
    out_of_range = 45,
    norms_versions = c("nacc", "updated"),
    norms_covars = c("age", "educ", "sex"),
    regression_versions = c("updated_2024.06", "updated_2025.06", "nacc"),
    default = list(method = "regression", version = "updated_2025.06")
  ),
  list(
    name = "CRAFTURS",
    label = "Craft Immediate - Paraphrase",
    range = c(0, 25),
    codes = c(-4, 95, 96, 97, 98),
    out_of_range = 26,
    norms_versions = c("nacc", "updated"),
    norms_covars = c("age", "educ", "sex"),
    regression_versions = c("updated_2024.06", "updated_2025.06", "nacc"),
    default = list(method = "regression", version = "updated_2025.06")
  ),
  list(
    name = "CRAFTVRS",
    label = "Craft Immediate - Verbatim",
    range = c(0, 44),
    codes = c(-4, 95, 96, 97, 98),
    out_of_range = 45,
    norms_versions = c("nacc", "updated"),
    norms_covars = c("age", "educ", "sex"),
    regression_versions = c("updated_2024.06", "updated_2025.06", "nacc"),
    default = list(method = "regression", version = "updated_2025.06")
  ),
  list(
    name = "DIGBACCT",
    label = "Number Span Backward - Total",
    range = c(0, 14),
    codes = c(-4, 95, 96, 97, 98),
    out_of_range = 15,
    norms_versions = c("nacc", "updated"),
    norms_covars = c("age", "educ", "sex"),
    regression_versions = c("updated_2024.06", "updated_2025.06", "nacc"),
    default = list(method = "regression", version = "updated_2025.06")
  ),
  list(
    name = "DIGBACLS",
    label = "Number Span Backward - Span Length",
    range = c(0, 8),
    codes = c(-4, 95, 96, 97, 98),
    out_of_range = 9,
    norms_versions = c("nacc", "updated"),
    norms_covars = c("age", "educ", "sex"),
    regression_versions = c("updated_2024.06", "updated_2025.06", "nacc"),
    default = list(method = "regression", version = "updated_2025.06")
  ),
  list(
    name = "DIGFORCT",
    label = "Number Span Forward - Total",
    range = c(0, 14),
    codes = c(-4, 95, 96, 97, 98),
    out_of_range = 15,
    norms_versions = c("nacc", "updated"),
    norms_covars = c("age", "educ", "sex"),
    regression_versions = c("updated_2024.06", "updated_2025.06", "nacc"),
    default = list(method = "regression", version = "updated_2025.06")
  ),
  list(
    name = "DIGFORSL",
    label = "Number Span Forward - Span Length",
    range = c(0, 9),
    codes = c(-4, 95, 96, 97, 98),
    out_of_range = 10,
    norms_versions = c("nacc", "updated"),
    norms_covars = c("age", "educ", "sex"),
    regression_versions = c("updated_2024.06", "updated_2025.06", "nacc"),
    default = list(method = "regression", version = "updated_2025.06")
  ),
  list(
    name = "MINTTOTS",
    label = "MINT",
    range = c(0, 32),
    codes = c(-4, 95, 96, 97, 98),
    out_of_range = 33,
    norms_versions = c("nacc", "updated"),
    norms_covars = c("age", "educ", "sex"),
    regression_versions = c("updated_2024.06", "updated_2025.06", "nacc"),
    default = list(method = "regression", version = "updated_2025.06")
  ),
  list(
    name = "MOCATOTS",
    label = "MoCA",
    range = c(0, 30),
    codes = c(-4, 88),
    out_of_range = 31,
    norms_versions = c("nacc", "updated"),
    norms_covars = c("age", "educ", "sex"),
    regression_versions = c("updated_2024.06", "updated_2025.06", "nacc"),
    default = list(method = "regression", version = "updated_2025.06")
  ),
  list(
    name = "TRAILA",
    label = "Trailmaking Part A",
    range = c(0, 150),
    codes = c(-4, 995, 996, 997, 998),
    out_of_range = 151,
    norms_versions = c("nacc", "updated"),
    norms_covars = c("age", "educ", "sex"),
    regression_versions = c("updated_2024.06", "updated_2025.06", "nacc"),
    default = list(method = "regression", version = "updated_2025.06")
  ),
  list(
    name = "TRAILB",
    label = "Trailmaking Part B",
    range = c(0, 300),
    codes = c(-4, 995, 996, 997, 998),
    out_of_range = 301,
    norms_versions = c("nacc", "updated"),
    norms_covars = c("age", "educ", "sex"),
    regression_versions = c("updated_2024.06", "updated_2025.06", "nacc"),
    default = list(method = "regression", version = "updated_2025.06")
  ),
  list(
    name = "UDSBENTC",
    label = "Benson Figure Copy",
    range = c(0, 17),
    codes = c(-4, 95, 96, 97, 98),
    out_of_range = 18,
    norms_versions = c("nacc", "updated"),
    norms_covars = c("age", "educ", "sex"),
    regression_versions = c("updated_2024.06", "updated_2025.06", "nacc"),
    default = list(method = "regression", version = "updated_2025.06")
  ),
  list(
    name = "UDSBENTD",
    label = "Benson Delay",
    range = c(0, 17),
    codes = c(-4, 95, 96, 97, 98),
    out_of_range = 18,
    norms_versions = c("nacc", "updated"),
    norms_covars = c("age", "educ", "sex"),
    regression_versions = c("updated_2024.06", "updated_2025.06", "nacc"),
    default = list(method = "regression", version = "updated_2025.06")
  ),
  list(
    name = "UDSVERFC",
    label = "F Words",
    range = c(0, 40),
    codes = c(-4, 95, 96, 97, 98),
    out_of_range = 41,
    norms_versions = c("nacc", "updated"),
    norms_covars = c("age", "educ", "sex"),
    regression_versions = c("updated_2024.06", "updated_2025.06", "nacc"),
    default = list(method = "regression", version = "updated_2025.06")
  ),
  list(
    name = "UDSVERLC",
    label = "L Words",
    range = c(0, 40),
    codes = c(-4, 95, 96, 97, 98),
    out_of_range = 41,
    norms_versions = c("nacc", "updated"),
    norms_covars = c("age", "educ", "sex"),
    regression_versions = c("updated_2024.06", "updated_2025.06", "nacc"),
    default = list(method = "regression", version = "updated_2025.06")
  ),
  list(
    name = "UDSVERTN",
    label = "F+L Words",
    range = c(0, 80),
    codes = c(-4, 95, 96, 97, 98),
    out_of_range = 81,
    norms_versions = c("nacc", "updated"),
    norms_covars = c("age", "educ", "sex"),
    regression_versions = c("updated_2024.06", "updated_2025.06", "nacc"),
    default = list(method = "regression", version = "updated_2025.06")
  ),
  list(
    name = "VEG",
    label = "Vegetable Fluency",
    range = c(0, 77),
    codes = c(-4, 95, 96, 97, 98),
    out_of_range = 78,
    norms_versions = c("nacc", "updated"),
    norms_covars = c("age", "educ", "sex"),
    regression_versions = c("updated_2024.06", "updated_2025.06", "nacc"),
    default = list(method = "regression", version = "updated_2025.06")
  ),

  # --- Group 2: Regression only with nacc_legacy ---

  list(
    name = "BOSTON",
    label = "Boston Naming Test",
    range = c(0, 30),
    codes = c(-4, 95, 96, 97, 98),
    out_of_range = 31,
    norms_versions = NULL,
    norms_covars = NULL,
    regression_versions = c(
      "updated_2024.06",
      "updated_2025.06",
      "nacc_legacy"
    ),
    default = list(method = "regression", version = "nacc_legacy")
  ),
  list(
    name = "DIGIB",
    label = "Digit Span Backward - Total",
    range = c(0, 12),
    codes = c(-4, 95, 96, 97, 98),
    out_of_range = 13,
    norms_versions = NULL,
    norms_covars = NULL,
    regression_versions = c(
      "updated_2024.06",
      "updated_2025.06",
      "nacc_legacy"
    ),
    default = list(method = "regression", version = "nacc_legacy")
  ),
  list(
    name = "DIGIBLEN",
    label = "Digit Span Backward - Span Length",
    range = c(0, 8),
    codes = c(-4, 95, 96, 97, 98),
    out_of_range = 9,
    norms_versions = NULL,
    norms_covars = NULL,
    regression_versions = c(
      "updated_2024.06",
      "updated_2025.06",
      "nacc_legacy"
    ),
    default = list(method = "regression", version = "nacc_legacy")
  ),
  list(
    name = "DIGIF",
    label = "Digit Span Forward - Total",
    range = c(0, 12),
    codes = c(-4, 95, 96, 97, 98),
    out_of_range = 13,
    norms_versions = NULL,
    norms_covars = NULL,
    regression_versions = c(
      "updated_2024.06",
      "updated_2025.06",
      "nacc_legacy"
    ),
    default = list(method = "regression", version = "nacc_legacy")
  ),
  list(
    name = "DIGIFLEN",
    label = "Digit Span Forward - Span Length",
    range = c(0, 8),
    codes = c(-4, 95, 96, 97, 98),
    out_of_range = 9,
    norms_versions = NULL,
    norms_covars = NULL,
    regression_versions = c(
      "updated_2024.06",
      "updated_2025.06",
      "nacc_legacy"
    ),
    default = list(method = "regression", version = "nacc_legacy")
  ),
  list(
    name = "LOGIMEM",
    label = "Logical Memory, Immediate",
    range = c(0, 25),
    codes = c(-4, 95, 96, 97, 98),
    out_of_range = 26,
    norms_versions = NULL,
    norms_covars = NULL,
    regression_versions = c(
      "updated_2024.06",
      "updated_2025.06",
      "nacc_legacy"
    ),
    default = list(method = "regression", version = "nacc_legacy")
  ),
  list(
    name = "MEMUNITS",
    label = "Logical Memory, Delayed",
    range = c(0, 25),
    codes = c(-4, 95, 96, 97, 98),
    out_of_range = 26,
    norms_versions = NULL,
    norms_covars = NULL,
    regression_versions = c(
      "updated_2024.06",
      "updated_2025.06",
      "nacc_legacy"
    ),
    default = list(method = "regression", version = "nacc_legacy")
  ),
  list(
    name = "NACCMMSE",
    label = "MMSE",
    range = c(0, 30),
    codes = c(-4, 88, 95, 96, 97, 98),
    out_of_range = 31,
    norms_versions = NULL,
    norms_covars = NULL,
    regression_versions = c(
      "updated_2024.06",
      "updated_2025.06",
      "nacc_legacy"
    ),
    default = list(method = "regression", version = "nacc_legacy")
  ),

  # --- Group 3: Oral trails — norms (updated only) + regression ---

  list(
    name = "OTRAILA",
    label = "Oral Trailmaking Part A - Completion Time",
    range = c(0, 100),
    codes = c(-4, 888, 995, 996, 997, 998),
    out_of_range = 101,
    norms_versions = c("updated"),
    norms_covars = c("age", "sex"),
    regression_versions = c("updated_2024.06", "updated_2025.06"),
    default = list(method = "regression", version = "updated_2025.06")
  ),
  list(
    name = "OTRAILB",
    label = "Oral Trailmaking Part B - Completion Time",
    range = c(0, 300),
    codes = c(-4, 888, 995, 996, 997, 998),
    out_of_range = 301,
    norms_versions = c("updated"),
    norms_covars = c("age", "sex"),
    regression_versions = c("updated_2024.06", "updated_2025.06"),
    default = list(method = "regression", version = "updated_2025.06")
  ),
  list(
    name = "OTRLARR",
    label = "Oral Trailmaking Part A - Errors",
    range = c(0, 77),
    codes = c(-4, 88, 95, 96, 97, 98),
    out_of_range = 78,
    norms_versions = c("updated"),
    norms_covars = c("age", "sex"),
    regression_versions = c("updated_2024.06", "updated_2025.06"),
    default = list(method = "regression", version = "updated_2025.06")
  ),
  list(
    name = "OTRLBRR",
    label = "Oral Trailmaking Part B - Errors",
    range = c(0, 77),
    codes = c(-4, 88, 95, 96, 97, 98),
    out_of_range = 78,
    norms_versions = c("updated"),
    norms_covars = c("age", "sex"),
    regression_versions = c("updated_2024.06", "updated_2025.06"),
    default = list(method = "regression", version = "updated_2025.06")
  ),

  # --- Group 4: RAVLT with ravlt_trials norms + regression ---

  list(
    name = "REY6REC",
    label = "RAVLT Short Delay",
    range = c(0, 15),
    codes = c(-4, 88, 95, 96, 97, 98),
    out_of_range = 16,
    norms_versions = c("ravlt_trials"),
    norms_covars = c("age"),
    regression_versions = c("updated_2024.06", "updated_2025.06"),
    default = NULL
  ),
  list(
    name = "REYDREC",
    label = "RAVLT Long Delay",
    range = c(0, 15),
    codes = c(-4, 88, 95, 96, 97, 98),
    out_of_range = 16,
    norms_versions = c("ravlt_trials"),
    norms_covars = c("age"),
    regression_versions = c("updated_2024.06", "updated_2025.06"),
    default = NULL
  ),
  list(
    name = "REYTOTAL",
    label = "RAVLT Total Learning",
    range = c(0, 75),
    codes = numeric(),
    out_of_range = 76,
    norms_versions = c("ravlt_trials"),
    norms_covars = c("age"),
    regression_versions = c("updated_2024.06", "updated_2025.06"),
    default = NULL
  ),

  # --- Group 5: RAVLT norms only ---

  list(
    name = "REYDLIST",
    label = "RAVLT Distractor List",
    range = c(0, 15),
    codes = numeric(),
    out_of_range = 16,
    norms_versions = c("ravlt_trials"),
    norms_covars = c("age"),
    regression_versions = NULL,
    default = NULL
  ),

  # --- Group 6: Regression only, no default ---

  list(
    name = "REYAREC",
    label = "RAVLT Recognition",
    range = c(0, 100),
    codes = numeric(),
    out_of_range = 101,
    norms_versions = NULL,
    norms_covars = NULL,
    regression_versions = c("updated_2024.06", "updated_2025.06"),
    default = NULL
  ),
  list(
    name = "WAIS",
    label = "WAIS-R Digit Symbol",
    range = c(0, 93),
    codes = c(-4, 95, 96, 97, 98),
    out_of_range = 94,
    norms_versions = NULL,
    norms_covars = NULL,
    regression_versions = c("updated_2024.06", "updated_2025.06"),
    default = NULL
  )
)
