# New Scores and Versions

This vignette shows how to extend `ntrs` by creating new
neuropsychological test score types and registering standardization
versions for them. You might do this to:

- Add a test that `ntrs` doesn’t ship with
- Register your own normative data for an existing test
- Use site-specific regression coefficients

## Creating a new score type

Every test score in `ntrs` is an S7 subclass of `npsych_scores`, created
with the
[`new_npsych_scores()`](https://rmtrane.github.io/ntrs/reference/new_npsych_scores.md)
factory function. Let’s create one for a fictional “Digit Symbol” test:

``` r
DSYM <- new_npsych_scores(
  name = "DSYM",
  label = "Digit Symbol",
  domain = "Processing Speed",
  short_descriptor = "Digit Symbol Substitution — total correct in 90s",
  range = c(0, 80),
  codes = c(
    "Not administered" = 88,
    "Could not complete" = 99
  )
)
```

The arguments are:

- **`name`**: The class name (used as the registry key and in output)
- **`label`**: A human-readable short name
- **`domain`**: The cognitive domain (optional)
- **`short_descriptor`**: A longer description (optional)
- **`range`**: The minimum and maximum *valid* scores (inclusive)
- **`codes`**: Named numeric vector of special/error codes that may fall
  outside the valid range

`DSYM` is now a constructor function. Use it to wrap raw numeric data:

``` r
scores <- DSYM(c(45, 62, 88, 33, NA))
scores
```

    <ntrs::DSYM> num [1:5] 45 62 88 33 NA
     @ label           : chr "Digit Symbol"
     @ domain          : chr "Processing Speed"
     @ short_descriptor: chr "Digit Symbol Substitution — total correct in 90s"
     @ range           : num [1:2] 0 80
     @ codes           : Named num [1:2] 88 99
     .. - attr(*, "names")= chr [1:2] "Not administered" "Could not complete"

The value `88` is stored as-is because it’s a registered code ("Not
administered"). Validation catches values outside the range *and* codes:

``` r
DSYM(c(45, 100))
```

    Error:
    ! <ntrs::DSYM> object is invalid:
    - `scores` must all be in the range 0 and 80 or one of the `codes` 88 (Not administered) or 99 (Could not complete).

The new score is immediately discoverable:

``` r
"DSYM" %in% list_npsych_scores()
```

    [1] TRUE

### Working with error codes

Use
[`remove_error_codes()`](https://rmtrane.github.io/ntrs/reference/remove_error_codes.md)
to replace codes with `NA` for analysis, or
[`replace_codes()`](https://rmtrane.github.io/ntrs/reference/replace_codes.md)
to see their labels:

``` r
remove_error_codes(scores)
```

    [1] 45 62 NA 33 NA

``` r
replace_codes(scores)
```

    [1] "45"               "62"               "Not administered" "33"
    [5] NA                

## Registering a norms version

To standardize scores, you need to register at least one standardization
version. Let’s start with norms-based standardization, which uses a
lookup table of means and standard deviations stratified by covariates.
This is implemented in the `ntrs` method
[`std_using_norms()`](https://rmtrane.github.io/ntrs/reference/std_using_norms.md),
so we just need to provide the necessary data and register it with
[`register_norms_version()`](https://rmtrane.github.io/ntrs/reference/register_norms_version.md),
and the method will be available for our new `DSYM` scores.

### The lookup table

The lookup table is a data frame with columns for each covariate plus
`m` (mean) and `sd` (standard deviation). Optionally, it can include `n`
(sample size). Here’s a small example stratified by age group and sex:

``` r
dsym_norms <- data.frame(
  age = factor(rep(c("<65", "65-74", "75+"), each = 2)),
  sex = factor(rep(c("m", "f"), 3)),
  m = c(55, 58, 48, 51, 40, 43),
  sd = c(10, 9, 11, 10, 12, 11)
)

dsym_norms
```

        age sex  m sd
    1   <65   m 55 10
    2   <65   f 58  9
    3 65-74   m 48 11
    4 65-74   f 51 10
    5   75+   m 40 12
    6   75+   f 43 11

### Covariate functions

Since users pass raw covariates (e.g., `age = 72`, `sex = 1`), you need
**covariate functions** that transform them to match the lookup table
format. Each function takes a numeric vector and returns the transformed
values:

``` r
dsym_covar_fns <- list(
  age = \(x) {
    dplyr::case_when(
      x < 65 ~ "<65",
      x >= 65 & x < 75 ~ "65-74",
      x >= 75 ~ "75+"
    ) |>
      factor(levels = c("<65", "65-74", "75+"))
  },
  sex = \(x) factor(x, levels = c(1, 2), labels = c("m", "f"))
)
```

The names in `covar_fns` must match the covariate column names in the
lookup table (here, `age` → `age`, `sex` → `sex`).

### Registering

Now register with
[`register_norms_version()`](https://rmtrane.github.io/ntrs/reference/register_norms_version.md):

``` r
register_norms_version(
  scores = DSYM(),
  version = "example_v1",
  lookup_table = dsym_norms,
  covar_fns = dsym_covar_fns
)
```

The version is now discoverable:

``` r
list_method_versions(DSYM(), "norms")
```

    [1] "example_v1"

We can standardize using it:

``` r
std(
  DSYM(c(45, 62, 33)),
  method = "norms",
  version = "example_v1",
  age = c(72, 68, 80),
  sex = c(1, 2, 1)
)
```

    [1] -0.2727273  1.1000000 -0.5833333

## Registering a regression version

Regression-based standardization uses a linear model:
$z = \left( \text{raw} - \text{predicted} \right)/\text{RMSE}$, where
predicted values come from regression coefficients.

### The coefficients

Provide a named numeric vector with `intercept`, `rmse`, and any
covariate names:

``` r
dsym_coefs <- c(
  intercept = 65.2,
  age = -0.35,
  sex = 3.1,
  educ = 0.8,
  rmse = 9.5
)
```

### Covariate functions (optional)

For regression, `covar_fns` transform raw inputs before they enter the
linear model. If omitted, identity functions are used for each
covariate. Here we recode sex from `1/2` to `0/1`, and truncate
education at 8 and 30 years:

``` r
register_regression_version(
  scores = DSYM(),
  version = "example_reg",
  coefs = dsym_coefs,
  covar_fns = list(
    age = identity,
    sex = \(x) as.numeric(x == 2),
    educ = \(x) pmin(pmax(x, 8), 30)
  )
)
```

Again, the version is discoverable and can be used for standardization:

``` r
list_method_versions(DSYM(), "regression")
```

    [1] "example_reg"

``` r
std(
  DSYM(c(45, 62, 33)),
  method = "regression",
  version = "example_reg",
  age = c(72, 68, 80),
  sex = c(1, 2, 1),
  educ = c(16, 12, 14)
)
```

    [1] -0.8210526  0.8315789 -1.6210526

We can verify that the education variable is indeed truncated by
comparing the result at the boundary to a value above it:

``` r
std(
  DSYM(c(45, 45)),
  method = "regression",
  version = "example_reg",
  age = c(72, 72),
  sex = c(1, 1),
  educ = c(30, 40) # 40 is truncated to
)
```

    [1] -2 -2

## Setting defaults and standardizing

Set a default method and version so users can call
[`std()`](https://rmtrane.github.io/ntrs/reference/std.md) without
specifying them:

``` r
set_std_defaults(DSYM(), method = "regression", version = "example_reg")
```

    ✔ Set "example_reg" as default for regression method on <DSYM>

``` r
get_std_defaults(DSYM())
```

    $method
    [1] "regression"

    $version
    [1] "example_reg"

Now standardize:

``` r
test_scores <- DSYM(c(45, 62, 33))

# Uses the default (regression)
std(test_scores, age = 72, sex = 1, educ = 16)
```

    [1] -0.8210526  0.9684211 -2.0842105

``` r
# Explicitly use norms
std(test_scores, method = "norms", version = "example_v1", age = 72, sex = 1)
```

    [1] -0.2727273  1.2727273 -1.3636364

## Optional: `raw_scores_fn` and `post_proc_fn`

Both
[`register_norms_version()`](https://rmtrane.github.io/ntrs/reference/register_norms_version.md)
and
[`register_regression_version()`](https://rmtrane.github.io/ntrs/reference/register_regression_version.md)
accept two optional transformation functions:

- **`raw_scores_fn`**: Applied to raw scores *before* standardization.
  Useful for log-transforms or reversals.
- **`post_proc_fn`**: Applied to z-scores *after* standardization.
  Useful for sign flips (e.g., when higher raw scores mean worse
  performance).

For example, Trail Making tests are timed, so higher scores = worse
performance. A `post_proc_fn = \(x) -x` flips the z-scores so that
positive always means better:

``` r
register_norms_version(
  scores = TRAILA(),
  version = "my_flipped",
  lookup_table = my_traila_norms,
  covar_fns = my_covar_fns,
  post_proc_fn = \(x) -x
)
```

## Adding a version to an existing score

You aren’t limited to new scores — you can register additional versions
for any score that `ntrs` already provides. For instance, to add
site-specific norms for `MOCATOTS`:

``` r
my_moca_norms <- data.frame(
  age = factor(c("<70", "70+", "<70", "70+")),
  sex = factor(c("m", "m", "f", "f")),
  m = c(26.1, 24.3, 26.5, 24.8),
  sd = c(2.8, 3.1, 2.6, 2.9)
)

register_norms_version(
  scores = MOCATOTS(),
  version = "my_site",
  lookup_table = my_moca_norms,
  covar_fns = list(
    age = \(x) factor(ifelse(x < 70, "<70", "70+")),
    sex = \(x) factor(x, levels = c(1, 2), labels = c("m", "f"))
  )
)

list_method_versions(MOCATOTS(), "norms")
```

    [1] "my_site" "nacc"    "updated"

The new version coexists with the built-in ones and can be used via
`std(moca_scores, method = "norms", version = "my_site", age = 72, sex = 1)`.

## Packaging it up

If you want your new scores and versions to be available every time you
load your package, follow the pattern used by `ntrs` internally:

1.  Create `R/npsych_scores-DSYM.R` with the
    [`new_npsych_scores()`](https://rmtrane.github.io/ntrs/reference/new_npsych_scores.md)
    call and a `.setup_DSYM_versions()` function that registers all
    versions and sets defaults.

2.  In your package’s `.onLoad()`, call
    [`S7::methods_register()`](https://rconsortium.github.io/S7/reference/methods_register.html)
    and then your setup functions. If your package extends `ntrs`, the
    `.onLoad()` hook in `ntrs` will auto-discover any
    `.setup_*_versions()` functions in the `ntrs` namespace, but for
    your own package you’ll need to call them explicitly. It is
    recommended that you do this in `.onAttach()`.

See `R/npsych_scores-MOCATOTS.R` in the `ntrs` source for a complete
template.
