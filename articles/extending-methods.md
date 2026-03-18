# New Standardization Methods

`ntrs` ships with two standardization methods — `norms` and `regression`
— but the system is designed to be extended. Any S7 generic named
`std_using_*` is automatically discovered by
[`list_std_methods()`](https://rmtrane.github.io/ntrs/reference/list_std_methods.md)
and can be called via
[`std()`](https://rmtrane.github.io/ntrs/reference/std.md).

This vignette walks through creating a new method from scratch, then
discusses how to package it as a standalone extension (using
[ntrsTscores](https://github.com/rmtrane/ntrsTscores) as a real-world
example).

## Part 1: A toy method — `std_using_percentile`

Let’s create a method that converts raw scores to percentile ranks using
the empirical CDF from a reference sample.

### Defining the generic

A standardization generic must:

1.  Be named `std_using_<method>` (so `ntrs` can discover it)
2.  Be an S7 generic dispatching on `scores`

``` r
std_using_percentile <- S7::new_generic(
  "std_using_percentile",
  dispatch_args = "scores"
)
```

### Implementing a method on `npsych_scores`

Implementing a method on the parent class `npsych_scores` means it
applies to **all** score types. This is the simplest approach when the
logic is the same across tests.

Our method will take a `ref_scores` argument — a numeric vector of
reference sample scores — and compute percentile ranks:

``` r
S7::method(std_using_percentile, npsych_scores) <- function(
  scores,
  ...,
  ref_scores
) {
  raw <- as.numeric(remove_error_codes(scores))
  ecdf_fn <- stats::ecdf(ref_scores)
  ecdf_fn(raw) * 100
}
```

That’s it. The method is immediately available:

``` r
list_std_methods(MOCATOTS())
```

    [1] "norms"      "regression" "percentile"

And callable via
[`std()`](https://rmtrane.github.io/ntrs/reference/std.md):

``` r
moca <- MOCATOTS(c(22, 25, 28))
ref <- c(20, 22, 23, 24, 25, 25, 26, 27, 28, 29, 30)

std(moca, method = "percentile", ref_scores = ref)
```

    [1] 18.18182 54.54545 81.81818
    attr(,"method")
    [1] "percentile"

Notice that our method doesn’t use a `version` parameter. That’s fine —
[`std()`](https://rmtrane.github.io/ntrs/reference/std.md) passes
`version` only when it’s non-NULL, and
[`list_std_methods()`](https://rmtrane.github.io/ntrs/reference/list_std_methods.md)
doesn’t require version registration for methods without a `version`
formal. Also, notice how `std_using_percentile()` removes error codes
before computing percentiles. It is on the method implementer to decide
how to handle error codes — the
[`remove_error_codes()`](https://rmtrane.github.io/ntrs/reference/remove_error_codes.md)
helper is available, and recommended, but needs to be explicitly called.

### Using the version registry (optional)

If you want your method to support multiple named versions (e.g.,
different reference samples), add a `version` parameter and use the
version registry. This requires a few more steps:

1.  Create an S7 class for your version data (inheriting from
    `std_version`)
2.  Write a registration function
3.  Use
    [`get_version_data()`](https://rmtrane.github.io/ntrs/reference/get_version_data.md)
    in your method

Let’s extend our percentile method to support named reference datasets.

**Step 1: Create an S7 class for your version data**

Your class must inherit from `std_version`, which provides the common
properties (`scores_class`, `method_name`, `version_id`, `description`).
Add any method-specific properties:

``` r
percentile_version <- S7::new_class(
  "percentile_version",
  parent = std_version,
  properties = list(
    ref_scores = S7::class_numeric
  ),
  constructor = function(
    scores_class,
    version_id,
    ref_scores,
    description = ""
  ) {
    S7::new_object(
      S7::S7_object(),
      scores_class = scores_class,
      method_name = "percentile",
      version_id = version_id,
      ref_scores = ref_scores,
      description = description
    )
  }
)
```

**Step 2: Write a registration function**

This is a thin wrapper around the internal
[`.register_std_version()`](https://rmtrane.github.io/ntrs/reference/dot-register_std_version.md):

``` r
register_percentile_version <- function(
  scores,
  version,
  ref_scores,
  description = "",
  overwrite = FALSE
) {
  scores_class <- S7::S7_class(scores)@name
  version_obj <- percentile_version(
    scores_class = scores_class,
    version_id = version,
    ref_scores = ref_scores,
    description = description
  )
  ntrs:::.register_std_version(version_obj, overwrite = overwrite)
}
```

**Step 3: Update the method to use
[`get_version_data()`](https://rmtrane.github.io/ntrs/reference/get_version_data.md)**

Now redefine the method with a `version` parameter, and look up the
reference data from the registry:

``` r
S7::method(std_using_percentile, npsych_scores) <- function(
  scores,
  ...,
  version
) {
  vdata <- get_version_data(scores, "percentile", version)
  raw <- as.numeric(remove_error_codes(scores))
  ecdf_fn <- stats::ecdf(vdata@ref_scores)
  ecdf_fn(raw) * 100
}
```

    Overwriting method std_using_percentile(<ntrs::npsych_scores>)

**Step 4: Register versions and set defaults**

``` r
register_percentile_version(
  MOCATOTS(),
  version = "community",
  ref_scores = c(20, 22, 23, 24, 25, 25, 26, 27, 28, 29, 30),
  description = "Community-dwelling sample"
)

register_percentile_version(
  MOCATOTS(),
  version = "clinic",
  ref_scores = c(15, 17, 18, 20, 21, 22, 23, 24, 25, 26, 27),
  description = "Memory clinic sample"
)

set_std_defaults(
  MOCATOTS(),
  method = "percentile",
  version = "community",
  overwrite = TRUE
)
```

    ℹ Default method and version for "MOCATOTS" was previously set to "regression"
      and "updated_2025.06".
    ℹ Will be overwritten.
    ✔ Set "community" as default for percentile method on <MOCATOTS>

Now versions appear in the discovery functions:

``` r
list_method_versions(MOCATOTS(), "percentile")
```

    [1] "clinic"    "community"

And [`std()`](https://rmtrane.github.io/ntrs/reference/std.md) can
resolve the default:

``` r
moca <- MOCATOTS(c(22, 25, 28))

# Uses the default "community" version
std(moca)
```

    [1] 18.18182 54.54545 81.81818
    attr(,"method")
    [1] "percentile"
    attr(,"version")
    [1] "community"

``` r
# Or specify explicitly
std(moca, method = "percentile", version = "clinic")
```

    [1]  54.54545  81.81818 100.00000
    attr(,"method")
    [1] "percentile"
    attr(,"version")
    [1] "clinic"

## Part 2: Score-specific method overrides

You can register a method for a *specific* score class that takes
precedence over the parent class method. This is useful when certain
tests need special handling.

For example, Trail Making tests are timed — higher scores mean *worse*
performance. Let’s make `std_using_percentile` flip the percentiles for
`TRAILA`:

``` r
S7::method(std_using_percentile, TRAILA) <- function(
  scores,
  ...,
  ref_scores
) {
  raw <- as.numeric(remove_error_codes(scores))
  ecdf_fn <- stats::ecdf(ref_scores)
  # Flip: 100 - percentile, so higher = better
  (1 - ecdf_fn(raw)) * 100
}
```

Now the dispatch works as expected:

``` r
# MOCATOTS uses the parent method (higher = better)
std(
  MOCATOTS(c(22, 28)),
  method = "percentile",
  version = "community"
)
```

    [1] 18.18182 81.81818
    attr(,"method")
    [1] "percentile"
    attr(,"version")
    [1] "community"

``` r
# TRAILA uses the specific method (lower time = better)
std(
  TRAILA(c(30, 60)),
  method = "percentile",
  ref_scores = c(20, 30, 40, 50, 60, 80, 100)
)
```

    [1] 71.42857 28.57143
    attr(,"method")
    [1] "percentile"

S7 dispatch order: specific class method → parent class method. The same
pattern is used in `ntrs` itself — `std_using_norms` and
`std_using_regression` have methods on `npsych_scores` (the parent) that
handle all tests uniformly.

## Part 3: Packaging as an extension

The [ntrsTscores](https://github.com/rmtrane/ntrsTscores) package
demonstrates how to package a new standardization method as a standalone
R package that extends `ntrs`. It adds T-score standardization
(`std_using_tscores`) for ~19 tests. Here’s the key pattern:

### Package structure

    ntrsTscores/
    ├── R/
    │   ├── std_using_tscores.R            # S7 generic definition
    │   ├── std_using_tscores-ANIMALS.R    # Method for ANIMALS
    │   ├── std_using_tscores-TRAILA.R     # Method for TRAILA
    │   ├── ...                            # One file per test
    │   ├── scale_scores.R                 # Helper: raw → scaled scores
    │   ├── SS.R                           # Reference data (scaled score tables)
    │   └── zzz.R                          # .onLoad / .onAttach hooks
    ├── DESCRIPTION
    └── tests/

### Key patterns

**1. Define the generic once** — in `R/std_using_tscores.R`:

``` r
std_using_tscores <- S7::new_generic(
  "std_using_tscores",
  dispatch_args = "scores"
)
```

Note that this method doesn’t use a `version` parameter — the
coefficients are hardcoded per test. This is a valid design when there’s
only one set of norms.

**2. Register S7 methods in `.onLoad()`** — in `R/zzz.R`:

``` r
.onLoad <- function(...) {
  S7::methods_register()
}
```

[`S7::methods_register()`](https://rconsortium.github.io/S7/reference/methods_register.html)
scans the package namespace and registers all
[`S7::method()`](https://rconsortium.github.io/S7/reference/method.html)
calls. This is essential — without it, your methods won’t be found.

**3. Set defaults in `.onAttach()`** — so they’re set when the user
loads the package:

``` r
.onAttach <- function(...) {
  ntrs::set_std_defaults(REY123(), method = "tscores")
}
```

**4. Each method file** follows a consistent pattern — apply
test-specific regression coefficients to scaled scores:

``` r
S7::method(std_using_tscores, ANIMALS) <- function(
  scores,
  ...,
  age,
  sex,
  educ
) {
  ss <- scale_scores(scores)
  tscore <- round(
    50 + (ss - (10.69 + age * -0.007 + sex * -0.654 + educ * 0.213)) / 0.311,
    0
  )
  pmax(pmin(tscore, 100), 0)
}
```

### DESCRIPTION dependencies

Your extension package needs:

``` yaml
Imports:
  ntrs, 
  S7
```

`Imports` is sufficient — `ntrs` generic discovery
([`.find_std_generics()`](https://rmtrane.github.io/ntrs/reference/dot-find_std_generics.md))
searches all **loaded** namespaces, and `Imports` loads the `ntrs`
namespace. Users who want score constructors like
[`MOCATOTS()`](https://rmtrane.github.io/ntrs/reference/MOCATOTS.md) on
the search path simply call
[`library(ntrs)`](https://rmtrane.github.io/ntrs/) themselves (which
they likely already do).

If you prefer that
[`library(yourpkg)`](https://rdrr.io/r/base/library.html) automatically
attaches `ntrs` for user convenience, use `Depends: ntrs` instead.

## Summary

To add a new standardization method to `ntrs`:

1.  **Create an S7 generic** named `std_using_<method>` dispatching on
    `scores`
2.  **Implement methods** on `npsych_scores` (for all tests) or specific
    subclasses (for test-specific logic)
3.  **Optionally** use the version registry if your method supports
    multiple parameter sets
4.  **To package**: put the generic + methods in an R package, call
    [`S7::methods_register()`](https://rconsortium.github.io/S7/reference/methods_register.html)
    in `.onLoad()`, and `Imports: ntrs` in DESCRIPTION
