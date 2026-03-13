# Getting Started

``` r
suppressPackageStartupMessages(
  library(ntrs)
)
```

## Overview

`ntrs` provides an S7-based interface for standardizing
neuropsychological test scores. The typical workflow is:

1.  Create score objects using factory functions (e.g.,
    [`MOCATOTS()`](https://rmtrane.github.io/ntrs/reference/MOCATOTS.md),
    [`ANIMALS()`](https://rmtrane.github.io/ntrs/reference/ANIMALS.md))
2.  Standardize scores with
    [`std()`](https://rmtrane.github.io/ntrs/reference/std.md) or
    batch-standardize a data frame with
    [`std_data()`](https://rmtrane.github.io/ntrs/reference/std_data.md)

## Creating score objects

Each neuropsychological test has a dedicated factory function that wraps
raw numeric scores with metadata (valid range, error codes, etc.):

``` r
moca <- MOCATOTS(c(25, 28, 30))
moca
```

    <ntrs::MOCATOTS> num [1:3] 25 28 30
     @ label           : chr "MoCA"
     @ domain          : chr "General Cognition"
     @ short_descriptor: chr "MoCA Total Raw Score - uncorrected"
     @ range           : num [1:2] 0 30
     @ codes           : Named num [1:2] -4 88
     .. - attr(*, "names")= chr [1:2] "Not available: UDS form submitted did not collect data in this way, or a skip pattern precludes response to this question" "Item(s) or whole test not administered"

To see all available tests:

``` r
list_npsych_scores()
```

     [1] "ANIMALS"   "BOSTON"    "CDRGLOB"   "CDRSUM"    "CRAFTDRE"  "CRAFTDVR"
     [7] "CRAFTURS"  "CRAFTVRS"  "DIGBACCT"  "DIGBACLS"  "DIGFORCT"  "DIGFORSL"
    [13] "DIGIB"     "DIGIBLEN"  "DIGIF"     "DIGIFLEN"  "LOGIMEM"   "MEMUNITS"
    [19] "MINTTOTS"  "MOCACLOCK" "MOCATOTS"  "MOCBTOTS"  "NACCGDS"   "NACCMMSE"
    [25] "OTRAILA"   "OTRAILB"   "OTRLARR"   "OTRLBRR"   "REY1REC"   "REY2REC"
    [31] "REY3REC"   "REY4REC"   "REY5REC"   "REY6REC"   "REYAREC"   "REYDLIST"
    [37] "REYDREC"   "REYFPOS"   "REYTCOR"   "REYTOTAL"  "TRAILA"    "TRAILALI"
    [43] "TRAILARR"  "TRAILB"    "TRAILBLI"  "TRAILBRR"  "UDSBENRS"  "UDSBENTC"
    [49] "UDSBENTD"  "UDSVERFC"  "UDSVERLC"  "UDSVERTN"  "VEG"       "WAIS"     

## Discovering standardization options

Before standardizing, you can check which methods and versions are
available for a given test:

``` r
list_std_methods(moca)
```

    [1] "norms"      "regression"

``` r
list_method_versions(moca, "norms")
```

    [1] "nacc"    "updated"

``` r
list_method_versions(moca, "regression")
```

    [1] "nacc"            "updated_2024.06" "updated_2025.06"

Each test has registered defaults so you don’t need to specify a method
every time:

``` r
get_std_defaults(moca)
```

    $method
    [1] "regression"

    $version
    [1] "updated_2025.06"

These can be changed globally with
[`set_std_defaults()`](https://rmtrane.github.io/ntrs/reference/set_std_defaults.md):

``` r
set_std_defaults(
  moca,
  method = "norms",
  version = "nacc",
  overwrite = TRUE
)
```

    ℹ Default method and version for "MOCATOTS" was previously set to "regression"
      and "updated_2025.06".

    ℹ Will be overwritten.

    ✔ Set "nacc" as default for norms method on <MOCATOTS>

``` r
get_std_defaults(moca)
```

    $method
    [1] "norms"

    $version
    [1] "nacc"

Do note that changes to the default will not be saved from session to
session, so you may want to set them in your `.Rprofile` if you want
them to persist.

## Standardizing a single score vector

Use [`std()`](https://rmtrane.github.io/ntrs/reference/std.md) to
convert raw scores to z-scores. Covariates (age, sex, education, etc.)
are passed as named arguments:

``` r
z <- std(moca, age = 72, sex = 1, educ = 16)
z
```

    [1] -0.4166667  0.8333333  1.6666667

``` r
set_std_defaults(
  moca,
  method = "regression",
  version = "updated_2025.06",
  overwrite = TRUE
)
```

    ℹ Default method and version for "MOCATOTS" was previously set to "norms" and
      "nacc".

    ℹ Will be overwritten.

    ✔ Set "updated_2025.06" as default for regression method on <MOCATOTS>

``` r
z_reg <- std(moca, age = 72, sex = 1, educ = 16, race = 1)
```

To override the default method or version, specify them explicitly:

``` r
z_norms <- std(
  moca,
  method = "norms",
  version = "updated",
  age = 72,
  sex = 1,
  educ = 16
)
z_norms
```

    [1] -0.3532407  0.8313253  1.6210360

## Batch standardization with `std_data()`

When you have a data frame with multiple test score columns,
[`std_data()`](https://rmtrane.github.io/ntrs/reference/std_data.md)
standardizes all of them at once:

``` r
df <- data.frame(
  age = c(72, 75, 68),
  sex = c(1, 2, 1),
  educ = c(16, 14, 12),
  race = c(1, 2, 1),
  moca = MOCATOTS(c(25, 28, 22)),
  animals = ANIMALS(c(18, 20, 16))
)

result <- std_data(
  df,
  age = age,
  sex = sex,
  educ = educ,
  race = race
)

result
```

      age sex educ race moca animals     z_moca  z_animals
    1  72   1   16    1   25      18 -0.3628727 -0.6295726
    2  75   2   14    2   28      20  1.6463434  0.5252069
    3  68   1   12    1   22      16 -1.1015874 -0.7205741

You can override the method/version per test class:

``` r
result2 <- std_data(
  df,
  methods = list(
    MOCATOTS = list(method = "norms", version = "nacc")
  ),
  age = age,
  sex = sex,
  educ = educ,
  race = race
)
```

    Warning: `race` is not needed to standardize using "norms" when version is "nacc" and
    will be ignored.

``` r
result2
```

      age sex educ race moca animals     z_moca  z_animals
    1  72   1   16    1   25      18 -0.4166667 -0.6295726
    2  75   2   14    2   28      20  0.8928571  0.5252069
    3  68   1   12    1   22      16 -1.0303030 -0.7205741

## Changing defaults

If you prefer a different default method for your session, use
[`set_std_defaults()`](https://rmtrane.github.io/ntrs/reference/set_std_defaults.md):

``` r
set_std_defaults(
  MOCATOTS(),
  method = "norms",
  version = "nacc",
  overwrite = TRUE
)

# Subsequent calls use the new default
std(MOCATOTS(c(25, 28)), age = 72, sex = 1, educ = 16)
```
