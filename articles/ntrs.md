# Get Started

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

When loading the package, messages are printed for all the defaults
being set. These can be suppressed with
[`suppressPackageStartupMessages()`](https://rdrr.io/r/base/message.html).

``` r
library(ntrs)
```

    ✔ Set "updated_2025.06" as default for regression method on <DIGBACLS>
    ✔ Set "updated_2025.06" as default for regression method on <MINTTOTS>
    ✔ Set "nacc_legacy" as default for regression method on <NACCMMSE>
    ✔ Set "updated_2025.06" as default for regression method on <OTRLBRR>
    ✔ Set "updated_2025.06" as default for regression method on <UDSVERTN>
    ✔ Set "nacc_legacy" as default for regression method on <BOSTON>
    ✔ Set "updated_2025.06" as default for regression method on <DIGBACCT>
    ✔ Set "updated_2025.06" as default for regression method on <CRAFTDVR>
    ✔ Set "updated_2025.06" as default for regression method on <OTRLARR>
    ✔ Set "updated_2025.06" as default for regression method on <UDSVERFC>
    ✔ Set "ravlt_trials" as default for norms method on <REYDREC>
    ✔ Set "ravlt_trials" as default for norms method on <REY6REC>
    ✔ Set "updated_2025.06" as default for regression method on <MOCATOTS>
    ✔ Set "updated_2025.06" as default for regression method on <ANIMALS>
    ✔ Set "nacc_legacy" as default for regression method on <MEMUNITS>
    ✔ Set "updated_2025.06" as default for regression method on <CRAFTURS>
    ✔ Set "updated_2025.06" as default for regression method on <DIGFORCT>
    ✔ Set "updated_2025.06" as default for regression method on <VEG>
    ✔ Set "updated_2025.06" as default for regression method on <UDSBENTD>
    ✔ Set "nacc_legacy" as default for regression method on <DIGIB>
    ✔ Set "updated_2025.06" as default for regression method on <CRAFTDRE>
    ✔ Set "updated_2025.06" as default for regression method on <TRAILB>
    ✔ Set "updated_2025.06" as default for regression method on <UDSVERLC>
    ✔ Set "nacc_legacy" as default for regression method on <DIGIFLEN>
    ✔ Set "updated_2025.06" as default for regression method on <OTRAILB>
    ✔ Set "updated_2025.06" as default for regression method on <UDSBENTC>
    ✔ Set "updated_2025.06" as default for regression method on <TRAILA>
    ✔ Set "nacc_legacy" as default for regression method on <LOGIMEM>
    ✔ Set "nacc_legacy" as default for regression method on <DIGIBLEN>
    ✔ Set "updated_2025.06" as default for regression method on <OTRAILA>
    ✔ Set "nacc_legacy" as default for regression method on <DIGIF>
    ✔ Set "updated_2025.06" as default for regression method on <CRAFTVRS>
    ✔ Set "updated_2025.06" as default for regression method on <DIGFORSL>

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

    <ntrs::std_npsych_scores> num [1:3] -0.417 0.833 1.667
     @ scores_subclass: chr "MOCATOTS"
     @ description    : chr "Standardized using norms, version nacc. Adjusted for covariates age, educ, sex."
     @ method         : chr "norms"
     @ version        : chr "nacc"

The result is an `std_npsych_scores` object. You can inspect the
standardization metadata via S7 properties:

``` r
z@method
```

    [1] "norms"

``` r
z@version
```

    [1] "nacc"

``` r
z@description
```

    [1] "Standardized using norms, version nacc. Adjusted for covariates age, educ, sex."

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
z_reg
```

    <ntrs::std_npsych_scores> num [1:3] -0.363 0.891 1.727
     @ scores_subclass: chr "MOCATOTS"
     @ description    : chr "Standardized using regression, version updated_2025.06. Adjusted for covariates age, sex, educ, race."
     @ method         : chr "regression"
     @ version        : chr "updated_2025.06"

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

    <ntrs::std_npsych_scores> num [1:3] -0.353 0.831 1.621
     @ scores_subclass: chr "MOCATOTS"
     @ description    : chr "Standardized using norms, version updated. Adjusted for covariates sex, age, educ."
     @ method         : chr "norms"
     @ version        : chr "updated"

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

Each standardized column is an `std_npsych_scores` object carrying
`@method`, `@version`, and `@description` properties. Use
[`methods_from_std_data()`](https://rmtrane.github.io/ntrs/reference/methods_from_std_data.md)
to extract them conveniently:

``` r
attr(result, "prefix_std")
```

    [1] "z_"

``` r
methods_from_std_data(result)
```

    $moca
               method           version
         "regression" "updated_2025.06"

    $animals
               method           version
         "regression" "updated_2025.06" 

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
