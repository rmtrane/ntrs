# Set the default standardization method (and version) an `npsych_scores` subclass

Set the default standardization method (and version) an `npsych_scores`
subclass

## Usage

``` r
set_std_defaults(scores, method, version = NULL, overwrite = FALSE)
```

## Arguments

- scores:

  S7 object of class `npsych_scores` with subclass.

- method:

  Character string, for example "norms" or "regression"

- version:

  Character string identifying the version to set as default

- overwrite:

  Logical (default: `FALSE`); should existing default, if it exists, be
  overwritten?
