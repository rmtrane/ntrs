# Check whether an S7 class inherits from npsych_scores

Walks the S7 parent chain. Returns TRUE if npsych_scores appears
anywhere (but is not npsych_scores itself — we only want subclasses).

## Usage

``` r
.is_npsych_subclass(cls)
```

## Arguments

- cls:

  An S7 class object.

## Value

Logical.
