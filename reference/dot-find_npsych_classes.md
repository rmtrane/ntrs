# Discover all npsych_scores subclasses across loaded namespaces

Scans every loaded namespace (and optionally .GlobalEnv) for exported or
internal objects that are S7 classes inheriting from npsych_scores.
Results are cached and invalidated when loadedNamespaces() changes
(i.e., when a new package is loaded or unloaded).

## Usage

``` r
.find_npsych_classes()
```

## Value

A named list of S7 class objects (name -\> class object).
