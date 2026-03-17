# Package index

## Core Class

Create and inspect `npsych_scores` objects.

- [`npsych_scores()`](https://rmtrane.github.io/ntrs/reference/npsych_scores.md)
  : Neuropsychological Test Scores

- [`new_npsych_scores()`](https://rmtrane.github.io/ntrs/reference/new_npsych_scores.md)
  : Create an npsych_scores subclass

- [`is_npsych_scores()`](https://rmtrane.github.io/ntrs/reference/is_npsych_scores.md)
  :

  Is `x` an npsych_scores object?

- [`remove_error_codes()`](https://rmtrane.github.io/ntrs/reference/remove_error_codes.md)
  : Remove error codes

- [`replace_codes()`](https://rmtrane.github.io/ntrs/reference/replace_codes.md)
  : Replace codes with their labels

## Standardization

Standardize scores.

- [`std()`](https://rmtrane.github.io/ntrs/reference/std.md) :
  Standardize neuropsychological test scores

- [`std_data()`](https://rmtrane.github.io/ntrs/reference/std_data.md) :

  Standardize all `npsych_scores` columns in a data frame

- [`std_using_norms()`](https://rmtrane.github.io/ntrs/reference/std_using_norms.md)
  :

  Standardize `npsych_scores` using norms

- [`std_using_regression()`](https://rmtrane.github.io/ntrs/reference/std_using_regression.md)
  : Standardize using regression

- [`std_using_norms-npsych_scores`](https://rmtrane.github.io/ntrs/reference/std_using_norms-npsych_scores.md)
  : Standardize test scores using norms

- [`std_using_regression-npsych_scores`](https://rmtrane.github.io/ntrs/reference/std_using_regression-npsych_scores.md)
  : Standardize test scores using regression

## Version Management

Register versions and manage defaults.

- [`std_version()`](https://rmtrane.github.io/ntrs/reference/std_version.md)
  : Standardization Version

- [`get_std_defaults()`](https://rmtrane.github.io/ntrs/reference/get_std_defaults.md)
  :

  Get standardization defaults `npsych_scores`

- [`set_std_defaults()`](https://rmtrane.github.io/ntrs/reference/set_std_defaults.md)
  :

  Set the default standardization method (and version) an
  `npsych_scores` subclass

## Norms Versions

Create and register norms-based standardization versions.

- [`norms_version()`](https://rmtrane.github.io/ntrs/reference/norms_version.md)
  : Norms-Based Standardization Version
- [`register_norms_version()`](https://rmtrane.github.io/ntrs/reference/register_norms_version.md)
  : Register a norms-based standardization version
- [`normative_summaries`](https://rmtrane.github.io/ntrs/reference/normative_summaries.md)
  : Means and SDs for calculating z-scores

## Regression Versions

Create and register regression-based standardization versions.

- [`regression_version()`](https://rmtrane.github.io/ntrs/reference/regression_version.md)
  : Regression-Based Standardization Version
- [`register_regression_version()`](https://rmtrane.github.io/ntrs/reference/register_regression_version.md)
  : Register a regression-based standardization version
- [`reg_coefs`](https://rmtrane.github.io/ntrs/reference/reg_coefs.md) :
  Regression Coefficients for Regression Based Standardization

## Discovery

List available scores, methods, and versions.

- [`list_npsych_scores()`](https://rmtrane.github.io/ntrs/reference/list_npsych_scores.md)
  :

  List `npsych_scores` subclasses

- [`list_std_methods()`](https://rmtrane.github.io/ntrs/reference/list_std_methods.md)
  :

  List available standardization methods for `npsych_scores`

- [`list_method_versions()`](https://rmtrane.github.io/ntrs/reference/list_method_versions.md)
  : Get available version names for a standardization method

- [`get_version_data()`](https://rmtrane.github.io/ntrs/reference/get_version_data.md)
  : Get version data

- [`get_npsych_scores()`](https://rmtrane.github.io/ntrs/reference/get_npsych_scores.md)
  : Get an npsych_scores constructor by name

## Score Calculators

Compute derived scores from raw inputs.

- [`calc_FAS()`](https://rmtrane.github.io/ntrs/reference/calc_FAS.md) :
  Calcaulate Functional Assessment Score summary
- [`calc_MOCACLOCK()`](https://rmtrane.github.io/ntrs/reference/calc_MOCACLOCK.md)
  : Calculate MoCA clock drawing test total
- [`calc_REYAREC()`](https://rmtrane.github.io/ntrs/reference/calc_REYAREC.md)
  : Calculate the Rey AVLT Accuracy
- [`calc_REYTOTAL()`](https://rmtrane.github.io/ntrs/reference/calc_REYTOTAL.md)
  : Calculate Rey Total

## Data & Reference

Demo data and reference resources.

- [`demo_data`](https://rmtrane.github.io/ntrs/reference/demo_data.md) :
  Demo Data
- [`rdd`](https://rmtrane.github.io/ntrs/reference/rdd.md) : Researchers
  Data Dictionary in List Form

## Test Score Constructors

Factory functions for individual neuropsychological test scores.

- [`ANIMALS()`](https://rmtrane.github.io/ntrs/reference/ANIMALS.md) :
  ANIMALS Test Scores
- [`BOSTON()`](https://rmtrane.github.io/ntrs/reference/BOSTON.md) :
  BOSTON Test Scores
- [`CDRGLOB()`](https://rmtrane.github.io/ntrs/reference/CDRGLOB.md) :
  CDRGLOB Test Scores
- [`CDRSUM()`](https://rmtrane.github.io/ntrs/reference/CDRSUM.md) :
  CDRSUM Test Scores
- [`CRAFTDRE()`](https://rmtrane.github.io/ntrs/reference/CRAFTDRE.md) :
  CRAFTDRE Test Scores
- [`CRAFTDVR()`](https://rmtrane.github.io/ntrs/reference/CRAFTDVR.md) :
  CRAFTDVR Test Scores
- [`CRAFTURS()`](https://rmtrane.github.io/ntrs/reference/CRAFTURS.md) :
  CRAFTURS Test Scores
- [`CRAFTVRS()`](https://rmtrane.github.io/ntrs/reference/CRAFTVRS.md) :
  CRAFTVRS Test Scores
- [`DIGBACCT()`](https://rmtrane.github.io/ntrs/reference/DIGBACCT.md) :
  DIGBACCT Test Scores
- [`DIGBACLS()`](https://rmtrane.github.io/ntrs/reference/DIGBACLS.md) :
  DIGBACLS Test Scores
- [`DIGFORCT()`](https://rmtrane.github.io/ntrs/reference/DIGFORCT.md) :
  DIGFORCT Test Scores
- [`DIGFORSL()`](https://rmtrane.github.io/ntrs/reference/DIGFORSL.md) :
  DIGFORSL Test Scores
- [`DIGIB()`](https://rmtrane.github.io/ntrs/reference/DIGIB.md) : DIGIB
  Test Scores
- [`DIGIBLEN()`](https://rmtrane.github.io/ntrs/reference/DIGIBLEN.md) :
  DIGIBLEN Test Scores
- [`DIGIF()`](https://rmtrane.github.io/ntrs/reference/DIGIF.md) : DIGIF
  Test Scores
- [`DIGIFLEN()`](https://rmtrane.github.io/ntrs/reference/DIGIFLEN.md) :
  DIGIFLEN Test Scores
- [`LOGIMEM()`](https://rmtrane.github.io/ntrs/reference/LOGIMEM.md) :
  LOGIMEM Test Scores
- [`MEMUNITS()`](https://rmtrane.github.io/ntrs/reference/MEMUNITS.md) :
  MEMUNITS Test Scores
- [`MINTTOTS()`](https://rmtrane.github.io/ntrs/reference/MINTTOTS.md) :
  MINTTOTS Test Scores
- [`MOCACLOCK()`](https://rmtrane.github.io/ntrs/reference/MOCACLOCK.md)
  : MOCACLOCK Test Scores
- [`MOCATOTS()`](https://rmtrane.github.io/ntrs/reference/MOCATOTS.md) :
  MOCATOTS Test Scores
- [`MOCBTOTS()`](https://rmtrane.github.io/ntrs/reference/MOCBTOTS.md) :
  MOCBTOTS Test Scores
- [`NACCGDS()`](https://rmtrane.github.io/ntrs/reference/NACCGDS.md) :
  NACCGDS Test Scores
- [`NACCMMSE()`](https://rmtrane.github.io/ntrs/reference/NACCMMSE.md) :
  NACCMMSE Test Scores
- [`OTRAILA()`](https://rmtrane.github.io/ntrs/reference/OTRAILA.md) :
  OTRAILA Test Scores
- [`OTRAILB()`](https://rmtrane.github.io/ntrs/reference/OTRAILB.md) :
  OTRAILB Test Scores
- [`OTRLARR()`](https://rmtrane.github.io/ntrs/reference/OTRLARR.md) :
  OTRLARR Test Scores
- [`OTRLBRR()`](https://rmtrane.github.io/ntrs/reference/OTRLBRR.md) :
  OTRLBRR Test Scores
- [`REY1REC()`](https://rmtrane.github.io/ntrs/reference/REY1REC.md) :
  REY1REC Test Scores
- [`REY2REC()`](https://rmtrane.github.io/ntrs/reference/REY2REC.md) :
  REY2REC Test Scores
- [`REY3REC()`](https://rmtrane.github.io/ntrs/reference/REY3REC.md) :
  REY3REC Test Scores
- [`REY4REC()`](https://rmtrane.github.io/ntrs/reference/REY4REC.md) :
  REY4REC Test Scores
- [`REY5REC()`](https://rmtrane.github.io/ntrs/reference/REY5REC.md) :
  REY5REC Test Scores
- [`REY6REC()`](https://rmtrane.github.io/ntrs/reference/REY6REC.md) :
  REY6REC Test Scores
- [`REYAREC()`](https://rmtrane.github.io/ntrs/reference/REYAREC.md) :
  REYAREC Test Scores
- [`REYDLIST()`](https://rmtrane.github.io/ntrs/reference/REYDLIST.md) :
  REYDLIST Test Scores
- [`REYDREC()`](https://rmtrane.github.io/ntrs/reference/REYDREC.md) :
  REYDREC Test Scores
- [`REYFPOS()`](https://rmtrane.github.io/ntrs/reference/REYFPOS.md) :
  REYFPOS Test Scores
- [`REYTCOR()`](https://rmtrane.github.io/ntrs/reference/REYTCOR.md) :
  REYTCOR Test Scores
- [`REYTOTAL()`](https://rmtrane.github.io/ntrs/reference/REYTOTAL.md) :
  REYTOTAL Test Scores
- [`TRAILA()`](https://rmtrane.github.io/ntrs/reference/TRAILA.md) :
  TRAILA Test Scores
- [`TRAILALI()`](https://rmtrane.github.io/ntrs/reference/TRAILALI.md) :
  TRAILALI Test Scores
- [`TRAILARR()`](https://rmtrane.github.io/ntrs/reference/TRAILARR.md) :
  TRAILARR Test Scores
- [`TRAILB()`](https://rmtrane.github.io/ntrs/reference/TRAILB.md) :
  TRAILB Test Scores
- [`TRAILBLI()`](https://rmtrane.github.io/ntrs/reference/TRAILBLI.md) :
  TRAILBLI Test Scores
- [`TRAILBRR()`](https://rmtrane.github.io/ntrs/reference/TRAILBRR.md) :
  TRAILBRR Test Scores
- [`UDSBENRS()`](https://rmtrane.github.io/ntrs/reference/UDSBENRS.md) :
  UDSBENRS Test Scores
- [`UDSBENTC()`](https://rmtrane.github.io/ntrs/reference/UDSBENTC.md) :
  UDSBENTC Test Scores
- [`UDSBENTD()`](https://rmtrane.github.io/ntrs/reference/UDSBENTD.md) :
  UDSBENTD Test Scores
- [`UDSVERFC()`](https://rmtrane.github.io/ntrs/reference/UDSVERFC.md) :
  UDSVERFC Test Scores
- [`UDSVERLC()`](https://rmtrane.github.io/ntrs/reference/UDSVERLC.md) :
  UDSVERLC Test Scores
- [`UDSVERTN()`](https://rmtrane.github.io/ntrs/reference/UDSVERTN.md) :
  UDSVERTN Test Scores
- [`VEG()`](https://rmtrane.github.io/ntrs/reference/VEG.md) : VEG Test
  Scores
- [`WAIS()`](https://rmtrane.github.io/ntrs/reference/WAIS.md) : WAIS
  Test Scores
