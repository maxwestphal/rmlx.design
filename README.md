
<!-- README.md is generated from README.Rmd. Please edit that file -->

# {rmlx.design}- Structured data splitting to facilitate estimand-aligned performance estimation in machine learning (R package)

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![](https://img.shields.io/badge/devel%20version-0.2.1-blue.svg)](https://github.com/maxwestphal/rmlx.design)
[![](https://www.r-pkg.org/badges/version/rmlx.design?color=orange)](https://cran.r-project.org/package=rmlx.design)
[![R-CMD-check](https://github.com/maxwestphal/rmlx.design/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/maxwestphal/rmlx.design/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/maxwestphal/rmlx.design/branch/main/graph/badge.svg)](https://app.codecov.io/gh/maxwestphal/rmlx.design?branch=main)
<!-- badges: end -->

The {rmlx.design} package allows structured data splitting for
supervised machine learning tasks. The data splitting in {rmlx.design}
is based on an **estimand** definition which is in turn based on
user-specified constraints.

**Constraints** can be thought of as generalized inclusion/exclusion
criteria for

1.  Test observations
2.  The relation between observations in training and test sets
3.  The training dataset(s)

In contrast to traditional techniques (hold-out, cross-validation,
bootstrap), this leads to a deterministic data splitting. The intention
behind structured data splitting is to allow estimation of
**estimand-aligned transferabiliy (out-of-distribution generalization)**
instead of **reproducibility (in distribution generalization)** (Alpers
& Westphal, 2025).

## Installation

You can install the development version of {rmlx.design} from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("maxwestphal/rmlx.design")
```

## Getting started

An overview over important functions and classes and a minimal example
can be found in the “overview” vignette:

``` r
library(rmlx.design)
vignette("overview", "rmlx.design")
```

## Roadmap

Planned features for future versions include:

1.  Extension of traditional (unstructured) data splitting methods
2.  Export of data splitting schemes to popular ML packages
3.  Improved summary/visualization functions for derived data splits

## References

- Alpers, R. and Westphal, M. 2025. An estimand framework to guide model
  and algorithm validation in predictive modelling. Submitted for
  publication.
