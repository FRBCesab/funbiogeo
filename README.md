
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `funbiogeo` - helping with functional biogeography

<!-- badges: start -->

[![R-CMD-check](https://github.com/FRBCesab/funbiogeo/workflows/R-CMD-check/badge.svg)](https://github.com/FRBCesab/funbiogeo/actions)
[![codecov](https://codecov.io/gh/FRBCesab/funbiogeo/branch/main/graph/badge.svg?token=JPXXVNMAJ0)](https://codecov.io/gh/FRBCesab/funbiogeo)
[![CRAN
status](https://www.r-pkg.org/badges/version/funbiogeo)](https://CRAN.R-project.org/package=funbiogeo)
[![License: GPL
v2](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html)
[![LifeCycle](man/figures/lifecycle-experimental.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

## Overview

`funbiogeo` aims to be a package to help with analyses in functional
biogeography (Violle et al. 2014).

`funbiogeo` helps to load and combine data, computing trait coverage, as
well as computing functional diversity indices, drawing maps, correlate
them with the environment, and upscaling assemblages. To know what
features are planned in the future you can refer to our [Roadmap wiki
page](https://github.com/FRBCesab/funbiogeo/wiki/Roadmap).

## Installation

For the moment `funbiogeo` is not on CRAN so you have to install it from
GitHub.

``` r
# install.packages("remotes")  # Run this line if 'remotes' pkg is not installed
remotes::install_github("FRBCesab/funbiogeo")
```

## Getting started

Please refer to the [“Getting started”
vignette](https://frbcesab.github.io/funbiogeo/).

## Related packages

To understand better our working philosophy you can refer to our [Design
principles wiki
page](https://github.com/FRBCesab/funbiogeo/wiki/Design-Principles).

`funbiogeo` relies on other packages to provide critical features:

  - [`fundiversity`](https://github.com/Bisaloo/fundiversity) computes
    functional diversity indices.
  - [`funrar`](https://github.com/Rekyt/funrar) computes functional
    originality indices.
  - [`mFD`](https://github.com/CmlMagneville/mFD) computes functional
    diversity indices.

## References

Violle, C., Reich, P. B., Pacala, S. W., Enquist, B. J., & Kattge, J.
(2014). The emergence and promise of functional biogeography.
Proceedings of the National Academy of Sciences, 111(38), 13690–13696.
<https://doi.org/10.1073/pnas.1415442111>.

## Code of Conduct

Please note that the `funbiogeo` project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
