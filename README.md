
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aptafford

<!-- badges: start -->

[![R-CMD-check](https://github.com/Ottawa-Neighbourhood-Study/aptafford/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Ottawa-Neighbourhood-Study/aptafford/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Functions for collecting and analyzing apartment affordability data.

## Installation

You can install the development version of aptafford like so:

``` r
devtools::install_github("Ottawa-Neighbourhood-Study/aptafford")
```

## Example

One function to collect current apartment listings in Ottawa, Ontario:

``` r
library(aptafford)

apartments <- aptafford::padmapper_scrape()
```
