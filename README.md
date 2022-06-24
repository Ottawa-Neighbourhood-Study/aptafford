
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aptafford

<!-- badges: start -->
<!-- badges: end -->

Functions for collecting and analyzing apartment affordability data.

## Installation

You can install the development version of aptafford like so:

``` r
# from github, tbc
```

## Example

One function to collect current apartment listings in Ottawa, Ontario:

``` r
library(aptafford)

apartments <- aptafford::padmapper_scrape()
```
