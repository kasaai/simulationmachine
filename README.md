
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simulationmachine

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/kasaai/simulationmachine.svg?branch=master)](https://travis-ci.org/kasaai/simulationmachine)
<!-- badges: end -->

This package implements the claims history simulation algorithm detailed
in [*An Individual Claims History Simulation
Machine*](https://www.mdpi.com/2227-9091/6/2/29) by Andrea Gabrielli and
Mario V. Wüthrich. The goal is to provide an easy-to-use interface for
generating claims data that can be used for loss reserving research.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("kasaai/simulationmachine")
```

## Example

First, we can specify the parameters of a simulation using
`simulation_machine()`:

``` r
library(simulationmachine)

charm <- simulation_machine(
  num_claims = 50000, 
  lob_distribution = c(0.25, 0.25, 0.30, 0.20), 
  inflation = c(0.01, 0.01, 0.01, 0.01), 
  sd_claim = 0.85, 
  sd_recovery = 0.85
)

charm
#> A simulation charm for `simulation_machine`
#> 
#> Each record is:
#>  - A snapshot of a claim's incremental paid loss and claim status
#>    at a development year.
#> 
#> Specs:
#>  - Expected number of claims: 50,000
#>  - LOB distribution: 0.25, 0.25, 0.3, 0.2
#>  - Inflation: 0.01, 0.01, 0.01, 0.01
#>  - SD of claim sizes: 0.85,
#>  - SD of recovery sizes: 0.85
```

Once we have the charm object, we can use `conjure()` to perform the
simulation.

``` r
library(dplyr)
records <- conjure(charm, seed = 100)
glimpse(records)
#> Observations: 603,324
#> Variables: 11
#> $ claim_id          <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "…
#> $ accident_year     <int> 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994…
#> $ development_year  <int> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 0, 1, 2,…
#> $ accident_quarter  <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 4, 4…
#> $ report_delay      <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ lob               <chr> "3", "3", "3", "3", "3", "3", "3", "3", "3", "…
#> $ cc                <chr> "42", "42", "42", "42", "42", "42", "42", "42"…
#> $ age               <int> 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65…
#> $ injured_part      <chr> "51", "51", "51", "51", "51", "51", "51", "51"…
#> $ paid_loss         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4913, 0, 0…
#> $ claim_status_open <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0…
```

Let’s see how many claims we drew:

``` r
records %>% 
  distinct(claim_id) %>% 
  count()
#> # A tibble: 1 x 1
#>       n
#>   <int>
#> 1 50277
```

If you prefer to have each row of the dataset to correspond to a claim,
you can simply pivot the data with tidyr:

``` r
records_wide <- records %>% 
  tidyr::pivot_wider(
    names_from = development_year, 
    values_from = c(paid_loss, claim_status_open),
    values_fill = list(paid_loss = 0)
  )

glimpse(records_wide)
#> Observations: 50,277
#> Variables: 32
#> $ claim_id             <chr> "1", "2", "3", "4", "5", "6", "7", "8", "9"…
#> $ accident_year        <int> 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1…
#> $ accident_quarter     <dbl> 1, 4, 4, 2, 1, 1, 1, 3, 4, 4, 2, 2, 4, 3, 1…
#> $ report_delay         <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0…
#> $ lob                  <chr> "3", "3", "3", "4", "2", "1", "3", "1", "3"…
#> $ cc                   <chr> "42", "39", "26", "8", "50", "22", "43", "1…
#> $ age                  <int> 65, 52, 23, 54, 24, 53, 39, 40, 27, 43, 55,…
#> $ injured_part         <chr> "51", "53", "70", "36", "36", "53", "51", "…
#> $ paid_loss_0          <dbl> 0, 4913, 0, 458, 1158, 376, 0, 285, 0, 0, 0…
#> $ paid_loss_1          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 3389, 0, 0, 0, 0…
#> $ paid_loss_2          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ paid_loss_3          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ paid_loss_4          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ paid_loss_5          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ paid_loss_6          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ paid_loss_7          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ paid_loss_8          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ paid_loss_9          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ paid_loss_10         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ paid_loss_11         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ claim_status_open_0  <int> 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0…
#> $ claim_status_open_1  <int> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0…
#> $ claim_status_open_2  <int> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ claim_status_open_3  <int> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ claim_status_open_4  <int> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ claim_status_open_5  <int> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ claim_status_open_6  <int> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ claim_status_open_7  <int> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ claim_status_open_8  <int> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ claim_status_open_9  <int> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ claim_status_open_10 <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ claim_status_open_11 <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
```

-----

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/kasaai/community/blob/master/CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.
