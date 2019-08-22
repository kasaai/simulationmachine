
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simulationmachine

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/kasaai/simulationmachine.svg?branch=master)](https://travis-ci.org/kasaai/simulationmachine)
<!-- badges: end -->

**NOTE: In early stages of development – do not use yet\!**

This package implements the claims history simulation algorithm detailed
in [*An Individual Claims History Simulation
Machine*](https://www.mdpi.com/2227-9091/6/2/29) by Andrea Gabrielli and
Mario V. Wüthrich. The goal is to provide an easy-to-use interface for
generating claims data that can be used for loss reserving research.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("kasaai/conjuror")
devtools::install_github("kasaai/simulationmachine")
```

## Example

First, we can specify the parameters of a simulation using
`simulation_machine()`:

``` r
library(simulationmachine)

incantation <- simulation_machine(
  num_records = 50000, 
  lob_distribution = c(0.25, 0.25, 0.30, 0.20), 
  inflation = c(0.01, 0.01, 0.01, 0.01), 
  sd_claim = 0.85, 
  sd_recovery = 0.85
)

incantation
#> A simulation incantation for `simulation_machine`
#> 
#> Specs:
#>  - Expected number of claims: 50,000
#>  - LOB distribution: 0.25, 0.25, 0.3, 0.2
#>  - Inflation: 0.01, 0.01, 0.01, 0.01
#>  - SD of claim sizes: 0.85,
#>  - SD of recovery sizes: 0.85
```

Once we have the incantation object, we can use `conjure()` to perform
the simulation.

``` r
records <- conjure(incantation, seed = 100)
dplyr::glimpse(records)
#> Observations: 50,277
#> Variables: 32
#> $ ClNr     <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, …
#> $ LoB      <fct> 3, 3, 3, 4, 2, 1, 3, 1, 3, 3, 2, 1, 3, 4, 1, 2, 2, 1, 1…
#> $ cc       <fct> 42, 39, 26, 8, 50, 22, 43, 17, 21, 17, 51, 20, 31, 51, …
#> $ AY       <dbl> 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1…
#> $ AQ       <dbl> 1, 4, 4, 2, 1, 1, 1, 3, 4, 4, 2, 2, 4, 3, 1, 3, 2, 3, 2…
#> $ age      <dbl> 65, 52, 23, 54, 24, 53, 39, 40, 27, 43, 55, 33, 16, 19,…
#> $ inj_part <fct> 51, 53, 70, 36, 36, 53, 51, 54, 43, 13, 54, 35, 51, 37,…
#> $ RepDel   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0…
#> $ Pay00    <dbl> 0, 4913, 0, 458, 1158, 376, 0, 285, 0, 0, 0, 256, 0, 29…
#> $ Pay01    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 3389, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ Pay02    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ Pay03    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ Pay04    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ Pay05    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ Pay06    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ Pay07    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ Pay08    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ Pay09    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ Pay10    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ Pay11    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ Open00   <dbl> 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0…
#> $ Open01   <dbl> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0…
#> $ Open02   <dbl> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ Open03   <dbl> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ Open04   <dbl> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ Open05   <dbl> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ Open06   <dbl> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ Open07   <dbl> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ Open08   <dbl> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ Open09   <dbl> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ Open10   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ Open11   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
```
