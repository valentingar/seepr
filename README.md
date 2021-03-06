
<!-- README.md is generated from README.Rmd. Please edit that file -->

# seepR

<!-- badges: start -->
<!-- badges: end -->

Calculate seepage flux from temperature probes.

This package is still under development and not yet stable. Please
contact valentin.gartiser <at> student.uni-tuebingen.de for any
questions. ## Installation

You can install the development version of seepr from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("valentingar/seepr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(seepr)
mydat %>%
  seepr_dat(depths = mydepths) %>%
  fit_dhr(s = 24*60) %>% # minute data
  flux(n = 0.39, 
       Cscal = 0.5,
       Cwcal = 1,
       Kcal = 0.0058,
       method = "keery_amplitude")
## basic example code
```
