
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lmForc <a href='https://github.com/lucius-verus-fan/lmForc'><img src='logo/lmForc_hexSticker.png' align="right" height="200" /></a>

<!-- badges: start -->

![R build
status](https://github.com/lucius-verus-fan/lmForc/workflows/R-CMD-check/badge.svg)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-success.svg)](https://lifecycle.r-lib.org/articles/stages.html)
<!-- badges: end -->

<!-- [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/lmForc)](https://cran.r-project.org/package=lmForc) -->

The R package *lmForc* introduces functions for testing linear model
forecasts and a new class for working with forecast data: Forecast. Test
linear models out-of-sample by conditioning on realized values, vintage
forecasts, or lagged values. Create and test performance weighted
forecasts out-of-sample. Collect multiple forecasts and evaluate MSE or
RMSE. These functions are all built around the Forecast class which
matches the simplicity and interpretability of linear models.

## Vignette

For an overview of the *lmForc* package, please read the vignette:
[lmForc
Vignette](https://htmlpreview.github.io/?https://github.com/lucius-verus-fan/lmForc/blob/main/doc/lmForc.html)

## Installation

To install the **stable** version from
[CRAN](https://cran.r-project.org/package=lmForc):

``` r
Coming Soon
```

To install the **development** version from
[GitHub](https://github.com/lucius-verus-fan/lmForc):

``` r
# install.packages("remotes")
remotes::install_github("lucius-verus-fan/lmForc")
```

## Example

Produce an out-of-sample forecast conditioned on realized values, then
calculate the RMSE of the forecast.

``` r
library(lmForc)
forecast <- oos_realized_forc(
  lm_call = lm(y ~ x1 + x2, data),
  h_ahead = 2L,
  estimation_end = as.Date("2011-03-31"),
  time_vec = data$date
)
forecast
#> h_ahead = 2 
#> 
#>       origin     future forecast realized
#> 1 2011-03-31 2011-09-30 1.623750     2.89
#> 2 2011-06-30 2011-12-31 2.341664     2.11
#> 3 2011-09-30 2012-03-31 3.415198     2.97
#> 4 2011-12-31 2012-06-30 2.708308     0.99
rmse(forecast)
#> [1] 1.09634
```
