
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lmForc <a href='https://github.com/lucius-verus-fan/lmForc/blob/main/vignettes/logo/lmForc_hexSticker.png'><img src='/vignettes/logo/lmForc_hexSticker.png' align="right" height="200" /></a>

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

Produce an out-of-sample forecast conditioned on realized values. This
test calculates linear model coefficients in each period based on
information that would have been available to the forecaster. These
coefficients are then combined with future realized values to compute a
forecast. Evaluates the performance of a linear forecasting model had it
been given perfect information.

``` r
library(lmForc)

forecast1 <- oos_realized_forc(
  lm_call = lm(y ~ x1 + x2, data),
  h_ahead = 2L,
  estimation_end = as.Date("2011-03-31"),
  time_vec = data$date
)

forecast1
#> h_ahead = 2 
#> 
#>       origin     future forecast realized
#> 1 2011-03-31 2011-09-30 1.623750     2.89
#> 2 2011-06-30 2011-12-31 2.341664     2.11
#> 3 2011-09-30 2012-03-31 3.415198     2.97
#> 4 2011-12-31 2012-06-30 2.708308     0.99
```

Produce an out-of-sample forecast based on the historical mean. In each
period the historical mean of the series is calculated based on
information that would have been available to the forecaster and set as
the forecast. Replicates the historical mean forecast that would have
been produced in real time and serves as a benchmark for other models.

``` r
forecast2 <- historical_mean_forc(
  realized_vec = data$y,
  h_ahead = 2L,
  estimation_end = as.Date("2011-03-31"),
  time_vec = data$date
)

forecast2
#> h_ahead = 2 
#> 
#>       origin     future forecast realized
#> 1 2011-03-31 2011-09-30 1.626000     2.89
#> 2 2011-06-30 2011-12-31 1.580000     2.11
#> 3 2011-09-30 2012-03-31 1.767143     2.97
#> 4 2011-12-31 2012-06-30 1.810000     0.99
```

Compare the performance of both models by calculating RMSE forecast
error.

``` r
rmse(forecast1)
#> [1] 1.09634
rmse(forecast2)
#> [1] 0.9997326
```
