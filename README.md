
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lmForc <a href='https://github.com/nelson-n/lmForc/blob/main/vignettes/logo/lmForc_hexSticker.png'><img src='/vignettes/logo/lmForc_hexSticker.png' align="right" height="200" /></a>

<!-- badges: start -->

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/lmForc?color=brightgreen)](https://cran.r-project.org/package=lmForc)
[![Total
Downloads](http://cranlogs.r-pkg.org/badges/grand-total/lmForc?color=blue)](https://cran.r-project.org/package=lmForc)
[![R build
status](https://github.com/nelson-n/lmForc/workflows/R-CMD-check/badge.svg)](https://github.com/nelson-n/lmForc/actions)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-success.svg)](https://lifecycle.r-lib.org/articles/stages.html)
<!-- badges: end -->

<!-- [![Monthly Downloads](http://cranlogs.r-pkg.org/badges/lmForc?color=blue)](https://cran.r-project.org/package=lmForc) -->

The R package *lmForc* introduces functions for testing linear model
forecasts and a new class for working with forecast data: Forecast. Test
linear models out-of-sample by conditioning on realized values, vintage
forecasts, or lagged values. Benchmark against AR models, historical
average forecasts, or random walk forecasts. Create performance weighted
or states weighted forecast combinations. These functions are built
around the Forecast class which matches the simplicity and
interpretability of linear models.

## Vignette

For an overview of the *lmForc* package, please read the vignette:
[lmForc
Vignette](https://cran.r-project.org/web/packages/lmForc/vignettes/lmForc.html)

<a href='https://cran.r-project.org/web/packages/lmForc/vignettes/lmForc.html'><img src='/vignettes/vignette_demo.png' align="center" height="220" /></a>

## Paper

Pre-print version of [lmForc: Linear Model Forecasting in
R](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4130453),
submitted to the [R Journal](https://journal.r-project.org/).

**Abstract**: Linear forecasting models are a popular option in many
domains due to their simplicity and interpretability. This paper
considers the tools a forecaster has for evaluating linear forecasting
models and presents lmForc, a package which implements these evaluation
functions. Functions in the lmForc package are built around a new S4
class, Forecast, which introduces a dedicated data structure for storing
forecasts to the R language and permits the creation of complex
forecasting functions. The lmForc package is designed to leverage the
simplicity and interpretability of linear models so that a forecaster
may easily test model specifications and understand precisely how a
model arrives at a forecast.

## Installation

To install the **stable** version from
[CRAN](https://cran.r-project.org/package=lmForc):

``` r
install.packages("lmForc")
```

To install the **development** version from
[GitHub](https://github.com/nelson-n/lmForc):

``` r
# install.packages("remotes")
remotes::install_github("nelson-n/lmForc")
```

## lmForc Helper Functions

Additional helper functions for working with lmForc `Forecast` objects
are available in the subdirectory
[**/lmForc_helpers**](https://github.com/nelson-n/lmForc/tree/main/lmForc_helpers).
These extension functions include `subset_identical()` which subsets a
list of forecasts to an identical sample period and `transform_bytime()`
which converts forecasts to *Time Format*. These functions can be
accessed by cloning the scripts *lmForc_subset.R*, *lmForc_transform.R*,
and *lmForc_visualize.R* from the **/lmForc_helpers** directory and
sourcing them in R.

``` r
source("lmForc_subset.R")
source("lmForc_transform.R")
source("lmForc_visualize.R"
```

## Examples

Produce an out-of-sample forecast conditioned on realized values.
Calculates linear model coefficients in each period based on information
that would have been available to the forecaster. Coefficients are
combined with future realized values to compute a conditional forecast.
Evaluates the performance of a linear model had it been conditioned on
perfect information.

``` r
library(lmForc)

# Stylized dataset.
date <- as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
                  "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", 
                  "2012-03-31", "2012-06-30"))
y    <- c(1.09, 1.71, 1.09, 2.46, 1.78, 1.35, 2.89, 2.11, 2.97, 0.99)
x1   <- c(4.22, 3.86, 4.27, 5.60, 5.11, 4.31, 4.92, 5.80, 6.30, 4.17)
x2   <- c(10.03, 10.49, 10.85, 10.47, 9.09, 10.91, 8.68, 9.91, 7.87, 6.63)
data <- data.frame(date, y, x1, x2)

# Out-of-sample forecast.
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

Produce an out-of-sample forecast based on the historical median. In
each period the historical median of the series is calculated based on
information that would have been available to the forecaster. Replicates
the historical median forecast that would have been produced in
real-time and serves as a benchmark for other models.

``` r
# Historical Median Forecast
forecast2 <- historical_average_forc(
  avg_function = "median",
  realized_vec = data$y,
  h_ahead = 2L,
  estimation_end = as.Date("2011-03-31"),
  time_vec = data$date
)

forecast2
#> h_ahead = 2 
#> 
#>       origin     future forecast realized
#> 1 2011-03-31 2011-09-30    1.710     2.89
#> 2 2011-06-30 2011-12-31    1.530     2.11
#> 3 2011-09-30 2012-03-31    1.710     2.97
#> 4 2011-12-31 2012-06-30    1.745     0.99
```

Compare the performance of both models by calculating RMSE forecast
error.

``` r
rmse(forecast1)
#> [1] 1.09634
rmse(forecast2)
#> [1] 0.9857009
```

Create a performance weighted forecast combination of `forecast1` and
`forecast2`. In each period forecast accuracy is calculated over recent
periods and each model is given a weight based on recent accuracy. The
forecast for the next period is calculated as a weighted combination of
both forecasts.

``` r
performance_weighted_forc(
  forecast1, forecast2,
  eval_window = 1L,
  errors = "mse",
  return_weights = FALSE
)
#> h_ahead = 2 
#> 
#>       origin     future forecast realized
#> 1 2011-03-31 2011-09-30       NA     2.89
#> 2 2011-06-30 2011-12-31       NA     2.11
#> 3 2011-09-30 2012-03-31 2.502552     2.97
#> 4 2011-12-31 2012-06-30 2.575770     0.99
```
