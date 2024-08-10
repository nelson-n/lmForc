
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lmForc <a href='https://github.com/nelson-n/lmForc/blob/main/vignettes/logo/lmForc_hexSticker.png'><img src='/Users/nelsonrayl/Desktop/init/lmForc/lmForc/vignettes/logo/lmForc_hexSticker.png' align="right" height="200" /></a>

<!-- badges: start -->

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/lmForc?color=brightgreen)](https://cran.r-project.org/package=lmForc)
[![CRAN Version](https://www.r-pkg.org/badges/version/lmForc)](https://www.r-pkg.org/pkg/lmForc)
[![CRAN Posit mirror downloads](https://cranlogs.r-pkg.org/badges/lmForc)](https://www.r-pkg.org/pkg/lmForc)
[![Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/lmForc?color=blue)](https://cran.r-project.org/package=lmForc)

[![R build status](https://github.com/nelson-n/lmForc/workflows/R-CMD-check/badge.svg)](https://github.com/nelson-n/lmForc/actions)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-success.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check](https://github.com/nelson-n/lmForc/workflows/R-CMD-check/badge.svg)](https://github.com/nelson-n/lmForc/actions)
<!-- badges: end -->






<!-- [![Monthly Downloads](http://cranlogs.r-pkg.org/badges/lmForc?color=blue)](https://cran.r-project.org/package=lmForc) -->

The R package *lmForc* introduces functions for testing forecasting
models and a new class for working with forecast data: `Forecast`. Test
models out-of-sample by conditioning on realized values, vintage
forecasts, or lagged values. Benchmark against AR models, historical
average forecasts, or random walk forecasts. Create performance weighted
or states weighted forecast combinations. These functions are built
around the `Forecast` class and support both linear forecasting models
and more complex models such as logistic regression, tree based models,
or neural networks.

## Vignette

For an overview of the *lmForc* package, please read the vignette:
[lmForc
Vignette](https://cran.r-project.org/web/packages/lmForc/vignettes/lmForc.html)

<a href='https://cran.r-project.org/web/packages/lmForc/vignettes/lmForc.html'><img src='/Users/nelsonrayl/Desktop/init/lmForc/lmForc/vignettes/vignette_demo.png' align="center" height="220" /></a>

## Paper

Accompanying Paper: [lmForc: Linear Model Forecasting in
R](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4130453)

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

## Linear Forecasting Model Example

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

# Linear model out-of-sample forecast.
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

## General Forecasting Model Example

Produces an out-of-sample forecast conditioned on realized values
similar to the example above, but does so using a logistic regression.
Functions with `_general` in the name are designed to work with any
model that can be estimated in R, including tree based models, neural
networks, or models with hand-built parameters. The user only needs to
specify a `model_function` that estimates the model and the
`prediction_function` which produces predictions from the model.

``` r
# Stylized Dataset.
date <- as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
                  "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", 
                  "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31",
                  "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31"))
y  <- c(1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0)
x1 <- c(8.22, 3.86, 4.27, 3.37, 5.88, 3.34, 2.92, 1.80, 3.30, 7.17, 3.22, 3.86, 4.27, 3.37, 5.88, 3.34)
x2 <- c(4.03, 2.46, 2.04, 2.44, 6.09, 2.91, 1.68, 2.91, 3.87, 1.63, 4.03, 2.46, 2.04, 2.44, 6.09, 2.91)
dataLogit <- data.frame(date, y, x1, x2)

# Logit model out-of-sample forecast.
forecast3 <- oos_realized_forc_general(
    model_function = function(data) {glm(y ~ x1 + x2, data = data, family = binomial)},
    prediction_function = function(model_function, data) {as.vector(predict(model_function, data, type = "response"))}, 
    data = dataLogit,
    realized = dataLogit$y,
    h_ahead = 2L,
    estimation_end = as.Date("2012-06-30"),
    time_vec = dataLogit$date,
    estimation_window = NULL
)

forecast3
#> h_ahead = 2 
#> 
#>       origin     future   forecast realized
#> 1 2012-06-30 2012-12-31 0.20301888        0
#> 2 2012-09-30 2013-03-31 0.24452583        0
#> 3 2012-12-31 2013-06-30 0.07931267        1
#> 4 2013-03-31 2013-09-30 0.98707714        1
#> 5 2013-06-30 2013-12-31 0.18387762        0
```
