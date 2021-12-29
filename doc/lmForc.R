## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(lmForc)

## -----------------------------------------------------------------------------
my_forecast <- Forecast(
   origin   = as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31")),
   future   = as.Date(c("2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31")),
   forecast = c(4.21, 4.27, 5.32, 5.11),
   realized = c(4.40, 4.45, 4.87, 4.77),
   h_ahead  = 4L
)

## -----------------------------------------------------------------------------
mse(my_forecast)
rmse(my_forecast)
mae(my_forecast)
mape(my_forecast)
R2(my_forecast)

## -----------------------------------------------------------------------------
forc2df(my_forecast)

origin(my_forecast)

future(my_forecast)

forc(my_forecast)

realized(my_forecast)

## -----------------------------------------------------------------------------
date <- as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
                  "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", 
                  "2012-03-31", "2012-06-30"))

y  <- c(1.09, 1.71, 1.09, 2.46, 1.78, 1.35, 2.89, 2.11, 2.97, 0.99)

x1 <- c(4.22, 3.86, 4.27, 5.60, 5.11, 4.31, 4.92, 5.80, 6.30, 4.17)

x2  <- c(10.03, 10.49, 10.85, 10.47, 9.09, 10.91, 8.68, 9.91, 7.87, 6.63)

data <- data.frame(date, y, x1, x2)

head(data)

## -----------------------------------------------------------------------------
is_forc(
  lm_call  = lm(y ~ x1 + x2, data),
  time_vec = data$date
)

## -----------------------------------------------------------------------------
oos_realized_forc(
  lm_call = lm(y ~ x1 + x2, data),
  h_ahead = 2L,
  estimation_end = as.Date("2011-03-31"),
  time_vec = data$date,
  estimation_window = NULL,
  return_betas = FALSE
)

## -----------------------------------------------------------------------------
oos_lag_forc(
  lm_call = lm(y ~ x1 + x2, data),
  h_ahead = 2L,
  estimation_end = as.Date("2011-03-31"),
  time_vec = data$date,
  estimation_window = NULL,
  return_betas = FALSE
)

## -----------------------------------------------------------------------------
x1_forecast_vintage <- Forecast(
   origin   = as.Date(c("2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30")),
   future   = as.Date(c("2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30")),
   forecast = c(6.30, 4.17, 5.30, 4.84),
   realized = c(4.92, 5.80, 6.30, 4.17),
   h_ahead  = 4L
)

x2_forecast_vintage <- Forecast(
   origin   = as.Date(c("2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30")),
   future   = as.Date(c("2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30")),
   forecast = c(7.32, 6.88, 6.82, 6.95),
   realized = c(8.68, 9.91, 7.87, 6.63),
   h_ahead  = 4L
)

oos_vintage_forc(
  lm_call = lm(y ~ x1 + x2, data),
  time_vec = data$date,
  x1_forecast_vintage, x2_forecast_vintage,
  estimation_window = NULL,
  return_betas = FALSE
)

## -----------------------------------------------------------------------------
x1_forecast <- Forecast(
   origin   = as.Date(c("2012-06-30", "2012-06-30", "2012-06-30", "2012-06-30")),
   future   = as.Date(c("2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30")),
   forecast = c(4.14, 4.04, 4.97, 5.12),
   realized = NULL,
   h_ahead  = NULL
)

x2_forecast <- Forecast(
   origin   = as.Date(c("2012-06-30", "2012-06-30", "2012-06-30", "2012-06-30")),
   future   = as.Date(c("2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30")),
   forecast = c(6.01, 6.05, 6.55, 7.45),
   realized = NULL,
   h_ahead  = NULL
)

conditional_forc(
  lm_call = lm(y ~ x1 + x2, data),
  time_vec = data$date,
  x1_forecast, x2_forecast
)

## -----------------------------------------------------------------------------
historical_average_forc(
  avg_function = "mean",
  realized_vec = data$y,
  h_ahead = 2L,
  estimation_end = as.Date("2011-03-31"),
  time_vec = data$date,
  estimation_window = 4L
)

## -----------------------------------------------------------------------------
random_walk_forc(
  realized_vec = data$y,
  h_ahead = 6L,
  time_vec = data$date 
)

## -----------------------------------------------------------------------------
autoreg_forc(
  realized_vec = data$y,
  h_ahead = 2L,
  ar_lags = 2L,
  estimation_end = as.Date("2011-06-30"),
  time_vec = data$date,
  estimation_window = NULL,
  return_betas = FALSE
)

## -----------------------------------------------------------------------------
y1_forecast <- Forecast(
  origin = as.Date(c("2009-03-31", "2009-06-30", "2009-09-30", "2009-12-31",
                     "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", 
                     "2011-03-31", "2011-06-30")),
  future = as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
                     "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", 
                     "2012-03-31", "2012-06-30")),
  forecast = c(1.33, 1.36, 1.38, 1.68, 1.60, 1.55, 1.32, 1.22, 1.08, 0.88),
  realized = c(1.09, 1.71, 1.09, 2.46, 1.78, 1.35, 2.89, 2.11, 2.97, 0.99),
  h_ahead = 4L
)

y2_forecast <- Forecast(
  origin = as.Date(c("2009-03-31", "2009-06-30", "2009-09-30", "2009-12-31",
                     "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", 
                     "2011-03-31", "2011-06-30")),
  future = as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
                     "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", 
                     "2012-03-31", "2012-06-30")),
  forecast = c(0.70, 0.88, 1.03, 1.05, 1.01, 0.82, 0.95, 1.09, 1.07, 1.06),
  realized = c(1.09, 1.71, 1.09, 2.46, 1.78, 1.35, 2.89, 2.11, 2.97, 0.99),
  h_ahead = 4L
)

mse_weighted_forc(
  y1_forecast, y2_forecast,
  eval_window = 2L,
  errors = "mse",
  return_weights = FALSE
)

## -----------------------------------------------------------------------------
date <- as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
                  "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31",
                  "2012-03-31", "2012-06-30"))

future <- as.Date(c("2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31",
                    "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31",
                    "2013-03-31", "2013-06-30"))

y  <- c(1.09, 1.71, 1.09, 2.46, 1.78, 1.35, 2.89, 2.11, 2.97, 0.99)
x1 <- c(4.22, 3.86, 4.27, 5.60, 5.11, 4.31, 4.92, 5.80, 6.30, 4.17)
x2 <- c(10.03, 10.49, 10.85, 10.47, 9.09, 10.91, 8.68, 9.91, 7.87, 6.63)

data <- data.frame(date, y, x1, x2)
matching_vars <- data[, c("x1", "x2")]

y1_forecast <- Forecast(
  origin = date,
  future = future,
  forecast = c(1.33, 1.36, 1.38, 1.68, 1.60, 1.55, 1.32, 1.22, 1.08, 0.88),
  realized = c(1.78, 1.35, 2.89, 2.11, 2.97, 0.99, 1.31, 1.41, 1.02, 1.05),
  h_ahead = 4L
)

y2_forecast <- Forecast(
  origin = date,
  future = future,
  forecast = c(0.70, 0.88, 1.03, 1.05, 1.01, 0.82, 0.95, 1.09, 1.07, 1.06),
  realized = c(1.78, 1.35, 2.89, 2.11, 2.97, 0.99, 1.31, 1.41, 1.02, 1.05),
  h_ahead = 4L
)

states_weighted_forc(
  y1_forecast, y2_forecast,
  matching_vars = matching_vars,
  time_vec = data$date,
  matching_window = 2L,
  matching = "euclidean",
  errors = "mse",
  return_weights = FALSE
)

