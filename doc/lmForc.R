## ----include = FALSE----------------------------------------------------------
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
date <- as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
                  "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", 
                  "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31",
                  "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31"))
y  <- c(1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0)
x1 <- c(8.22, 3.86, 4.27, 3.37, 5.88, 3.34, 2.92, 1.80, 3.30, 7.17, 3.22, 3.86, 4.27, 3.37, 5.88, 3.34)
x2 <- c(4.03, 2.46, 2.04, 2.44, 6.09, 2.91, 1.68, 2.91, 3.87, 1.63, 4.03, 2.46, 2.04, 2.44, 6.09, 2.91)
dataLogit <- data.frame(date, y, x1, x2)

head(dataLogit)

## -----------------------------------------------------------------------------
is_forc(
  lm_call  = lm(y ~ x1 + x2, data),
  time_vec = data$date
)

## -----------------------------------------------------------------------------
is_forc_general(
  model_function = function(data) {glm(y ~ x1 + x2, data = data, family = binomial)},
  prediction_function = function(model_function, data) {as.vector(predict(model_function, data, type = "response"))}, 
  data = dataLogit,
  realized = dataLogit$y,
  time_vec = dataLogit$date
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
forc <- oos_realized_forc_general(
    model_function = function(data) {glm(y ~ x1 + x2, data = data, family = binomial)},
    prediction_function = function(model_function, data) {
      as.vector(predict(model_function, data, type = "response"))
    }, 
    data = dataLogit,
    realized = dataLogit$y,
    h_ahead = 2L,
    estimation_end = as.Date("2012-06-30"),
    time_vec = dataLogit$date,
    estimation_window = NULL
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
x1_forecast_vintageLogit <- Forecast(
   origin   = as.Date(c("2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30")),
   future   = as.Date(c("2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30")),
   forecast = c(6.34, 4.17, 2.98, 1.84),
   realized = c(5.88, 3.34, 2.92, 1.80),
   h_ahead  = 4L
)

x2_forecast_vintageLogit <- Forecast(
   origin   = as.Date(c("2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30")),
   future   = as.Date(c("2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30")),
   forecast = c(7.32, 3.22, 2.21, 2.65),
   realized = c(6.09, 2.91, 1.68, 2.91),
   h_ahead  = 4L
)

oos_vintage_forc_general(
    model_function = function(data) {glm(y ~ x1 + x2, data = data, family = binomial)},
    prediction_function = function(model_function, data) {
        names(data) <- c("x1", "x2")
        as.vector(predict(model_function, data, type = "response"))
    }, 
    data = dataLogit,
    realized = dataLogit$y,
    time_vec = dataLogit$date,
    x1_forecast_vintageLogit, x2_forecast_vintageLogit,
    estimation_window = NULL
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
# Parameter Forecasts.
x1_forecastLogit <- Forecast(
   origin   = as.Date(c("2013-12-31", "2013-12-31", "2013-12-31", "2013-12-31")),
   future   = as.Date(c("2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31")),
   forecast = c(2.11, 6.11, 6.75, 4.30),
   realized = NULL,
   h_ahead  = NULL
)

x2_forecastLogit <- Forecast(
   origin   = as.Date(c("2013-12-31", "2013-12-31", "2013-12-31", "2013-12-31")),
   future   = as.Date(c("2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31")),
   forecast = c(1.98, 7.44, 7.86, 5.98),
   realized = NULL,
   h_ahead  = NULL
)

conditional_forc_general(
    model_function = function(data) {glm(y ~ x1 + x2, data = data, family = binomial)},
    prediction_function = function(model_function, data) {
        names(data) <- c("x1", "x2")
        as.vector(predict(model_function, data, type = "response"))
    }, 
    data = dataLogit,
    time_vec = dataLogit$date,
    x1_forecastLogit, x2_forecastLogit
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

performance_weighted_forc(
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

## ----example101, message=FALSE------------------------------------------------
forc1_1h <- Forecast(
  origin = as.Date(c("2010-02-17", "2010-05-14", "2010-07-22", "2010-12-05", "2011-03-10")),
  future = as.Date(c("2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30")),
  forecast = c(4.27, 3.36, 4.78, 5.45, 5.12),
  realized = c(4.96, 4.17, 4.26, 4.99, 5.38),
  h_ahead = 1
)

forc2_1h <- Forecast(
  origin = as.Date(c("2010-02-17", "2010-05-14", "2010-07-22", "2010-12-22", "2011-03-27")),
  future = as.Date(c("2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30")),
  forecast = c(4.01, 3.89, 3.31, 4.33, 4.61),
  realized = c(4.96, 4.17, 4.26, 4.99, 5.38),
  h_ahead = 1
)

## ----example104, message=FALSE------------------------------------------------
forcs <- list(forc1_1h, forc2_1h)

subset_forcs(forcs, 2:3)

## ----example105, message=FALSE------------------------------------------------
forcs <- list(forc1_1h, forc2_1h)
 
subset_bytime(
  forcs, 
  values = as.Date(c("2010-09-30", "2010-12-31", "2011-03-31")), 
  slot = "future"
)

## ----example106, message=FALSE------------------------------------------------
forcs <- list(forc1_1h, forc2_1h)

subset_identical(forcs, slot = "origin")

## ----example108, message=FALSE------------------------------------------------

forc1_t1 <- Forecast(
  origin = as.Date(c("2010-02-17", "2010-02-17", "2010-02-17")),
  future = as.Date(c("2010-06-30", "2010-09-30", "2010-12-31")),
  forecast = c(4.27, 3.77, 3.52),
  realized = c(4.96, 4.17, 4.26),
  h_ahead = NA
)

forc1_t2 <- Forecast(
  origin = as.Date(c("2010-05-14", "2010-05-14", "2010-05-14")),
  future = as.Date(c("2010-09-30", "2010-12-31", "2011-03-31")),
  forecast = c(3.36, 3.82, 4.22),
  realized = c(4.17, 4.26, 4.99),
  h_ahead = NA
)

forc1_t3 <- Forecast(
  origin = as.Date(c("2010-07-22", "2010-07-22", "2010-07-22")),
  future = as.Date(c("2010-12-31", "2011-03-31", "2011-06-30")),
  forecast = c(4.78, 4.53, 5.03),
  realized = c(4.26, 4.99, 5.33),
  h_ahead = NA
)

forc1_t4 <- Forecast(
  origin = as.Date(c("2010-12-22", "2010-12-22", "2010-12-22")),
  future = as.Date(c("2011-03-31", "2011-06-30", "2011-09-30")),
  forecast = c(5.45, 4.89, 5.78),
  realized = c(4.99, 5.33, 5.21),
  h_ahead = NA
)

forcs_time_format <- list(forc1_t1, forc1_t2, forc1_t3, forc1_t4)


## ----example109, message=FALSE------------------------------------------------

forc1_1h <- Forecast(
  origin = as.Date(c("2010-02-17", "2010-05-14", "2010-07-22", "2010-12-22")),
  future = as.Date(c("2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31")),
  forecast = c(4.27, 3.36, 4.78, 5.45),
  realized = c(4.96, 4.17, 4.26, 4.99),
  h_ahead = 1
)

forc1_2h <- Forecast(
  origin = as.Date(c("2010-02-17", "2010-05-14", "2010-07-22", "2010-12-22")),
  future = as.Date(c("2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30")),
  forecast = c(3.77, 3.82, 4.53, 4.89),
  realized = c(4.17, 4.26, 4.99, 5.33),
  h_ahead = 2
)

forc1_3h <- Forecast(
  origin = as.Date(c("2010-02-17", "2010-05-14", "2010-07-22", "2010-12-22")),
  future = as.Date(c("2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30")),
  forecast = c(3.52, 4.22, 5.03, 5.78),
  realized = c(4.26, 4.99, 5.33, 5.21),
  h_ahead = 3
)

forcs_h_ahead_format <- list(forc1_1h, forc1_2h, forc1_3h)


## ----example110, message=FALSE------------------------------------------------

convert_bytime(
  forcs_h_ahead_format,
  value = as.Date(c("2010-07-22", "2010-12-22")),
  slot = "origin"
)



## ----example111, message=FALSE------------------------------------------------

transform_bytime(forcs_h_ahead_format, slot = "origin")


## ----example112, message=FALSE------------------------------------------------
convert_byh(forcs_time_format, index = 1:2, h_aheads = c(1, 2))

## ----example113, message=FALSE------------------------------------------------

transform_byh(forcs_time_format, h_aheads = c(1, 2, 3))


