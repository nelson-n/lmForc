
library(lmForc)

#===============================================================================
# Stylized Testing Data
#===============================================================================

# Estimation Data.
date <- as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
                  "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", 
                  "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31",
                  "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31"))
y  <- c(1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0)
x1 <- c(8.22, 3.86, 4.27, 3.37, 5.88, 3.34, 2.92, 1.80, 3.30, 7.17, 3.22, 3.86, 4.27, 3.37, 5.88, 3.34)
x2 <- c(4.03, 2.46, 2.04, 2.44, 6.09, 2.91, 1.68, 2.91, 3.87, 1.63, 4.03, 2.46, 2.04, 2.44, 6.09, 2.91)
dataLogit <- data.frame(date, y, x1, x2)

# Parameter Forecasts.
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

#===============================================================================
# Intended Evaluation
#===============================================================================

# Function inputs.
glm_call <- glm(y ~ x1 + x2, data = dataLogit, family = binomial) 

# Test output forecast length.
output_length <- length(x1_forecast_vintageLogit@forecast)
time_vec <- dataLogit$date

# Testing the h_ahead = 4 forecast made at 2012-12-31 for 2013-12-31.
estimation_end <- as.Date("2012-12-31")

train_glm <- glm(y ~ x1 + x2, dataLogit[dataLogit$date <= estimation_end, ], family = binomial)

pos2 <- 1 / (1 + exp(-1 * (train_glm$coefficients[[1]] + 
                           train_glm$coefficients[[2]] * x1_forecast_vintageLogit@forecast[x1_forecast_vintageLogit@future == as.Date("2013-12-31")] +
                           train_glm$coefficients[[3]] * x2_forecast_vintageLogit@forecast[x2_forecast_vintageLogit@future == as.Date("2013-12-31")])))

# Testing the h_ahead = 4 forecast made at 2013-06-30 for 2014-06-30.
estimation_end <- as.Date("2013-06-30")

train_glm <- glm(y ~ x1 + x2, dataLogit[dataLogit$date <= estimation_end, ], family = binomial)

pos4 <- 1 / (1 + exp(-1 * (train_glm$coefficients[[1]] + 
                           train_glm$coefficients[[2]] * x1_forecast_vintageLogit@forecast[x1_forecast_vintageLogit@future == as.Date("2014-06-30")] +
                           train_glm$coefficients[[3]] * x2_forecast_vintageLogit@forecast[x2_forecast_vintageLogit@future == as.Date("2014-06-30")])))

#===============================================================================
# True Evaluation
#===============================================================================

forc <- oos_vintage_forc_general(
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

#===============================================================================
# Testing
#===============================================================================

test_that("Origin and future output are of the correct class.", {
  expect_equal(class(time_vec), class(origin(forc)))
  expect_equal(class(time_vec), class(future(forc)))
})

test_that("Output values are correct.", {
    expect_equal(origin(forc), origin(x1_forecast_vintageLogit))
    expect_equal(future(forc), future(x1_forecast_vintageLogit))
    expect_equal(forc(forc)[2], pos2)
    expect_equal(forc(forc)[4], pos4)
})

