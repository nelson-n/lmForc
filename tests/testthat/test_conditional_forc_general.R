
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

#===============================================================================
# Intended Evaluation
#===============================================================================

# Function inputs.
glm_call <- glm(y ~ x1 + x2, data = dataLogit, family = binomial) 

# Test output forecast length.
output_length <- length(x1_forecastLogit@forecast)
time_vec <- dataLogit$date

# Test forecasts.
train_glm <- glm(y ~ x1 + x2, dataLogit, family = binomial)

pos2 <- 1 / (1 + exp(-1 * (train_glm$coefficients[[1]] + 
                           train_glm$coefficients[[2]] * x1_forecastLogit@forecast[2] +
                           train_glm$coefficients[[3]] * x2_forecastLogit@forecast[2])))

pos4 <- 1 / (1 + exp(-1 * (train_glm$coefficients[[1]] + 
                           train_glm$coefficients[[2]] * x1_forecastLogit@forecast[4] +
                           train_glm$coefficients[[3]] * x2_forecastLogit@forecast[4])))

#===============================================================================
# True Evaluation
#===============================================================================

forc <- conditional_forc_general(
    model_function = function(data) {glm(y ~ x1 + x2, data = data, family = binomial)},
    prediction_function = function(model_function, data) {
        names(data) <- c("x1", "x2")
        as.vector(predict(model_function, data, type = "response"))
    }, 
    data = dataLogit,
    time_vec = dataLogit$date,
    x1_forecastLogit, x2_forecastLogit
)

#===============================================================================
# Testing
#===============================================================================

test_that("Origin and future output are of the correct class.", {
  expect_equal(class(time_vec), class(origin(forc)))
  expect_equal(class(time_vec), class(future(forc)))
})

test_that("Output values are correct.", {
    expect_equal(origin(forc), origin(x1_forecastLogit))
    expect_equal(future(forc), future(x1_forecastLogit))
    expect_equal(forc(forc)[2], pos2)
    expect_equal(forc(forc)[4], pos4)
})

