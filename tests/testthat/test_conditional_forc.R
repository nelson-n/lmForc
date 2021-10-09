
library(lmForc)

#===============================================================================
# Stylized Testing Data
#===============================================================================

date <- as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
                  "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31",
                  "2012-03-31", "2012-06-30"))
y  <- c(1.09, 1.71, 1.09, 2.46, 1.78, 1.35, 2.89, 2.11, 2.97, 0.99)
x1 <- c(4.22, 3.86, 4.27, 5.60, 5.11, 4.31, 4.92, 5.80, 6.30, 4.17)
x2  <- c(10.03, 10.49, 10.85, 10.47, 9.09, 10.91, 8.68, 9.91, 7.87, 6.63)
data <- data.frame(date, y, x1, x2)

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

#===============================================================================
# Intended Evaluation
#===============================================================================

# Function inputs.
lm_call = lm(y ~ x1 + x2, data)
time_vec = data$date

# Test forecast for vintage forecast period 2.
pos2 <- lm_call$coefficients[[1]] +
  lm_call$coefficients[[2]] * forc(x1_forecast)[2] +
  lm_call$coefficients[[3]] * forc(x2_forecast)[2]

# Test forecast for vintage forecast period 4.
pos4 <- lm_call$coefficients[[1]] +
  lm_call$coefficients[[2]] * forc(x1_forecast)[4] +
  lm_call$coefficients[[3]] * forc(x2_forecast)[4]

#===============================================================================
# True Evaluation
#===============================================================================

forc <- conditional_forc(
  lm_call = lm(y ~ x1 + x2, data),
  time_vec = data$date,
  x1_forecast, x2_forecast
)

#===============================================================================
# Testing
#===============================================================================

test_that("Origin and future output are of the correct class.", {
  expect_equal(class(time_vec), class(origin(forc)))
  expect_equal(class(time_vec), class(future(forc)))
})

test_that("Output values are correct.", {
  expect_equal(future(forc), future(x1_forecast))
  expect_equal(forc(forc)[2], pos2)
  expect_equal(forc(forc)[4], pos4)
})

test_that("Output Forecast is the correct length.", {
  expect_equal(length(forc(x1_forecast)), length(forc(forc)))
})
