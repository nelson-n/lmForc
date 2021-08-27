
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

x1_forecast_vintage <- Forecast(
  origin   = as.Date(c("2010-09-28", "2010-12-29", "2011-03-31", "2011-06-30")),
  future   = as.Date(c("2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30")),
  forecast = c(6.30, 4.17, 5.30, 4.84),
  realized = c(4.92, 5.80, 6.30, 4.17),
  h_ahead  = 4L
)

x2_forecast_vintage <- Forecast(
  origin   = as.Date(c("2010-09-26", "2010-12-30", "2011-03-31", "2011-06-30")),
  future   = as.Date(c("2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30")),
  forecast = c(7.32, 6.88, 6.82, 6.95),
  realized = c(8.68, 9.91, 7.87, 6.63),
  h_ahead  = 4L
)

#===============================================================================
# Intended Evaluation
#===============================================================================

# Function inputs.
lm_call = lm(y ~ x1 + x2, data)
time_vec = data$date

# Test forecast for vintage forecast period 2.
train_data <- lm_call$model[(which(time_vec <= origin(x2_forecast_vintage)[2])), ]
train_lm   <- lm(y ~ x1 + x2, train_data)

pos2 <- train_lm$coefficients[[1]] +
  train_lm$coefficients[[2]] * forc(x1_forecast_vintage)[2] +
  train_lm$coefficients[[3]] * forc(x2_forecast_vintage)[2]

# Test forecast for vintage forecast period 4.
train_data <- lm_call$model[(which(time_vec <= origin(x2_forecast_vintage)[4])), ]
train_lm   <- lm(y ~ x1 + x2, train_data)

pos4 <- train_lm$coefficients[[1]] +
  train_lm$coefficients[[2]] * forc(x1_forecast_vintage)[4] +
  train_lm$coefficients[[3]] * forc(x2_forecast_vintage)[4]

#===============================================================================
# True Evaluation
#===============================================================================

forc <- oos_vintage_forc(
  lm_call = lm(y ~ x1 + x2, data),
  time_vec = data$date,
  x1_forecast_vintage, x2_forecast_vintage
)

#===============================================================================
# Testing
#===============================================================================

test_that("Origin and future output are of the correct class.", {
  expect_equal(class(time_vec), class(origin(forc)))
  expect_equal(class(time_vec), class(future(forc)))
})

test_that("Output values are correct.", {
  expect_equal(future(forc), future(x1_forecast_vintage))
  expect_equal(realized(forc), lm_call$model$y[time_vec %in% future(x1_forecast_vintage)])
  expect_equal(forc(forc)[2], pos2)
  expect_equal(forc(forc)[4], pos4)
})

test_that("Output Forecast is the correct length.", {
  expect_equal(length(forc(x1_forecast_vintage)), length(forc(forc)))
})

test_that("Origin vector correctly merges origin values.", {
  expect_equal(
    origin(forc),
    as.Date(c("2010-09-28", "2010-12-30", "2011-03-31", "2011-06-30")))
})
