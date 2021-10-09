
library(lmForc)

#===============================================================================
# Stylized Testing Data
#===============================================================================

origins <- as.Date(c("2009-03-31", "2009-06-30", "2009-09-30", "2009-12-31",
                     "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
                     "2011-03-31", "2011-06-30"))

futures <- as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
                     "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31",
                     "2012-03-31", "2012-06-30"))

y  <- c(1.09, 1.71, 1.09, 2.46, 1.78, 1.35, 2.89, 2.11, 2.97, 0.99)

y1_forecast <- Forecast(
  origin = origins,
  future = futures,
  forecast = c(1.33, 1.36, 1.38, 1.68, 1.60, 1.55, 1.32, 1.22, 1.08, 0.88),
  realized = c(1.09, 1.71, 1.09, 2.46, 1.78, 1.35, 2.89, 2.11, 2.97, 0.99),
  h_ahead = 4L
)

y2_forecast <- Forecast(
  origin = origins,
  future = futures,
  forecast = c(0.70, 0.88, 1.03, 1.05, 1.01, 0.82, 0.95, 1.09, 1.07, 1.06),
  realized = c(1.09, 1.71, 1.09, 2.46, 1.78, 1.35, 2.89, 2.11, 2.97, 0.99),
  h_ahead = 4L
)

#===============================================================================
# Intended Evaluation
#===============================================================================

# Function inputs.
eval_window = 2L

# Test mse weighted forecast for period 8.
mse_y1 <- 1/eval_window * sum((realized(y1_forecast)[3:4] - forc(y1_forecast)[3:4])^2)
mse_y2 <- 1/eval_window * sum((realized(y2_forecast)[3:4] - forc(y2_forecast)[3:4])^2)

w1 <- (1/mse_y1) / (1/mse_y1 + 1/mse_y2)
w2 <- (1/mse_y2) / (1/mse_y1 + 1/mse_y2)

pos8 <- (forc(y1_forecast)[8] * w1) + (forc(y2_forecast)[8] * w2)

# Test mse weighted forecast for period 10.
mse_y1 <- 1/eval_window * sum((realized(y1_forecast)[5:6] - forc(y1_forecast)[5:6])^2)
mse_y2 <- 1/eval_window * sum((realized(y2_forecast)[5:6] - forc(y2_forecast)[5:6])^2)

w1 <- (1/mse_y1) / (1/mse_y1 + 1/mse_y2)
w2 <- (1/mse_y2) / (1/mse_y1 + 1/mse_y2)

pos10 <- (forc(y1_forecast)[10] * w1) + (forc(y2_forecast)[10] * w2)

# Test rmse weighted forecast for period 10 with eval_window = 4L.
mse_y1  <- 1/eval_window * sum((realized(y1_forecast)[3:6] - forc(y1_forecast)[3:6])^2)
mse_y2  <- 1/eval_window * sum((realized(y2_forecast)[3:6] - forc(y2_forecast)[3:6])^2)
rmse_y1 <- sqrt(mse_y1)
rmse_y2 <- sqrt(mse_y2)

w1 <- (1/rmse_y1) / (1/rmse_y1 + 1/rmse_y2)
w2 <- (1/rmse_y2) / (1/rmse_y1 + 1/rmse_y2)

pos10_2 <- (forc(y1_forecast)[10] * w1) + (forc(y2_forecast)[10] * w2)

#===============================================================================
# True Evaluation
#===============================================================================

forc1 <- mse_weighted_forc(
  y1_forecast, y2_forecast,
  eval_window = 2L,
  errors = "mse",
  return_weights = FALSE
)

forc2 <- mse_weighted_forc(
  y1_forecast, y2_forecast,
  eval_window = 4L,
  errors = "rmse",
  return_weights = FALSE
)

#===============================================================================
# Testing
#===============================================================================

test_that("Output values are correct.", {
  expect_equal(future(forc1), future(y1_forecast))
  expect_equal(realized(forc1), realized(y1_forecast))
  expect_equal(forc(forc1)[8], pos8)
  expect_equal(forc(forc1)[10], pos10)
  expect_equal(forc(forc2)[10], pos10_2)
})
