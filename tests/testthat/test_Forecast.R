
library(lmForc)

#===============================================================================
# Stylized Testing Data
#===============================================================================

# For testing merging with the forc2df function.
merge_1 <- Forecast(
  origin = c(1, 4, 5, 8),
  future = c(10, 11, 12, 13),
  forecast = c(20, 21, 22, 23),
  realized = c(30, 31, 32, 33),
)

merge_2 <- Forecast(
  origin = c(2, 3, 6, 7),
  future = c(10, 11, 12, 13),
  forecast = c(40, 41, 42, 43),
  realized = c(30, 31, 32, 33),
)

# For testing mse and rmse methods.
my_forecast <- Forecast(
   origin   = as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31")),
   future   = as.Date(c("2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2011-03-31")),
   forecast = c(4.21, NA, 5.32, 5.11, 7.42),
   realized = c(4.40, 4.45, 4.87, 4.77, NA),
   h_ahead  = 4L
)

#===============================================================================
# Intended Evaluation
#===============================================================================

forecast <- c(4.21, 5.32, 5.11)
realized <- c(4.40, 4.87, 4.77)

mse_intended <- (1/length(forecast) * sum((realized - forecast)^2))
rmse_intended <- sqrt(mse_intended)
mae_intended <- mean(abs(forecast - realized))
mape_intended <- mean(abs(realized - forecast) / realized)
R2_intended <- cor(forecast, realized)^2

#===============================================================================
# True Evaluation
#===============================================================================

mse  <- mse(my_forecast)
rmse <- rmse(my_forecast)
mae <- mae(my_forecast)
mape <- mape(my_forecast)
R2 <- R2(my_forecast)

merged <- forc2df(merge_1, merge_2)

#===============================================================================
# Testing
#===============================================================================

test_that("Mse, rmse, mape, and R2 methods produce correct values.", {
  expect_equal(mse, mse_intended)
  expect_equal(rmse, rmse_intended)
  expect_equal(mape, mape_intended)
  expect_equal(R2, R2_intended)
})

test_that("forc2df function correctly merges origin values.", {
  expect_equal(merged$origin, c(2, 4, 6, 8))
})
