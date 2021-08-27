
library(lmForc)

#===============================================================================
# Stylized Testing Data
#===============================================================================

# For testing mse and rmse methods.
my_forecast <- Forecast(
   origin   = as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31")),
   future   = as.Date(c("2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31")),
   forecast = c(4.21, 4.27, 5.32, 5.11),
   realized = c(4.40, 4.45, 4.87, 4.77),
   h_ahead  = 4L
)

# For testing merging with the collect method.
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

#===============================================================================
# Intended Evaluation
#===============================================================================

mse_intended <- (1/length(forc(my_forecast)) *
  sum((realized(my_forecast) - forc(my_forecast))^2))

rmse_intended <- sqrt(mse_intended)

#===============================================================================
# True Evaluation
#===============================================================================

mse  <- mse(my_forecast)
rmse <- rmse(my_forecast)

merged <- collect(merge_1, merge_2)

#===============================================================================
# Testing
#===============================================================================

test_that("Mse and rmse methods produce correct values.", {
  expect_equal(mse, mse_intended)
  expect_equal(rmse, rmse_intended)
})

test_that("Collect method correctly merges origin values.", {
  expect_equal(merged$origin, c(2, 4, 6, 8))
})
