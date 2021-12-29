
library(lmForc)

#===============================================================================
# Stylized Testing Data
#===============================================================================

date <- as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
                  "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31",
                  "2012-03-31", "2012-06-30"))

future <- as.Date(c("2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31",
                    "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31",
                    "2013-03-31", "2013-06-30"))

y  <- c(1.09, 1.71, 1.09, 2.46, 1.78, 1.35, 2.89, 2.11, 2.97, 0.99)
x1 <- c(4.22, 3.86, 4.27, 5.60, 5.11, 4.31, 4.92, 5.80, 6.30, 4.17)
x2  <- c(10.03, 10.49, 10.85, 10.47, 9.09, 10.91, 8.68, 9.91, 7.87, 6.63)

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

#===============================================================================
# Functions
#===============================================================================

# Standardization function. 
standardize <- function(x) (x - mean(x)) / sd(x)

# Euclidean matching function.
euclidean <- function(x, y) sqrt(sum((x - y)^2))

# MSE function.
mse <- function(x, y) {
  1/length(x) * sum((y - x)^2)
}

# RMSE function.
rmse <- function(x, y) {
  sqrt(mse(x, y))
}

#===============================================================================
# Intended Evaluation
#===============================================================================

#-------------------------------------------------------------------------------
# Test forecast for period 8.
#-------------------------------------------------------------------------------

# Standardize matching_vars.
matching_vars <- apply(matching_vars, MARGIN = 2, standardize)

# Find current state of the world.
current_state <- matching_vars[7:8, ]

# Test all possible state matches manually.
euclidean(current_state, matching_vars[1:2, ])
euclidean(current_state, matching_vars[2:3, ])
euclidean(current_state, matching_vars[3:4, ])
euclidean(current_state, matching_vars[4:5, ])
euclidean(current_state, matching_vars[5:6, ])

# Calculate forecast RMSEs in state match period.
y1_rmse <- rmse(
  forc(y1_forecast)[origin(y1_forecast) %in% data$date[3:4]],
  realized(y1_forecast)[origin(y1_forecast) %in% data$date[3:4]]
)

y2_rmse <- rmse(
  forc(y2_forecast)[origin(y2_forecast) %in% data$date[3:4]],
  realized(y2_forecast)[origin(y2_forecast) %in% data$date[3:4]]
)

# Calculate forecast weights.
y1_weight = (1 / y1_rmse) / ((1 / y1_rmse) + (1 / y2_rmse))
y2_weight = (1 / y2_rmse) / ((1 / y1_rmse) + (1 / y2_rmse))

# Calculate forecast.
pos8_forc <- forc(y1_forecast)[[8]] * y1_weight + forc(y2_forecast)[[8]] * y2_weight

#-------------------------------------------------------------------------------
# Test forecast for period 9.
#-------------------------------------------------------------------------------

# Find current state of the world.
current_state <- matching_vars[7:9, ]

# Test all possible state matches manually.
euclidean(current_state, matching_vars[1:3, ])
euclidean(current_state, matching_vars[2:4, ])
euclidean(current_state, matching_vars[3:5, ])
euclidean(current_state, matching_vars[4:6, ])

# Calculate forecast RMSEs in state match period.
y1_mse <- mse(
  forc(y1_forecast)[origin(y1_forecast) %in% data$date[3:5]],
  realized(y1_forecast)[origin(y1_forecast) %in% data$date[3:5]]
)

y2_mse <- mse(
  forc(y2_forecast)[origin(y2_forecast) %in% data$date[3:5]],
  realized(y2_forecast)[origin(y2_forecast) %in% data$date[3:5]]
)

# Calculate forecast weights.
y1_weight = (1 / y1_mse) / ((1 / y1_mse) + (1 / y2_mse))
y2_weight = (1 / y2_mse) / ((1 / y1_mse) + (1 / y2_mse))

# Calculate forecast.
pos9_forc <- forc(y1_forecast)[[9]] * y1_weight + forc(y2_forecast)[[9]] * y2_weight

#===============================================================================
# True Evaluation
#===============================================================================

forc1 <- states_weighted_forc(
  y1_forecast, y2_forecast,
  matching_vars = matching_vars,
  time_vec = data$date,
  matching_window = 2L,
  matching = "euclidean",
  errors = "rmse",
  return_weights = FALSE
)

forc2 <- states_weighted_forc(
  y1_forecast, y2_forecast,
  matching_vars = matching_vars,
  time_vec = data$date,
  matching_window = 3L,
  matching = "euclidean",
  errors = "mse",
  return_weights = FALSE
)

#===============================================================================
# Testing
#===============================================================================

test_that("Origin and future output are of the correct class.", {
  expect_equal(class(date), class(origin(forc1)))
  expect_equal(class(future), class(future(forc1)))
})

test_that("Output values are correct.", {
  expect_equal(future(forc1), future(y1_forecast))
  expect_equal(realized(forc1), realized(y1_forecast))
  expect_equal(future(forc2), future(y2_forecast))
  expect_equal(realized(forc2), realized(y2_forecast))
  expect_equal(forc(forc1)[8], pos8_forc)
  expect_equal(forc(forc2)[9], pos9_forc)
})

test_that("Output Forecast is the correct length.", {
  expect_equal(length(forc(y1_forecast)), length(forc(forc1)))
})
