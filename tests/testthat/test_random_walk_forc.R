
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

#===============================================================================
# Intended Evaluation
#===============================================================================

# Function inputs.
realized_vec = data$y
h_ahead = 4L
time_vec = data$date

# Test output length.
output_length <- length(realized_vec) - h_ahead

# Manually construct Forecast values for pos 4.
test_origin <- time_vec[[4]]
test_future <- time_vec[[4 + h_ahead]]
test_forecast <- realized_vec[[4]]
test_realized <- realized_vec[[4 + h_ahead]]

#===============================================================================
# True Evaluation
#===============================================================================

forc <- random_walk_forc(
  realized_vec = data$y,
  h_ahead = 4L,
  time_vec = data$date 
)

#===============================================================================
# Testing
#===============================================================================

test_that("Output values are correct.", {
  expect_equal(origin(forc)[4], test_origin)
  expect_equal(future(forc)[4], test_future)
  expect_equal(forc(forc)[4], test_forecast)
  expect_equal(future(forc)[4], test_future)
})

test_that("Output Forecast is the correct length.", {
  expect_equal(output_length, length(origin(forc)))
})
