
library(lmForc)

#===============================================================================
# Stylized Testing Data
#===============================================================================

date <- as.Date(c(NA, NA, "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
                  "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31",
                  "2012-03-31", "2012-06-30"))
y  <- c(NA, NA, 1.09, 1.71, 1.09, 2.46, 1.78, 1.35, 2.89, 2.11, 2.97, 0.99)
x1 <- c(NA, NA, 4.22, 3.86, 4.27, 5.60, 5.11, 4.31, 4.92, 5.80, 6.30, 4.17)
x2  <- c(NA, NA, 10.03, 10.49, 10.85, 10.47, 9.09, 10.91, 8.68, 9.91, 7.87, 6.63)
data <- data.frame(date, y, x1, x2)

#===============================================================================
# Intended Evaluation
#===============================================================================

# Function inputs.
realized_vec = data$y
h_ahead = 2L
estimation_end = as.Date("2011-03-31")
time_vec = data$date
estimation_window = 4L

# Test output forecast length.
output_start  <- which(time_vec == estimation_end)
output_end    <- length(realized_vec) - h_ahead
output_length <- length(output_start:output_end)

# Test forecast for period 6 with estimation_window = 4L and mean avg_function.
pos8_forc_mean <- base::mean(realized_vec[(8 - estimation_window):8], na.rm = TRUE)

# Test forecast period 6 without estimation_window and mean avg_function.
pos6_forc2_mean <- base::mean(realized_vec[1:6], na.rm = TRUE)

# Test forecast period 6 without estimation_window and median avg_function.
pos6_forc3_median <- stats::median(realized_vec[1:6], na.rm = TRUE)

#===============================================================================
# True Evaluation
#===============================================================================

forc <- historical_average_forc(
  avg_function = "mean",
  realized_vec = data$y,
  h_ahead = 2L,
  estimation_end = as.Date("2011-03-31"),
  time_vec = data$date,
  estimation_window = 4L
)

forc2 <- historical_average_forc(
  avg_function = "mean",
  realized_vec = data$y,
  h_ahead = 4L,
  estimation_end = 4L,
)

forc3 <- historical_average_forc(
  avg_function = "median",
  realized_vec = data$y,
  h_ahead = 4L,
  estimation_end = 4L,
)

#===============================================================================
# Testing
#===============================================================================

test_that("Origin and future output are of the correct class.", {
  expect_equal(class(time_vec), class(origin(forc)))
  expect_equal(class(time_vec), class(future(forc)))
})

test_that("Output values are correct.", {
  expect_equal(origin(forc), time_vec[output_start:output_end])
  expect_equal(future(forc), time_vec[(nrow(data) - output_length + 1):nrow(data)])
  expect_equal(realized(forc), data$y[(nrow(data) - output_length + 1):nrow(data)])
  expect_equal(forc(forc)[2], pos8_forc_mean)
  expect_equal(forc(forc2)[3], pos6_forc2_mean)
  expect_equal(forc(forc3)[3], pos6_forc3_median)
})

test_that("Output Forecast is the correct length.", {
  expect_equal(output_length, length(output_start:output_end))
})
