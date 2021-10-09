
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
h_ahead = 2L
estimation_end = as.Date("2011-06-30")
time_vec = data$date
estimation_window = 4L

# Test output forecast length.
output_start  <- which(time_vec == estimation_end)
output_end    <- length(realized_vec) - h_ahead
output_length <- length(output_start:output_end)

# Test for forecast period 2 with h_ahead = 2L and estimation_window = 4L.
train_data     <- realized_vec[3:7]
lag_train_data <- train_data[1:3]
train_data_lhs <- train_data[3:5]
coefs <- lm(train_data_lhs ~ lag_train_data)$coefficients
pos2_forc <- coefs[[1]] + coefs[[2]] * train_data[length(train_data)]

# Function inputs.
realized_vec = data$y
h_ahead = 3L
estimation_end = as.Date("2011-06-30")
time_vec = data$date
estimation_window = NULL

# Test for forecast period 2 with h_ahead = 3L and estimation_window = NULL.
train_data     <- realized_vec[1:7]
lag_train_data <- train_data[1:4]
train_data_lhs <- train_data[4:7]
coefs <- lm(train_data_lhs ~ lag_train_data)$coefficients
pos2_forc2 <- coefs[[1]] + coefs[[2]] * train_data[length(train_data)]

#===============================================================================
# True Evaluation
#===============================================================================

forc <- autoreg_forc(
  realized_vec = data$y,
  h_ahead = 2L,
  estimation_end = as.Date("2011-06-30"),
  time_vec = data$date,
  estimation_window = 4L
)

forc2 <- autoreg_forc(
  realized_vec = data$y,
  h_ahead = 3L,
  estimation_end = as.Date("2011-06-30"),
  time_vec = data$date,
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
  expect_equal(origin(forc), time_vec[output_start:output_end])
  expect_equal(future(forc), time_vec[(nrow(data) - output_length + 1):nrow(data)])
  expect_equal(realized(forc), data$y[(nrow(data) - output_length + 1):nrow(data)])
  expect_equal(forc(forc)[2], pos2_forc)
  expect_equal(forc(forc2)[2], pos2_forc2)
})

test_that("Output Forecast is the correct length.", {
  expect_equal(output_length, length(output_start:output_end))
})
