
library(lmForc)

# Function to lag a vector n steps.
vector_lag <- function(vector, n) {
  vector <- c(rep(NA, n), vector[1:(length(vector) - n)])
  return(vector)
}

#===============================================================================
# Stylized Testing Data
#===============================================================================

date <- as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
                  "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31",
                  "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31"))
y  <- c(1.09, 1.71, 1.09, 2.46, 1.78, 1.35, 2.89, 2.11, 2.97, 0.99, 1.31, 2.33)
x1 <- c(4.22, 3.86, 4.27, 5.60, 5.11, 4.31, 4.92, 5.80, 6.30, 4.17, 4.18, 5.89)
x2  <- c(10.03, 10.49, 10.85, 10.47, 9.09, 10.91, 8.68, 9.91, 7.87, 6.63, 6.67, 7.77)
data <- data.frame(date, y, x1, x2)

#===============================================================================
# Intended Evaluation
#===============================================================================

# Function inputs.
realized_vec = data$y
h_ahead = 4L
ar_lags = 3L
estimation_end = 4L
time_vec = 1:length(realized_vec)
estimation_window = NULL 

# Test output forecast length.
output_start  <- which(time_vec == estimation_end)
output_end    <- length(realized_vec) - h_ahead
output_length <- length(output_start:output_end)

# Test for forecast period 5 with h_ahead = 4L and ar_lags = 3L.
full_train_data <- realized_vec[1:8]
full_train_data_lag1 <- vector_lag(full_train_data, n = 1)
full_train_data_lag2 <- vector_lag(full_train_data, n = 2)
full_train_data_lag3 <- vector_lag(full_train_data, n = 3)

train_data <- full_train_data
train_data_lag1 <- full_train_data_lag1
train_data_lag2 <- full_train_data_lag2
train_data_lag3 <- full_train_data_lag3

coefs <- lm(train_data ~ train_data_lag1 + train_data_lag2 + train_data_lag3)$coefficients

forc_1ahead <- coefs[[1]] + coefs[[2]] * realized_vec[[8]] + coefs[[3]] * realized_vec[[7]] + coefs[[4]] * realized_vec[[6]]
forc_2ahead <- coefs[[1]] + coefs[[2]] * forc_1ahead + coefs[[3]] * realized_vec[[8]] + coefs[[4]] * realized_vec[[7]]
forc_3ahead <- coefs[[1]] + coefs[[2]] * forc_2ahead + coefs[[3]] * forc_1ahead + coefs[[4]] * realized_vec[[8]]
forc_4ahead <- coefs[[1]] + coefs[[2]] * forc_3ahead + coefs[[3]] * forc_2ahead + coefs[[4]] * forc_1ahead
pos5_forc <- forc_4ahead

# Test for forecast period 4 with h_ahead = 1L, ar_lags = 2L, and estimation_window = 4L.
full_train_data <- realized_vec
full_train_data_lag1 <- vector_lag(full_train_data, n = 1)
full_train_data_lag2 <- vector_lag(full_train_data, n = 2)

train_data <- full_train_data[5:9]
train_data_lag1 <- full_train_data_lag1[5:9]
train_data_lag2 <- full_train_data_lag2[5:9]

coefs <- lm(train_data ~ train_data_lag1 + train_data_lag2)$coefficients

pos4_forc2 <- coefs[[1]] + coefs[[2]] * realized_vec[[9]] + coefs[[3]] * realized_vec[[8]]

#===============================================================================
# True Evaluation
#===============================================================================

forc <- autoreg_forc(
  realized_vec = data$y,
  h_ahead = 4L,
  ar_lags = 3L,
  estimation_end = 4L,
  time_vec = NULL,
  estimation_window = NULL
)

forc2 <- autoreg_forc(
  realized_vec = data$y,
  h_ahead = 1L,
  ar_lags = 2L,
  estimation_end = as.Date("2011-06-30"),
  time_vec = data$date,
  estimation_window = 4L
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
  expect_equal(forc(forc)[5], pos5_forc)
  expect_equal(forc(forc2)[4], pos4_forc2)
})

test_that("Output Forecast is the correct length.", {
  expect_equal(output_length, length(output_start:output_end))
})
