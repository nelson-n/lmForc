
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
lm_call = lm(y ~ x1 + x2, data)
h_ahead = 2L
estimation_end = as.Date("2011-03-31")
time_vec = data$date

# Test output forecast length.
output_start  <- which(time_vec == estimation_end)
output_end    <- nrow(lm_call$model) - h_ahead
output_length <- length(output_start:output_end)

# Manually lag covariates.
x1[3:10] <- x1[1:8]
x1[1:2]  <- NA

x2[3:10] <- x2[1:8]
x2[1:2]  <- NA

train_data <- data.frame(date, y, x1, x2)

# Test forecast for period 5.
train_lm = lm(y ~ x1 + x2, train_data[1:5, ])

pos5 <- train_lm$coefficients[[1]] + train_lm$coefficients[[2]] * data$x1[5] +
  train_lm$coefficients[[3]] * data$x2[5]

# Test forecast for period 7.
train_lm = lm(y ~ x1 + x2, train_data[1:7, ])

pos7 <- train_lm$coefficients[[1]] + train_lm$coefficients[[2]] * data$x1[7] +
  train_lm$coefficients[[3]] * data$x2[7]

# Test forecast for period 8 with estimation_window set to 4.
train_lm2 = lm(y ~ x1 + x2, train_data[4:8, ])

pos8 <- train_lm2$coefficients[[1]] + train_lm2$coefficients[[2]] * data$x1[8] +
  train_lm2$coefficients[[3]] * data$x2[8]

#===============================================================================
# True Evaluation
#===============================================================================

forc <- oos_lag_forc(
  lm_call = lm(y ~ x1 + x2, data),
  h_ahead = 2L,
  estimation_end = as.Date("2011-03-31"),
  time_vec = data$date
)

forc2 <- oos_lag_forc(
  lm_call = lm(y ~ x1 + x2, data),
  h_ahead = 2L,
  estimation_end = as.Date("2011-03-31"),
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
  expect_equal(forc(forc)[1], pos5)
  expect_equal(forc(forc)[3], pos7)
  expect_equal(forc(forc2)[4], pos8)
})

test_that("Output Forecast is the correct length.", {
  expect_equal(output_length, length(output_start:output_end))
})
