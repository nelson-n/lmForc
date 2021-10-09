
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
lm_call  = lm(y ~ x1 + x2, data)
time_vec = data$date

# Test forecast for period 8.
pos8 <- lm_call$coefficients[[1]] + lm_call$coefficients[[2]] * data$x1[8] +
  lm_call$coefficients[[3]] * data$x2[8]

#===============================================================================
# True Evaluation
#===============================================================================

forc <- is_forc(
  lm_call  = lm_call,
  time_vec = time_vec
)

#===============================================================================
# Testing
#===============================================================================

test_that("Origin and future output are of the correct class.", {
  expect_equal(class(time_vec), class(origin(forc)))
  expect_equal(class(time_vec), class(future(forc)))
})

test_that("Output values are correct.", {
  expect_equal(origin(forc)[8], time_vec[8])
  expect_equal(future(forc)[8], time_vec[8])
  expect_equal(forc(forc)[8], pos8)
  expect_equal(realized(forc)[8], data$y[8])
})
