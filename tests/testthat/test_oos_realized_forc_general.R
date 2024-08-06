
library(lmForc)

#===============================================================================
# Stylized Testing Data
#===============================================================================

date <- as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
                  "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", 
                  "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31",
                  "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31"))
y  <- c(1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0)
x1 <- c(8.22, 3.86, 4.27, 3.37, 5.88, 3.34, 2.92, 1.80, 3.30, 7.17, 3.22, 3.86, 4.27, 3.37, 5.88, 3.34)
x2 <- c(4.03, 2.46, 2.04, 2.44, 6.09, 2.91, 1.68, 2.91, 3.87, 1.63, 4.03, 2.46, 2.04, 2.44, 6.09, 2.91)
dataLogit <- data.frame(date, y, x1, x2)

#===============================================================================
# Intended Evaluation
#===============================================================================

# Function inputs.
glm_call <- glm(y ~ x1 + x2, data = dataLogit, family = binomial) 
h_ahead = 2L
estimation_end = as.Date("2012-06-30")
time_vec <- dataLogit$date

# Test output forecast length.
output_start  <- which(time_vec == estimation_end)
output_end    <- nrow(dataLogit) - h_ahead
output_length <- length(output_start:output_end)

# Test forecast for period 12.
train_glm = glm(y ~ x1 + x2, dataLogit[1:12, ], family = binomial)

pos12 <- 1 / (1 + exp(-1 * (train_glm$coefficients[[1]] + 
                            train_glm$coefficients[[2]] * dataLogit$x1[12 + h_ahead] + 
                            train_glm$coefficients[[3]] * dataLogit$x2[12 + h_ahead])))

# Test forecast for period 14.
train_glm = glm(y ~ x1 + x2, dataLogit[1:14, ], family = binomial)

pos14 <- 1 / (1 + exp(-1 * (train_glm$coefficients[[1]] + 
                            train_glm$coefficients[[2]] * dataLogit$x1[14 + h_ahead] + 
                            train_glm$coefficients[[3]] * dataLogit$x2[14 + h_ahead])))

# Test forecast for period 13 with evaluation_window set to 8. 
train_glm2 = glm(y ~ x1 + x2, dataLogit[5:13, ], family = binomial)

pos13 <- 1 / (1 + exp(-1 * (train_glm2$coefficients[[1]] + 
                            train_glm2$coefficients[[2]] * dataLogit$x1[13 + h_ahead] + 
                            train_glm2$coefficients[[3]] * dataLogit$x2[13 + h_ahead])))

#===============================================================================
# True Evaluation
#===============================================================================

forc <- oos_realized_forc_general(
    model_function = function(data) {glm(y ~ x1 + x2, data = data, family = binomial)},
    prediction_function = function(model_function, data) {as.vector(predict(model_function, data, type = "response"))}, 
    data = dataLogit,
    realized = dataLogit$y,
    h_ahead = 2L,
    estimation_end = as.Date("2012-06-30"),
    time_vec = dataLogit$date,
    estimation_window = NULL
)

forc2 <- oos_realized_forc_general(
    model_function = function(data) {glm(y ~ x1 + x2, data = data, family = binomial)},
    prediction_function = function(model_function, data) {as.vector(predict(model_function, data, type = "response"))}, 
    data = dataLogit,
    realized = dataLogit$y,
    h_ahead = 2L,
    estimation_end = as.Date("2012-06-30"),
    time_vec = dataLogit$date,
    estimation_window = 8L
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
  expect_equal(future(forc), time_vec[(nrow(dataLogit) - output_length + 1):nrow(dataLogit)])
  expect_equal(realized(forc), dataLogit$y[(nrow(dataLogit) - output_length + 1):nrow(dataLogit)])
  expect_equal(forc(forc)[3], pos12)
  expect_equal(forc(forc)[5], pos14)
  expect_equal(forc(forc2)[4], pos13)
})

test_that("Output Forecast is the correct length.", {
  expect_equal(output_length, length(output_start:output_end))
})

