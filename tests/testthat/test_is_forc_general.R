
library(lmForc)

#===============================================================================
# Stylized Testing Data
#===============================================================================

date <- as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
                  "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", 
                  "2012-03-31", "2012-06-30"))
y  <- c(1, 0, 0, 0, 1, 1, 0, 0, 0, 1)
x1 <- c(8.22, 3.86, 4.27, 3.37, 5.88, 3.34, 2.92, 1.80, 3.30, 7.17)
x2 <- c(4.03, 2.46, 2.04, 2.44, 6.09, 2.91, 1.68, 2.91, 3.87, 1.63)
dataLogit <- data.frame(date, y, x1, x2)

#===============================================================================
# Intended Evaluation (Logit Example)
#===============================================================================

# Function inputs.
glm_call <- glm(y ~ x1 + x2, data = dataLogit, family = binomial) 
time_vec <- dataLogit$date

# Logit estimation formula. 
pos10 <- 1 / (1 + exp(-1 * (glm_call$coefficients[[1]] + 
                            glm_call$coefficients[[2]] * dataLogit$x1[10] + 
                            glm_call$coefficients[[3]] * dataLogit$x2[10])))

#===============================================================================
# True Evaluation (Logit Example)
#===============================================================================

forc <- is_forc_general(
  model_function = function(data) {glm(y ~ x1 + x2, data = data, family = binomial)},
  prediction_function = function(model_function, data) {as.vector(predict(model_function, data, type = "response"))}, 
  data = dataLogit,
  realized = dataLogit$y,
  time_vec = dataLogit$date
)

#===============================================================================
# Testing (Logit Example)
#===============================================================================

test_that("Origin and future output are of the correct class.", {
  expect_equal(class(time_vec), class(origin(forc)))
  expect_equal(class(time_vec), class(future(forc)))
})

test_that("Output values are correct.", {
  expect_equal(origin(forc)[10], time_vec[10])
  expect_equal(future(forc)[10], time_vec[10])
  expect_equal(forc(forc)[10], pos10)
  expect_equal(realized(forc)[10], dataLogit$y[10])
})

#===============================================================================
# Intended Evaluation (Custom Function Example)
#===============================================================================

# Model is a custom function where: y = ifelse(x1 > 5, 1, 0).

intendedOutput = c(1, 0, 0, 0, 1, 0, 0, 0, 0, 1)

#===============================================================================
# True Evaluation (Custom Function Example)
#===============================================================================

forc <- is_forc_general(
  model_function = function(data) {ifelse(data$x1 > 5, 1, 0)},
  prediction_function = function(model_function, data) {model_function},
  data = dataLogit,
  realized = dataLogit$y,
  time_vec = dataLogit$date
)

#===============================================================================
# Testing (Custom Function Example)
#===============================================================================

test_that("Output values are correct.", {
  expect_equal(forc(forc), intendedOutput)
})

