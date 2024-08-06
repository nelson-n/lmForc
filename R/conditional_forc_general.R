
#' General model forecast conditioned on input forecasts
#'
#' \code{conditional_forc_general} takes a model function, a prediction function,
#' input data for estimating the model, and a vector of time data associated with the
#' model. The model is estimated once over the entire sample period and the model
#' parameters are then combined with the input forecasts to generate a forecast.
#' Returns a forecast conditional on forecasts of each parameter. Used to create a
#' forecast for the present period or replicate a forecast made at a specific period
#' in the past.
#' 
#' @param model_function Function that estimates a model using the \code{data} input.
#' @param prediction_function Function that generates model predictions using \code{model_function}
#'   and \code{data} arguments. Note* that the \code{data} argument passed to the prediction_function
#'   takes the form of a data.frame with a number of columns equal to the number of 
#'   input vintage forecasts passed by the user. The prediction_function needs to be able to take
#'   this input format and generate a prediction based on it.
#' @param data Input data for estimating the model.
#' @param time_vec Vector of any class that represents time and is equal in length to 
#'   the length of \code{realized} and \code{data}.
#' @param ... Set of forecasts of class Forecast, one forecast for each
#'   parameter in the linear model.
#'   
#' @return \code{\link{Forecast}} object that contains the out-of-sample
#'   forecast.
#'
#' @seealso
#' For a detailed example see the help vignette:
#' \code{vignette("lmForc", package = "lmForc")}
#'
#' @examples
#' 
#' 
#' # Estimation Data.
#' date <- as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
#'                   "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", 
#'                   "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31",
#'                   "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31"))
#' y  <- c(1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0)
#' x1 <- c(8.22, 3.86, 4.27, 3.37, 5.88, 3.34, 2.92, 1.80, 3.30, 7.17, 3.22, 3.86, 4.27, 3.37, 5.88, 3.34)
#' x2 <- c(4.03, 2.46, 2.04, 2.44, 6.09, 2.91, 1.68, 2.91, 3.87, 1.63, 4.03, 2.46, 2.04, 2.44, 6.09, 2.91)
#' dataLogit <- data.frame(date, y, x1, x2)
#' 
#' # Parameter Forecasts.
#' x1_forecastLogit <- Forecast(
#'    origin   = as.Date(c("2013-12-31", "2013-12-31", "2013-12-31", "2013-12-31")),
#'    future   = as.Date(c("2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31")),
#'    forecast = c(2.11, 6.11, 6.75, 4.30),
#'    realized = NULL,
#'    h_ahead  = NULL
#' )
#' 
#' x2_forecastLogit <- Forecast(
#'    origin   = as.Date(c("2013-12-31", "2013-12-31", "2013-12-31", "2013-12-31")),
#'    future   = as.Date(c("2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31")),
#'    forecast = c(1.98, 7.44, 7.86, 5.98),
#'    realized = NULL,
#'    h_ahead  = NULL
#' )
#' 
#' # Forecasting Function.
#' conditional_forc_general(
#'     model_function = function(data) {glm(y ~ x1 + x2, data = data, family = binomial)},
#'     prediction_function = function(model_function, data) {
#'         names(data) <- c("x1", "x2")
#'         as.vector(predict(model_function, data, type = "response"))
#'     }, 
#'     data = dataLogit,
#'     time_vec = dataLogit$date,
#'     x1_forecastLogit, x2_forecastLogit
#' ) 
#'

#===============================================================================
# Conditional Forecast General
#===============================================================================

#' @export

conditional_forc_general <- function(model_function, prediction_function, data, time_vec, ...) {

  forecasts <- list(...)

  # Input validation.
  if (class(model_function) != "function") {
    stop(paste0(
      "* model_function must be a function that estimates model parameters based on a data argument: \n",
      "model_function = function(data) {glm(y ~ x1 + x2, data = data, family = binomial)}"
    ))
  }

  if ("data" %in% formalArgs(model_function) == FALSE) {
    stop(paste0(
      "* model_function must include a data argument, usually the data.frame used to estimate the model: \n",
      "model_function = function(data) {glm(y ~ x1 + x2, data = data, family = binomial)}"
    ))
  }

  if (class(prediction_function) != "function") {
    stop(paste0(
      "* prediction_function must be a function that generates model predictions based the model_function and data arguments: \n",
      "prediction_function = function(model, data) {as.vector(predict(model, data, type = 'response'))}"
    ))
  }

  if (any(c("model_function", "data") %in% formalArgs(prediction_function)) == FALSE) {
    stop(paste0(
      "* prediction_function must include model_function and data arguments: \n",
      "prediction_function = function(model, data) {as.vector(predict(model, data, type = 'response'))}"
    ))
  }

  if (any(lapply(forecasts, class) != "Forecast")) {
    stop(paste0("* all ellipsis (...) arguments must be of class Forecast.\n",
                "  * ellipsis (...) arguments are currently of the class: ",
                paste(lapply(forecasts, class), collapse = ", ")))
  }

  if (length(unique(lapply(forecasts, function(x) class(x@origin)))) > 1) {
    stop(paste0("* origin values of all forecasts must be of the same class.\n",
                "  * origin values are currently of the class: ",
                paste(lapply(forecasts, function(x) class(x@origin)), collapse = ", ")))
  }

  if (length(unique(lapply(forecasts, function(x) x@future))) > 1) {
    stop("* all forecasts must have the same future values.")
  }

  if (class(time_vec) != class(forecasts[[1]]@origin)) {
    stop(paste0("* The class of time_vec must equal the class of the origin slot of each forecast.\n",
                "  * time_vec is of class: ", class(time_vec),
                "\n  * forecast origin(s) are of class: ", class(forecasts[[1]]@origin)))
  }

  if (class(time_vec) != class(forecasts[[1]]@future)) {
    stop(paste0("* The class of time_vec must equal the class of the future slot of each forecast.\n",
                "  * time_vec is of class: ", class(time_vec),
                "\n  * forecast future(s) are of class: ", class(forecasts[[1]]@future)))
  }
  

  # For each future value, find the latest origin in all forecasts.
  origin_vecs <- lapply(forecasts, function(x) x@origin)
  origin_vec  <- Reduce(pmax, origin_vecs)

  origin   <- origin_vec
  future   <- forecasts[[1]]@future
  realized <- rep(NA_real_, length(forecasts[[1]]@future))
  h_ahead  <- forecasts[[1]]@h_ahead
  
  # Estimate model.
  train_data <- data
  model <- model_function(data = train_data)

  # Extract forecasted parameters that will be used to generate the forecast.
  parameter_values <- sapply(forecasts, function(x) x@forecast)
  parameter_values <- data.frame(parameter_values)

  message("\n Note* the data argument passed to prediction_function has the following form: \n")
  print(parameter_values)
  message("\n\n")

  # Generate forecast.
  forecast <- prediction_function(model_function = model, data = parameter_values)

  Forecast(
    origin = origin, 
    future = future, 
    forecast = forecast,
    realized = realized, 
    h_ahead = h_ahead
  )

}
