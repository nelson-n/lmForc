
#' In-sample general model forecast
#' 
#' \code{is_forc_general} takes a model function, a prediction function, input
#' data for estimating the model, realized values of the dependent variable,
#' and an optional vector of time data associated with the model. The model is
#' estimated once over the entire sample period using the input data and model function.
#' Model parameters are then combined with the input data using the prediction function
#' to generate in-sample forecasts. Returns an in-sample forecast conditional on realized values.
#'
#' @param model_function Function that estimates a model using the \code{data} input.
#' @param prediction_function Function that generates model predictions using \code{model_function} and \code{data} as inputs.
#' @param data Input data for estimating the model.
#' @param realized Vector of realized values of the dependent variable equal in length to the data in \code{data}.
#' @param time_vec Vector of any class that represents time and is equal in length to the length of \code{realized} and \code{data}.
#'
#' @return \code{\link{Forecast}} object that contains the in-sample forecast.
#'
#' @seealso
#' For a detailed example see the help vignette:
#' \code{vignette("lmForc", package = "lmForc")}
#'
#' @examples 
#' 
#' date <- as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
#'                   "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", 
#'                   "2012-03-31", "2012-06-30"))
#' y  <- c(1, 0, 0, 0, 1, 1, 0, 0, 0, 1)
#' x1 <- c(8.22, 3.86, 4.27, 3.37, 5.88, 3.34, 2.92, 1.80, 3.30, 7.17)
#' x2 <- c(4.03, 2.46, 2.04, 2.44, 6.09, 2.91, 1.68, 2.91, 3.87, 1.63)
#' dataLogit <- data.frame(date, y, x1, x2)
#' 
#' is_forc_general(
#'   model_function = function(data) {glm(y ~ x1 + x2, data = data, family = binomial)},
#'   prediction_function = function(model_function, data) {as.vector(predict(model_function, data, type = "response"))}, 
#'   data = dataLogit,
#'   realized = dataLogit$y,
#'   time_vec = dataLogit$date
#' )
#' 

#===============================================================================
# IS Forecast
#===============================================================================

#' @export

is_forc_general <- function(model_function, prediction_function, data, realized, time_vec) {

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

  if (length(time_vec) != length(realized)) {
    stop(paste0(
      "* time_vec must be the same length as the realized vector.",
      "\n  * Length of time_vec: ", length(time_vec),
      "\n  * Length of realized vector: ", length(realized)
    ))
  }

  # Estimate the model based on the model_function and data arguments.
  model <- model_function(data = data)

  # Calculate in sample forecast based on the prediction_function, model_function, and data arguments.
  forecast <- prediction_function(model_function = model, data = data)

  if (class(forecast) != "numeric") {
    stop("* prediction_function must return a numeric vector")
  }

  if (length(forecast) != length(realized)) {
    stop(paste0(
      "* Length of forecast returned by the prediction_function() must equal the length of realized vector: ",
      "\n  * Length of forecast: ", length(forecast),
      "\n  * Length of realized vector: ", length(realized)
    ))
  } 

  # Return in-sample forecast.
  Forecast(
    origin   = time_vec,
    future   = time_vec,
    forecast = forecast,
    realized = realized,
    h_ahead  = 0
  )

}
