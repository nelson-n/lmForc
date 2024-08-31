
#' Out-of-sample general model forecast conditioned on vintage forecasts
#'
#' \code{oos_vintage_forc_general} takes a model function, a prediction function,
#' input data for estimating the model, realized values of the dependent variable,
#' a vector of time data associated with the model, a forecast for each parameter
#' in the model, and an optional integer number of past periods to estimate the
#' model over. For each period in the vintage forecasts, model parametes are 
#' estimated with data up to the current period minus the number of periods 
#' specified in \code{estimation_window}. If \code{estimation_window} is left
#' \code{NULL} then the model is estimated with all available data up to the
#' current period. Model parameters are then combined with vintage forecast values
#' to generate a forecast. Returns an out-of-sample forecast conditional on vintage
#' forecasts that \strong{would} have been available at the forecast origin. Replicates
#' the forecasts that a conditional forecasting model would have produced in real time.
#' 
#' @param model_function Function that estimates a model using the \code{data} input.
#' @param prediction_function Function that generates model predictions using \code{model_function}
#'   and \code{data} arguments. Note* that the \code{data} argument passed to the prediction_function
#'   takes the form of a data.frame with a number of columns equal to the number of 
#'   input vintage forecasts passed by the user. The prediction_function needs to be able to take
#'   this input format and generate a prediction based on it.
#' @param data Input data for estimating the model.
#' @param realized Vector of realized values of the dependent variable equal in length
#'   to the data in \code{data}.
#' @param time_vec Vector of any class that represents time and is equal in length to 
#'   the length of \code{realized} and \code{data}.
#' @param ... Set of forecasts of class Forecast, one forecast for each
#'   parameter in the linear model.
#' @param estimation_window Integer representing the number of past periods 
#'   that the linear model should be estimated over in each period. 
#'   
#' @return \code{\link{Forecast}} object that contains the out-of-sample
#'   forecast.
#'
#' @seealso
#' For a detailed example see the help vignette:
#' \code{vignette("lmForc", package = "lmForc")}
#'
#' @examples
#' # Estimation Data.
#' date <- as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
#'                   "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", 
#'                   "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31",
#'                   "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31"))
#' y  <- c(1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0)
#' x1 <- c(8.22, 3.86, 4.27, 3.37, 5.88, 3.34, 2.92, 1.80, 3.30, 7.17, 3.22, 3.86, 
#'     4.27, 3.37, 5.88, 3.34)
#' x2 <- c(4.03, 2.46, 2.04, 2.44, 6.09, 2.91, 1.68, 2.91, 3.87, 1.63, 4.03, 2.46, 
#'     2.04, 2.44, 6.09, 2.91)
#' dataLogit <- data.frame(date, y, x1, x2)
#' 
#' # Vintage Forecasts.
#' x1_forecast_vintageLogit <- Forecast(
#'    origin   = as.Date(c("2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30")),
#'    future   = as.Date(c("2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30")),
#'    forecast = c(6.34, 4.17, 2.98, 1.84),
#'    realized = c(5.88, 3.34, 2.92, 1.80),
#'    h_ahead  = 4L
#' )
#' 
#' x2_forecast_vintageLogit <- Forecast(
#'    origin   = as.Date(c("2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30")),
#'    future   = as.Date(c("2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30")),
#'    forecast = c(7.32, 3.22, 2.21, 2.65),
#'    realized = c(6.09, 2.91, 1.68, 2.91),
#'    h_ahead  = 4L
#' )
#' 
#' # Forecasting function.
#' oos_vintage_forc_general(
#'     model_function = function(data) {glm(y ~ x1 + x2, data = data, family = binomial)},
#'     prediction_function = function(model_function, data) {
#'         names(data) <- c("x1", "x2")
#'         as.vector(predict(model_function, data, type = "response"))
#'     }, 
#'     data = dataLogit,
#'     realized = dataLogit$y,
#'     time_vec = dataLogit$date,
#'     x1_forecast_vintageLogit, x2_forecast_vintageLogit,
#'     estimation_window = NULL
#' )
#' 

#===============================================================================
# OOS Vintage Forecast
#===============================================================================

#' @export

oos_vintage_forc_general <- function(model_function, prediction_function, data, 
    realized, time_vec, ..., estimation_window = NULL) {

  forecasts <- list(...)

  # Input validation.
  if (inherits(model_function, "function") == FALSE) {
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

  if (inherits(prediction_function, "function") == FALSE) {
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
  
  if (is.null(estimation_window) == FALSE & is.integer(estimation_window) == FALSE) {
    stop("* estimation_window must be an integer: estimation_end = 20L")
  }
  
  if (is.null(estimation_window) == FALSE & is.integer(estimation_window) == FALSE) {
    stop("* estimation_window must be of length one: estimation_end = 20L")
  }

  # For each future value, find the latest origin in all forecasts.
  origin_vecs <- lapply(forecasts, function(x) x@origin)
  origin_vec  <- Reduce(pmax, origin_vecs)

  origin   <- origin_vec
  future   <- forecasts[[1]]@future
  forecast <- vector(mode = "double", length = length(origin_vec))
  realized <- realized[match(forecasts[[1]]@future, time_vec)]
  h_ahead  <- forecasts[[1]]@h_ahead
  

  # Run forecast loop.
  for (i in 1:length(origin_vec)) {

    # Subset train_data by estimation_window parameter.
    if (is.null(estimation_window) == TRUE) {
      train_data <- data[time_vec <= origin_vec[i], ] 
    } else {
      train_data <- data[time_vec <= origin_vec[i], ]
      if ((nrow(train_data) - estimation_window) >= 1) {
        train_data <- train_data[((nrow(train_data) - estimation_window + 1):nrow(train_data)), ]
      }
    }

    # Estimate model.
    model <- model_function(data = train_data)

    # Extract forecasted parameters that will be used to generate the forecast.
    parameter_values <- sapply(forecasts, function(x) x@forecast[i])
    parameter_values <- data.frame(t(parameter_values))

    if (i == 1) {
        message("\n Note* the data argument passed to prediction_function has the following form: \n")
        print(parameter_values)
        message("\n\n")
    }

    # Generate forecast.
    forecast[[i]] <- prediction_function(model_function = model, data = parameter_values)

  }

  Forecast(
    origin = origin, 
    future = future, 
    forecast = forecast,
    realized = realized, 
    h_ahead = h_ahead
  )

}
