
#' Out-of-sample general model forecast conditioned on realized values
#'
#' \code{oos_realized_forc} takes a model function, a prediction function, input 
#' data for estimating the model, realized values of the dependent variable, an
#' integer number of periods ahead to forecast, a period to end the initial coefficient
#' estimation and begin forecasting, a vector of time data associated with the model,
#' and an optional integer number of past periods to estimate the model over. The
#' model is originally estimated using the input data and model function with data 
#' up to \code{estimation_end} minus the the number of periods specified in \code{estimation_window}. 
#' If \code{estimation_window} is left \code{NULL} then the model is estimated with all 
#' available data up to \code{estimation_end}. Model parameters are then combined with realized
#' values of the input data \code{h_ahead} periods ahead to generate an \code{h_ahead} period
#' ahead forecast. This process is iteratively repeated for each period after \code{estimation_end}
#' with model parameters updating in each period. Returns an out-of-sample forecast
#' conditional on realized values that \strong{would not} have been available at the forecast origin.
#' Tests the out-of-sample performance of a model had it been conditioned on perfect information.
#' 
#' @param model_function Function that estimates a model using the \code{data} input.
#' @param prediction_function Function that generates model predictions using \code{model_function}
#'   and \code{data} as inputs.
#' @param data Input data for estimating the model.
#' @param realized Vector of realized values of the dependent variable equal in length
#'   to the data in \code{data}.
#' @param h_ahead Integer representing the number of periods ahead that is being
#'   forecasted.
#' @param estimation_end Value of any class representing when to end the initial
#'   coefficient estimation period and begin forecasting.
#' @param time_vec Vector of any class that represents time and is equal in length to 
#'   the length of \code{realized} and \code{data}.
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
#' 
#' date <- as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
#'                   "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", 
#'                   "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31",
#'                   "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31"))
#' y  <- c(1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0)
#' x1 <- c(8.22, 3.86, 4.27, 3.37, 5.88, 3.34, 2.92, 1.80, 3.30, 7.17, 3.22, 3.86, 4.27, 3.37, 5.88, 3.34)
#' x2 <- c(4.03, 2.46, 2.04, 2.44, 6.09, 2.91, 1.68, 2.91, 3.87, 1.63, 4.03, 2.46, 2.04, 2.44, 6.09, 2.91)
#' dataLogit <- data.frame(date, y, x1, x2)
#'
#' forc <- oos_realized_forc_general(
#'     model_function = function(data) {glm(y ~ x1 + x2, data = data, family = binomial)},
#'     prediction_function = function(model_function, data) {as.vector(predict(model_function, data, type = "response"))}, 
#'     data = dataLogit,
#'     realized = dataLogit$y,
#'     h_ahead = 2L,
#'     estimation_end = as.Date("2012-06-30"),
#'     time_vec = dataLogit$date,
#'     estimation_window = NULL
#' )
#'

#===============================================================================
# OOS Realized Forecast
#===============================================================================

#' @export

oos_realized_forc_general <- function(model_function, prediction_function, data,
    realized, h_ahead, estimation_end, time_vec, estimation_window = NULL) {

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

  if (is.integer(h_ahead) != TRUE) {
    stop("* h_ahead must be an integer: h_ahead = 4L")
  }
  
  if (length(h_ahead) > 1) {
    stop("* h_ahead must be of length one: h_ahead = 4L")
  }
  
  if (length(time_vec) != length(realized)) {
    stop(paste0(
      "* time_vec must be the same length as the realized vector.",
      "\n  * Length of time_vec: ", length(time_vec),
      "\n  * Length of realized vector: ", length(realized)
    ))
  }

  if (class(estimation_end) != class(time_vec)) {
    stop(paste0("* The class of estimation_end must equal the class of time_vec.",
                "\n  * estimation_end is of class: ", class(estimation_end),
                "\n  * time_vec is of class: ", class(time_vec)))
  }
  
  if (is.null(estimation_window) == FALSE & is.integer(estimation_window) == FALSE) {
    stop("* estimation_window must be an integer: estimation_end = 20L")
  }
  
  if (is.null(estimation_window) == FALSE & is.integer(estimation_window) == FALSE) {
    stop("* estimation_window must be of length one: estimation_end = 20L")
  }
  
  estimation_end <- which(time_vec < estimation_end)
  estimation_end <- estimation_end[length(estimation_end)] + 1
  
  # Verify there is enough data after estimation_end to produce a forecast.
  if (estimation_end > (length(time_vec) - h_ahead)) {
    stop(paste0("* Not enough data after estimation_end to produce a forecast.\n",
                "  * Decrease estimation_end, decrease h_ahead, or add additional observations."))
  }
  
  oos_index <- estimation_end:(length(time_vec) - h_ahead)
  
  origin   <- time_vec[oos_index]
  future   <- time_vec[oos_index + h_ahead]
  forecast <- vector(mode = "double", length = length(oos_index))
  realized <- realized[oos_index + h_ahead]
  
  # Run forecasting loop.
  for (i in 1:length(oos_index)) {
    
    index <- oos_index[[i]]
    
    # Subset train_data by estimation_window parameter.
    if (is.null(estimation_window) == TRUE) {
      train_data <- data[1:index, ]
    } else {
      if ((index - estimation_window) < 1) {
        train_data <- data[1:index, ]
      } else {
        train_data <- data[(index - estimation_window):index, ]
      }
    }

    # Estimate model.
    model <- model_function(data = train_data)

    # Calculate h_ahead forecast conditioned on realized values.
    realized_vals <- data[index + h_ahead, ]
    forecast[[i]] <- prediction_function(model_function = model, data = realized_vals)
    
  }
  
  Forecast(
    origin = origin, 
    future = future, 
    forecast = forecast,
    realized = realized,
    h_ahead = h_ahead
  )

}
