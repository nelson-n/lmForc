
#' Autoregression forecast
#'
#' \code{autoreg_forc} takes a vector of realized values, an integer number of 
#' periods ahead to forecast, a period to end the initial model estimation and 
#' begin forecasting, an optional vector of time data associated with the
#' realized values, and an optional integer number of past periods to estimate 
#' the model over. An AR(\code{h_ahead}) autoregressive model is estimated with realized 
#' values up to \code{estimation_end} minus the number of periods specified in
#' \code{estimation_window}. If \code{estimation_window} is left \code{NULL} 
#' then the autoregressive model is estimated with all realized values up to 
#' \code{estimation_end}. The AR(\code{h_ahead}) model is estimated by regressing the
#' realized values on the same realized values that have been lagged by 
#' \code{h_ahead} steps. The AR coefficient of this model is multiplied by the 
#' present period realized value to create a forecast for \code{h_ahead} periods 
#' ahead. This process is iteratively repeated for each period after 
#' \code{estimation_end} with AR model coefficients updating as more information 
#' would have become available to the forecaster. Returns an autoregression 
#' forecast based on information that \strong{would} have been available at the
#' forecast origin.
#' 
#' @param realized_vec Vector of realized values. This is the series that is 
#'   being forecasted.
#' @param h_ahead Integer representing the number of periods ahead that is being
#'   forecasted.
#' @param estimation_end Value of any class representing when to end the initial
#'   coefficient estimation period and begin forecasting.
#' @param time_vec Vector of any class that is equal in length to the
#'   \code{realized_vec} vector.
#' @param estimation_window Integer representing the number of past periods 
#'   that the autoregressive model should be estimated over in each period. 
#'
#' @return \code{\link{Forecast}} object that contains the autoregression
#'   forecast.
#'
#' @seealso
#' For a detailed example see the help vignette:
#' \code{vignette("lmForc", package = "lmForc")}
#'
#' @examples \dontrun{
#'
#' autoreg_forc(
#'   realized_vec = data$y,
#'   h_ahead = 2L,
#'   estimation_end = as.Date("2011-06-30"),
#'   time_vec = data$date,
#'   estimation_window = NULL
#' )
#'
#' autoreg_forc(
#'   realized_vec = data$y,
#'   h_ahead = 2L,
#'   estimation_end = 4L
#' )
#'  
#' }
#' 
#' @importFrom stats lm
#' 
 
#===============================================================================
# Autoregression Forecast
#===============================================================================

#' @export

autoreg_forc <- function(realized_vec, h_ahead, estimation_end, time_vec = NULL,
  estimation_window = NULL) {
  
  # Input validation.
  if (is.integer(h_ahead) != TRUE) {
    stop("* h_ahead must be an integer: h_ahead = 4L")
  }

  if (length(h_ahead) > 1) {
    stop("* h_ahead must be of length one: h_ahead = 4L")
  }

  if (is.null(time_vec) == TRUE & is.integer(estimation_end) != TRUE) {
    stop("* If time_vec is NULL then estimation_end must be an integer: estimation_end = 50L")
  }

  if (is.null(time_vec) == FALSE & class(estimation_end) != class(time_vec)) {
    stop(paste0("* The class of estimation_end must equal the class of time_vec.\n",
                "  * estimation_end is of class: ", class(estimation_end),
                "\n  * time_vec is of class: ", class(time_vec)))
  }

  if (is.null(time_vec) == FALSE & length(time_vec) != length(realized_vec)) {
    stop(paste0("* Length of time_vec must equal the length of realized_vec.\n",
                "  * Length of time_vec: ", length(time_vec),
                "\n  * Length of realized_vec: ", length(realized_vec)))
  }

  if (is.null(estimation_window) == FALSE & is.integer(estimation_window) == FALSE) {
    stop("* estimation_window must be an integer: estimation_end = 20L")
  }
  
  if (is.null(estimation_window) == FALSE & is.integer(estimation_window) == FALSE) {
    stop("* estimation_window must be of length one: estimation_end = 20L")
  }
  
  # Prepare inputs for forecasting loop.
  if (is.null(time_vec) == TRUE) {
    time_vec <- 1:length(realized_vec)
  }
  
  if (is.null(time_vec) == FALSE) {
    estimation_end <- which(time_vec <= estimation_end)
    estimation_end <- estimation_end[length(estimation_end)]
  }

  # Verify there is enough data before estimation_end to estimate the model.
  if (is.null(estimation_window) == FALSE) {
    if (h_ahead >= estimation_window) {
      stop("* estimation_window must be larger than h_ahead to estimate the autoregressive model.")
    }
  } 

  if (h_ahead >= estimation_end) {
    stop(paste0("* Not enough data to estimate the autoregressive model in the initial estimation period.\n",
                "  * Increase estimation_end or decrease h_ahead to allow for initial model estimation."))
  }
  
  # Verify there is enough data after estimation_end to produce a forecast.
  if (estimation_end > (length(realized_vec) - h_ahead)) {
    stop(paste0("* Not enough data after estimation_end to produce a forecast.\n",
                "  * Decrease estimation_end, decrease h_ahead, or add additional observations (*these may be NA observations)."))
  }
  
  # Function to lag a vector n steps.
  vector_lag <- function(vector, n) {
    vector <- c(rep(NA, n), vector[1:(length(vector) - n)])
    return(vector)
  }
  
  oos_index <- estimation_end:(length(realized_vec) - h_ahead)
  
  origin   <- time_vec[oos_index]
  future   <- time_vec[oos_index + h_ahead]
  forecast <- vector(mode = "double", length = length(oos_index))
  realized <- realized_vec[oos_index + h_ahead]
  
  # Run forecasting loop.
  for (i in 1:length(oos_index)) {
  
    index <- oos_index[[i]]
    
    # Subset train_data by estimation_window parameter.
    if (is.null(estimation_window) == TRUE) {
      train_data <- realized_vec[1:index]  
    } else {
      if ((index - estimation_window) < 1) {
        train_data <- realized_vec[1:index]  
      } else {
        train_data <- realized_vec[(index - estimation_window):index] 
      }
    }
    
    lag_train_data <- vector_lag(train_data, n = h_ahead)
    train_lm       <- lm(train_data ~ lag_train_data)
    coefs          <- train_lm$coefficients
    forecast[[i]]  <- coefs[[1]] + coefs[[2]] * train_data[length(train_data)]
  
  }
  
  Forecast(origin = origin, future = future, forecast = forecast,
           realized = realized, h_ahead = h_ahead)
  
}
