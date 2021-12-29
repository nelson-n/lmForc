
#' Historical average forecast
#'
#' \code{historical_average_forc} takes an average function, a vector of realized 
#' values, an integer number of periods ahead to forecast, a period to end the initial 
#' average estimation and begin forecasting, an optional vector of time data 
#' associated with the realized values, and an optional integer number of past
#' periods to estimate the average over.  The historical average is originally 
#' calculated with realized values up to \code{estimation_end} minus the number 
#' of periods specified in \code{estimation_window}. If \code{estimation_window}
#' is left \code{NULL} then the historical average is calculated with all available
#' realized values up to \code{estimation_end}. In each period the historical
#' average is set as the \code{h_ahead} period ahead forecast. This process is 
#' iteratively repeated for each period after \code{estimation_end} with the 
#' historical average updating in each period as more information would have 
#' become available to the forecaster. Returns a historical average forecast where
#' the \code{h_ahead} period ahead forecast is simply the historical average or
#' rolling window average of the series being forecasted. 
#'
#' @param avg_function Character, either "mean" or "median". Selects whether 
#'   forecasts are made using the historical mean or historical median of 
#'   the series.
#' @param realized_vec Vector of realized values. This is the series that is 
#'   being forecasted.
#' @param h_ahead Integer representing the number of periods ahead that is being
#'   forecasted.
#' @param estimation_end Value of any class representing when to end the initial
#'   average estimation period and begin forecasting.
#' @param time_vec Vector of any class that is equal in length to the
#'   \code{realized_vec} vector.
#' @param estimation_window Integer representing the number of past periods 
#'   that the historical average should be estimated over in each period. 
#'
#' @return \code{\link{Forecast}} object that contains the historical average
#'   forecast.
#'
#' @seealso
#' For a detailed example see the help vignette:
#' \code{vignette("lmForc", package = "lmForc")}
#'
#' @examples 
#' date <- as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
#'                   "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", 
#'                   "2012-03-31", "2012-06-30"))
#' y  <- c(1.09, 1.71, 1.09, 2.46, 1.78, 1.35, 2.89, 2.11, 2.97, 0.99)
#' data <- data.frame(date, y)
#'
#' historical_average_forc(
#'   avg_function = "mean",
#'   realized_vec = data$y,
#'   h_ahead = 2L,
#'   estimation_end = as.Date("2011-03-31"),
#'   time_vec = data$date,
#'   estimation_window = 4L
#' )
#'  
#' historical_average_forc(
#'   avg_function = "median",
#'   realized_vec = data$y,
#'   h_ahead = 4L,
#'   estimation_end = 4L
#' )
#'  
#' @importFrom stats median
#' 
 
#===============================================================================
# Historical Average Forecast
#===============================================================================

#' @export

historical_average_forc <- function(avg_function, realized_vec, h_ahead, 
  estimation_end, time_vec = NULL, estimation_window = NULL) {
  
  # Input validation.
  if (!(avg_function %in% c("mean", "median"))) {
    stop('* avg_function must be either "mean" or "median": avg_function = "mean"')
  }
  
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
  
  # Select avg_function.
  if (avg_function == "mean") {
    avg_func <- eval(base::mean)
  }
  
  if (avg_function == "median") {
    avg_func <- eval(stats::median)
  }
  
  # Prepare inputs for forecasting loop.
  if (is.null(time_vec) == TRUE) {
    time_vec <- 1:length(realized_vec)
  }
  
  if (is.null(time_vec) == FALSE) {
    estimation_end <- which(time_vec < estimation_end)
    estimation_end <- estimation_end[length(estimation_end)] + 1
  }
  
  # Verify there is enough data after estimation_end to produce a forecast.
  if (estimation_end > (length(realized_vec) - h_ahead)) {
    stop(paste0("* Not enough data after estimation_end to produce a forecast.\n",
                "  * Decrease estimation_end, decrease h_ahead, or add additional observations."))
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
  
    forecast[[i]] <- avg_func(train_data, na.rm = TRUE)
  
  }
  
  Forecast(origin = origin, future = future, forecast = forecast,
           realized = realized, h_ahead = h_ahead)
  
}
