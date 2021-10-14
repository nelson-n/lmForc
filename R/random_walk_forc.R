
#' Random walk forecast
#'
#' \code{random_walk_forc} takes a vector of realized values, an integer
#' number of periods ahead to forecast, and an optional vector of time data 
#' associated with the realized values. In each period, the current period value 
#' of the \code{realized_vec} series is set as the \code{h_ahead} period ahead forecast.
#' Returns a random walk forecast where the \code{h_ahead} period ahead forecast
#' is simply the present value of the series being forecasted.
#'
#' @param realized_vec Vector of realized values. This is the series that is 
#'   being forecasted.
#' @param h_ahead Integer representing the number of periods ahead that is being
#'   forecasted.
#' @param time_vec Vector of any class that is equal in length to the
#'   \code{realized_vec} vector.
#'
#' @return \code{\link{Forecast}} object that contains the random walk forecast.
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
#' random_walk_forc(
#'   realized_vec = data$y,
#'   h_ahead = 4L,
#'   time_vec = data$date 
#' )
#' 

#===============================================================================
# Random Walk Forecast
#===============================================================================

#' @export

random_walk_forc <- function(realized_vec, h_ahead, time_vec = NULL) {
  
  # Input validation.
  if (is.integer(h_ahead) != TRUE) {
    stop("* h_ahead must be an integer: h_ahead = 4L")
  }

  if (length(h_ahead) > 1) {
    stop("* h_ahead must be of length one: h_ahead = 4L")
  }
  
  if (is.null(time_vec) == FALSE & length(time_vec) != length(realized_vec)) {
    stop(paste0("* Length of time_vec must equal the length of realized_vec.\n",
                "  * Length of time_vec: ", length(time_vec),
                "\n  * Length of realized_vec: ", length(realized_vec)))
  }
  
  if (is.null(time_vec) == TRUE) {
    time_vec <- 1:length(realized_vec)
  }
  
  # Create random walk forecast.
  forecast <- realized_vec[1:(length(realized_vec) - h_ahead)]
  origin   <- time_vec[1:(length(time_vec) - h_ahead)]
  realized <- realized_vec[(1 + h_ahead):length(realized_vec)]
  future   <- time_vec[(1 + h_ahead):length(time_vec)]
  
  Forecast(origin = origin, future = future, forecast = forecast,
           realized = realized, h_ahead = h_ahead)
  
}
