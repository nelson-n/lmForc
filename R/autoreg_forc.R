
#' Autoregression forecast
#'
#' \code{autoreg_forc} takes a vector of realized values, an integer number of 
#' periods ahead to forecast, an integer number of lags to include in the 
#' autoregressive model, a period to end the initial model estimation and 
#' begin forecasting, an optional vector of time data associated with the
#' realized values, and an optional integer number of past periods to estimate 
#' the model over. An AR(\code{ar_lags}) autoregressive model is originally estimated 
#' with realized values up to \code{estimation_end} minus the number of periods specified 
#' in \code{estimation_window}. If \code{estimation_window} is left \code{NULL} 
#' then the autoregressive model is estimated with all realized values up to 
#' \code{estimation_end}. The AR(\code{ar_lags}) model is estimated by regressing the
#' realized values on the same realized values that have been lagged by 
#' one to \code{ar_lags} steps. The AR coefficients of this model are multiplied by  
#' lagged values and the present period realized value to create a forecast for 
#' one period ahead. If \code{h_ahead} is greater than one, this process of 
#' forecasting one period ahead is iteratively repeated so that the two period 
#' ahead forecast conditions on the one period ahead forecasted value and so 
#' on until a \code{h_ahead} forecast is obtained. This forecasting process is 
#' repeated for each period after \code{estimation_end} with AR model coefficients 
#' updating as more information would have become available to the forecaster. 
#' Optionally returns the coefficients used to create each forecast.
#' Returns an autoregression forecast based on information that \strong{would} 
#' have been available at the forecast origin and replicates the forecasts that an 
#' AR model would have produced in real-time.
#' 
#' @param realized_vec Vector of realized values. This is the series that is 
#'   being forecasted.
#' @param h_ahead Integer representing the number of periods ahead that is being
#'   forecasted.
#' @param ar_lags Integer representing the number of lags included in the AR model.
#' @param estimation_end Value of any class representing when to end the initial
#'   coefficient estimation period and begin forecasting.
#' @param time_vec Vector of any class that is equal in length to the
#'   \code{realized_vec} vector.
#' @param estimation_window Integer representing the number of past periods 
#'   that the autoregressive model should be estimated over in each period. 
#' @param return_betas Boolean, selects whether the coefficients used in each 
#'   period to create the forecast are returned. If TRUE, a data frame of 
#'   betas is returned to the Global Environment.
#'  
#' @return \code{\link{Forecast}} object that contains the autoregression
#'   forecast.
#'
#' @seealso
#' For a detailed example see the help vignette:
#' \code{vignette("lmForc", package = "lmForc")}
#'
#' @examples 
#' date <- as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
#'                   "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31",
#'                   "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31"))
#' y <- c(1.09, 1.71, 1.09, 2.46, 1.78, 1.35, 2.89, 2.11, 2.97, 0.99, 1.31, 2.33)
#' data <- data.frame(date, y)
#' 
#' autoreg_forc(
#'   realized_vec = data$y,
#'   h_ahead = 1L,
#'   ar_lags = 2L,
#'   estimation_end = as.Date("2011-06-30"),
#'   time_vec = data$date,
#'   estimation_window = 4L,
#'   return_betas = FALSE
#' )
#' 
#' autoreg_forc(
#'   realized_vec = data$y,
#'   h_ahead = 4L,
#'   ar_lags = 2L,
#'   estimation_end = 4L,
#'   time_vec = NULL,
#'   estimation_window = NULL
#' )
#' 
#' @importFrom stats lm
#'   
  
#===============================================================================
# Autoregression Forecast
#===============================================================================

#' @export  
  
autoreg_forc <- function(realized_vec, h_ahead, ar_lags, estimation_end, 
  time_vec = NULL, estimation_window = NULL, return_betas = FALSE) {
  
   # Input validation.
  if (is.integer(h_ahead) != TRUE) {
    stop("* h_ahead must be an integer: h_ahead = 4L")
  }

  if (length(h_ahead) > 1) {
    stop("* h_ahead must be of length one: h_ahead = 4L")
  }
  
  if (is.integer(ar_lags) != TRUE) {
    stop("* ar_lags must be an integer: ar_lags = 4L")
  }

  if (length(ar_lags) > 1) {
    stop("* ar_lags must be of length one: ar_lags = 4L")
  }
  
  if (is.null(estimation_window) == FALSE & is.integer(estimation_window) == FALSE) {
    stop("* estimation_window must be an integer: estimation_end = 20L")
  }
  
  if (is.null(estimation_window) == FALSE & is.integer(estimation_window) == FALSE) {
    stop("* estimation_window must be of length one: estimation_end = 20L")
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

  # Prepare inputs for forecasting loop.
  if (is.null(time_vec) == TRUE) {
    time_vec <- 1:length(realized_vec)
  }
  
  if (is.null(time_vec) == FALSE) {
    estimation_end <- which(time_vec < estimation_end)
    estimation_end <- estimation_end[length(estimation_end)] + 1
  }

  # Verify there is enough data before estimation_end to estimate the model.
  if (is.null(estimation_window) == FALSE) {
    if (ar_lags >= estimation_window) {
      stop("* estimation_window must be larger than ar_lags to estimate the autoregressive model.")
    }
  } 

  if (ar_lags >= estimation_end) {
    stop(paste0("* Not enough data to estimate the autoregressive model in the initial estimation period.\n",
                "  * Increase estimation_end or decrease ar_lags to allow for initial model estimation."))
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
  
  betas <- vector(mode = "list", length = length(oos_index))
  
  # Create ar lagged training data.
  full_train_data <- vector(mode = "list", length = ar_lags)
  
  for (i in 1:ar_lags) {
    full_train_data[[i]] <- vector_lag(realized_vec, n = i)
  }
  
  full_train_data <- do.call("cbind", full_train_data)
  full_train_data <- cbind(realized_vec, full_train_data)
  
  # Run forecasting loop.
  for (i in 1:length(oos_index)) {
    
    index <- oos_index[[i]]

    # Subset train_data by estimation_window parameter.
    if (is.null(estimation_window) == TRUE) {
      train_data <- full_train_data[1:index, ]  
    } else {
      if ((index - estimation_window) < 1) {
        train_data <- full_train_data[1:index, ]  
      } else {
        train_data <- full_train_data[(index - estimation_window):index, ] 
      }
    }
    
    # Estimate AR model.
    train_lm <- lm(train_data[, 1] ~ train_data[, 2:(ar_lags + 1)])
    coefs    <- train_lm$coefficients
    betas[[i]] <- coefs
    
    # Iteratively forecast h_ahead periods into the future.
    forc_data <- train_data[(nrow(train_data) - ar_lags + 1):nrow(train_data), 1]
    
    for (h in 1:h_ahead) {
      last_forc <- forc_data[(length(forc_data) - ar_lags + 1):length(forc_data)]
      forc_data <- c(forc_data, coefs[[1]] + sum(coefs[2:(ar_lags + 1)] * rev(last_forc)))
    }
    
    forecast[[i]] <- forc_data[[length(forc_data)]]
    
  }
  
  if (return_betas == TRUE) {
    betas <- data.frame(do.call(rbind, betas))
    betas <- cbind(origin, betas)
    colnames(betas)[1:2] <- c("origin", "intercept")
    colnames(betas)[3:(ar_lags + 2)] <- paste0("lag", c(1:ar_lags), "_beta")
    betas <<- betas
  }
  
  Forecast(origin = origin, future = future, forecast = forecast,
           realized = realized, h_ahead = h_ahead)
  
}
