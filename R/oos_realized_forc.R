
#' Out-of-sample linear model forecast conditioned on realized values
#'
#' \code{oos_realized_forc} takes a linear model call, an integer number of
#' periods ahead to forecast, a period to end the initial coefficient estimation
#' and begin forecasting, an optional vector of time data associated with
#' the linear model, and an optional integer number of past periods to estimate
#' the linear model over. The linear model is originally estimated with data up 
#' to \code{estimation_end} minus the number of periods specified in 
#' \code{estimation_window}. If \code{estimation_window} is left \code{NULL} 
#' then the linear model is estimated with all available data up to 
#' \code{estimation_end}. Coefficients are multiplied by realized values of the
#' covariates \code{h_ahead} periods ahead to create an \code{h_ahead} period
#' ahead forecast. This process is iteratively repeated for each period after
#' \code{estimation_end} with coefficients updating in each period. Returns an
#' out-of-sample forecast conditional on realized values that \strong{would not}
#' have been available at the forecast origin. Optionally returns the coefficients 
#' used to create each forecast. Tests the out-of-sample performance of a linear 
#' model had it been conditioned on perfect information.
#'
#' @param lm_call Linear model call of the class lm.
#' @param h_ahead Integer representing the number of periods ahead that is being
#'   forecasted.
#' @param estimation_end Value of any class representing when to end the initial
#'   coefficient estimation period and begin forecasting.
#' @param time_vec Vector of any class that is equal in length to the data
#'   in \code{lm_call}.
#' @param estimation_window Integer representing the number of past periods 
#'   that the linear model should be estimated over in each period. 
#' @param return_betas Boolean, selects whether the coefficients used in each 
#'   period to create the forecast are returned. If TRUE, a data frame of 
#'   betas is returned to the Global Environment.
#'
#' @return \code{\link{Forecast}} object that contains the out-of-sample
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
#' x1 <- c(4.22, 3.86, 4.27, 5.60, 5.11, 4.31, 4.92, 5.80, 6.30, 4.17)
#' x2  <- c(10.03, 10.49, 10.85, 10.47, 9.09, 10.91, 8.68, 9.91, 7.87, 6.63)
#' data <- data.frame(date, y, x1, x2)
#' 
#' oos_realized_forc(
#'   lm_call = lm(y ~ x1 + x2, data),
#'   h_ahead = 2L,
#'   estimation_end = as.Date("2011-03-31"),
#'   time_vec = data$date,
#'   estimation_window = NULL,
#'   return_betas = FALSE
#' )
#' 

#===============================================================================
# OOS Realized Forecast
#===============================================================================

#' @export

oos_realized_forc <- function(lm_call, h_ahead, estimation_end, time_vec = NULL,
                              estimation_window = NULL, return_betas = FALSE) {
  
  # Input validation.
  if (inherits(lm_call , "lm") == FALSE) {
    stop("* lm_call must be must be of the lm function form: lm_call = lm(y = x1 + x2, data)")
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
  
  if (is.null(time_vec) == FALSE & length(time_vec) != nrow(lm_call$model)) {
    stop(paste0("* Length of time_vec must equal the number of rows in the lm_call data.\n",
                "  * Length of time_vec: ", length(time_vec),
                "\n  * Number of rows in lm_call data: ", nrow(lm_call$model),
                "\n  * This may be caused by NAs in the data."))
  }
  
  if (is.null(estimation_window) == FALSE & is.integer(estimation_window) == FALSE) {
    stop("* estimation_window must be an integer: estimation_end = 20L")
  }
  
  if (is.null(estimation_window) == FALSE & is.integer(estimation_window) == FALSE) {
    stop("* estimation_window must be of length one: estimation_end = 20L")
  }
  
  # Find OOS forecast period and prepare forecasting loop.
  if (is.null(time_vec) == TRUE) {
    time_vec <- 1:nrow(lm_call$model)
  }
  
  if (is.null(time_vec) == FALSE) {
    estimation_end <- which(time_vec < estimation_end)
    estimation_end <- estimation_end[length(estimation_end)] + 1
  }
  
  # Verify there is enough data after estimation_end to produce a forecast.
  if (estimation_end > (nrow(lm_call$model) - h_ahead)) {
    stop(paste0("* Not enough data after estimation_end to produce a forecast.\n",
                "  * Decrease estimation_end, decrease h_ahead, or add additional observations."))
  }
  
  oos_index <- estimation_end:(nrow(lm_call$model) - h_ahead)
  
  lm_call$call$data <- quote(train_data)
  
  origin   <- time_vec[oos_index]
  future   <- time_vec[oos_index + h_ahead]
  forecast <- vector(mode = "double", length = length(oos_index))
  realized <- lm_call$model[[1]][oos_index + h_ahead]
  
  betas <- vector(mode = "list", length = length(oos_index))
  
  # Run forecasting loop.
  for (i in 1:length(oos_index)) {
    
    index <- oos_index[[i]]
    
    # Subset train_data by estimation_window parameter.
    if (is.null(estimation_window) == TRUE) {
      train_data <- lm_call$model[1:index, ]  
    } else {
      if ((index - estimation_window) < 1) {
        train_data <- lm_call$model[1:index, ]  
      } else {
        train_data <- lm_call$model[(index - estimation_window):index, ] 
      }
    }
    
    train_lm   <- eval(lm_call$call)
    coefs      <- train_lm$coefficients
    betas[[i]] <- coefs
    
    realized_vals <- lm_call$model[index + h_ahead, ]
    forecast[[i]] <- coefs[[1]] + sum(coefs[2:length(coefs)] * realized_vals[2:length(realized_vals)])
    
  }
  
  if (return_betas == TRUE) {
    betas <- data.frame(do.call(rbind, betas))
    betas <- cbind(origin, betas)
    colnames(betas) <- paste0(colnames(betas), "_beta")
    colnames(betas)[1:2] <- c("origin", "intercept")
    betas <<- betas
  }
  
  Forecast(origin = origin, future = future, forecast = forecast,
           realized = realized, h_ahead = h_ahead)
  
}
