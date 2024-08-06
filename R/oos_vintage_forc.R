
#' Out-of-sample linear model forecast conditioned on vintage forecasts
#'
#' \code{oos_vintage_forc} takes a linear model call, a vector of time data
#' associated with the linear model, a forecast for each covariate in the
#' linear model, and an optional integer number of past periods to estimate the 
#' linear model over. For each period in the vintage forecasts, coefficients are
#' estimated with data up to the current period minus the number of periods 
#' specified in \code{estimation_window}. If \code{estimation_window} is left
#' \code{NULL} then the linear model is estimated with all available data up to
#' the current period. Coefficients are then multiplied by vintage forecast values. 
#' Returns an out-of-sample forecast conditional on vintage forecasts that 
#' \strong{would} have been available at the forecast origin. Optionally returns 
#' the coefficients used to create each forecast. Replicates the forecasts that a 
#' linear model would have produced in real time.
#'
#' @param lm_call Linear model call of the class lm.
#' @param time_vec Vector of any class that is equal in length to the data
#'   in \code{lm_call}.
#' @param ... Set of forecasts of class Forecast, one forecast for each
#'   covariate in the linear model.
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
#' x1_forecast_vintage <- Forecast(
#'    origin   = as.Date(c("2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30")),
#'    future   = as.Date(c("2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30")),
#'    forecast = c(6.30, 4.17, 5.30, 4.84),
#'    realized = c(4.92, 5.80, 6.30, 4.17),
#'    h_ahead  = 4L
#' )
#'
#' x2_forecast_vintage <- Forecast(
#'    origin   = as.Date(c("2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30")),
#'    future   = as.Date(c("2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30")),
#'  forecast = c(7.32, 6.88, 6.82, 6.95),
#'  realized = c(8.68, 9.91, 7.87, 6.63),
#'  h_ahead  = 4L
#' ) 
#' 
#' oos_vintage_forc(
#'   lm_call = lm(y ~ x1 + x2, data),
#'   time_vec = data$date,
#'   x1_forecast_vintage, x2_forecast_vintage,
#'   estimation_window = 4L,
#'   return_betas = FALSE
#' )
#' 
#' oos_vintage_forc(
#'   lm_call = lm(y ~ x1 + x2, data),
#'   time_vec = data$date,
#'   x1_forecast_vintage, x2_forecast_vintage
#' )
#' 

#===============================================================================
# OOS Vintage Forecast
#===============================================================================

#' @export

oos_vintage_forc <- function(lm_call, time_vec, ..., estimation_window = NULL, return_betas = FALSE) {

  forecasts <- list(...)
  num_coefs <- length(lm_call$coefficients)

  # Input validation.
  if (class(lm_call) != "lm") {
    stop("* lm_call must be must be of the lm function form: lm_call = lm(y = x1 + x2, data)")
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

  if ((num_coefs - 1) != length(forecasts)) {
    stop(paste0("* Number of forecasts must equal the number of coefficients in lm_call.\n",
      "  * Number of coefficients in lm_call: ", num_coefs - 1,
      "\n  * Number of forecasts passed to function: ", length(forecasts),
      "\n  * Forecasts are passed to lm_forc functions via ellipsis (...)"))
  }

  if (length(time_vec) != nrow(lm_call$model)) {
    stop(paste0("* Length of time_vec must equal the number of rows in the lm_call data.\n",
                "  * Length of time_vec: ", length(time_vec),
                "\n  * Number of rows in lm_call data: ", nrow(lm_call$model),
                "\n  * This may be caused by NAs in the data."))
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

  lm_call$call$data <- quote(train_data)

  origin   <- origin_vec
  future   <- forecasts[[1]]@future
  forecast <- vector(mode = "double", length = length(origin_vec))
  realized <- lm_call$model[[1]][match(forecasts[[1]]@future, time_vec)]
  h_ahead  <- forecasts[[1]]@h_ahead

  betas <- vector(mode = "list", length = length(origin_vec))
  
  # Run forecast loop.
  for (i in 1:length(origin_vec)) {

    # Subset train_data by estimation_window parameter.
    if (is.null(estimation_window) == TRUE) {
      train_data <- lm_call$model[time_vec <= origin_vec[i], ] 
    } else {
      train_data <- lm_call$model[time_vec <= origin_vec[i], ]
      if ((nrow(train_data) - estimation_window) >= 1) {
        train_data <- train_data[((nrow(train_data) - estimation_window + 1):nrow(train_data)), ]
      }
    }
    
    train_lm   <- eval(lm_call$call)

    coefs  <- train_lm$coefficients
    covars <- sapply(forecasts, function(x) x@forecast[i])
    betas[[i]] <- coefs

    forecast[[i]] <- coefs[[1]] + sum(coefs[2:length(coefs)] * covars)

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
