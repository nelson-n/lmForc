
#' MSE or RMSE weighted forecast
#'
#' \code{mse_weighted_forc} takes two or more forecasts, an evaluation window,
#' and an error function. For each forecast period, the error function is used
#' to calculate forecast accuracy over the past \code{eval_window} number of
#' periods. The forecast accuracy of each forecast is used to weight forecasts
#' based on performance. Returns a weighted forecast. Optionally returns the set
#' of weights used to weight forecasts in each period.
#'
#' Forecasts are weighted in each period with the following function. The error
#' function used is MSE or RMSE depending on user selection. This example shows
#' MSE errors.
#' \deqn{weight = (1 / MSE(forecast)) / (1 / sum(MSE(forecasts)))}
#'
#' @param ... Two or more forecasts of class Forecast.
#' @param eval_window Integer representing the window over which forecast
#'   accuracy is evaluated. Forecasts are weighted based on their accuracy over
#'   the past \code{eval_window} number of periods.
#' @param errors Character, either "mse" or "rmse". Selects whether forecast
#'   accuracy is evaluated using mean squared errors or root mean squared
#'   errors.
#' @param return_weights Boolean, selects whether the weights used to weight
#'   forecasts in each period are returned. If TRUE, a data frame of weights is returned to the
#'   Global Environment.
#'
#' @return \code{\link{Forecast}} object that contains the weighted forecast.
#'
#' @seealso
#' For a detailed example see the help vignette:
#' \code{vignette("lmForc", package = "lmForc")}
#'
#' @examples 
#' y1_forecast <- Forecast(
#'   origin = as.Date(c("2009-03-31", "2009-06-30", "2009-09-30", "2009-12-31",
#'                      "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", 
#'                      "2011-03-31", "2011-06-30")),
#'   future = as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
#'                      "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", 
#'                      "2012-03-31", "2012-06-30")),
#'   forecast = c(1.33, 1.36, 1.38, 1.68, 1.60, 1.55, 1.32, 1.22, 1.08, 0.88),
#'   realized = c(1.09, 1.71, 1.09, 2.46, 1.78, 1.35, 2.89, 2.11, 2.97, 0.99),
#'   h_ahead = 4L
#' )
#'
#' y2_forecast <- Forecast(
#'   origin = as.Date(c("2009-03-31", "2009-06-30", "2009-09-30", "2009-12-31",
#'                      "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", 
#'                      "2011-03-31", "2011-06-30")),
#'   future = as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
#'                      "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", 
#'                      "2012-03-31", "2012-06-30")),
#'   forecast = c(0.70, 0.88, 1.03, 1.05, 1.01, 0.82, 0.95, 1.09, 1.07, 1.06),
#'   realized = c(1.09, 1.71, 1.09, 2.46, 1.78, 1.35, 2.89, 2.11, 2.97, 0.99),
#'   h_ahead = 4L
#' )
#'
#' mse_weighted_forc(
#'   y1_forecast, y2_forecast,
#'   eval_window = 2L,
#'   errors = "mse",
#'   return_weights = FALSE
#' )
#' 

#===============================================================================
# MSE Weighted Forecast
#===============================================================================

#' @export

mse_weighted_forc <- function(..., eval_window, errors = "mse", return_weights = FALSE) {

  forecasts <- list(...)
  weights_out <- NULL

  # Input validation.
  if (any(lapply(forecasts, class) != "Forecast")) {
    stop(paste0("* all ellipsis (...) arguments must be of class Forecast.\n",
                "  * ellipsis (...) arguments are currently of the class: ",
                paste(lapply(forecasts, class), collapse = ", ")))
  }

  if (length(forecasts) < 2) {
    stop("* at least two forecasts must be passed to the function:
  forecast1, forecast2")
  }

  if (is.integer(eval_window) != TRUE) {
    stop("* eval_window must be an integer: eval_window = 4L")
  }

  if (length(eval_window) > 1) {
    stop("* eval_window must be of length one: eval_window = 4L")
  }

  if (!(errors %in% c("mse", "rmse"))) {
    stop('* errors must be either "mse" or "rmse": errors = "mse"')
  }

  if (length(unique(lapply(forecasts, function(x) class(x@origin)))) > 1) {
    stop(paste0("* origin values of all forecasts must be of the same class.\n",
                "  * origin values are currently of the class: ",
                paste(lapply(forecasts, function(x) class(x@origin)), collapse = ", ")))
  }

  if (length(unique(lapply(forecasts, function(x) x@future))) > 1) {
    stop("* all forecasts must have identical future values.")
  }

  # MSE function.
  mse <- function(forecast, realized) {
    1/length(forecast) * sum((realized - forecast)^2)
  }

  # RMSE function.
  rmse <- function(forecast, realized) {
    sqrt(mse(forecast, realized))
  }

  # Select error function.
  if (errors == "mse") {
    error_function <- mse
  }

  if (errors == "rmse") {
    error_function <- rmse
  }

  # Find forecast names.
  argnames   <- sys.call()
  argnames   <- unlist(lapply(argnames[-1], as.character))
  forc_names <- paste0(argnames[1:length(forecasts)])

  # For each future value, find the latest origin in all forecasts.
  origin_vecs <- lapply(forecasts, function(x) x@origin)
  origin_vec  <- Reduce(pmax, origin_vecs)

  # Check that max_eval_window has not been exceeded.
  max_eval_window <- length(which(origin_vec[length(origin_vec)] >= forecasts[[1]]@future))
  
  if (eval_window > max_eval_window) {
    stop(paste0("* eval_window exceeds the number of historical forecast observations.\n",
                "  * the maximum eval_window is: ",
                paste(max_eval_window)))
  }
  
  # Find OOS forecast period and prepare forecasting loop.
  forecast_periods <- which(origin_vec >= forecasts[[1]]@future[eval_window])
  eval_period <- length(forecast_periods)
  weighted_forc   <- vector(mode = "numeric", length = eval_period)
  weights         <- vector(mode = "list", length = eval_period)

  for (i in 1:eval_period) {

    forecast_period <- forecast_periods[i]
    pos <- max(which(origin_vec[forecast_period] >= forecasts[[1]]@future))

    # Subset forecasts to historical error evaluation period.
    h_forecasts <- lapply(forecasts, function(x) x@forecast[(pos - eval_window + 1):pos])
    h_realized  <- lapply(forecasts, function(x) x@realized[(pos - eval_window + 1):pos])

    errors <- mapply(error_function, h_forecasts, h_realized)
    weight <- (1/errors) / sum(1/errors)

    # Subset forecasts to current forecasts.
    c_forecasts <- sapply(forecasts, function(x) x@forecast[forecast_period])

    weighted_forc[[i]] <- sum(c_forecasts * weight)
    weights[[i]] <- weight

  }

  if (return_weights == TRUE) {

    weights <- data.frame(do.call(rbind, weights))

    weights <- cbind(
      origin_vec[forecast_periods],
      forecasts[[1]]@future[forecast_periods],
      weights
    )

    colnames(weights) <- c("origin", "future", forc_names)
    weights_out <<- weights

  }

  Forecast(
    origin   = origin_vec,
    future   = forecasts[[1]]@future,
    forecast = c(rep(NA_real_, length(1:forecast_periods[1]) - 1), weighted_forc),
    realized = forecasts[[1]]@realized,
    h_ahead  = forecasts[[1]]@h_ahead
  )

}
