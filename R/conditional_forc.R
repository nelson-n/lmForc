
#' Linear model forecast conditioned on an input forecast
#'
#' \code{conditional_forc} takes a linear model call, a vector of time data
#' associated with the linear model, and a forecast for each covariate in the
#' linear model. The linear model is estimated once over the entire sample
#' period and the coefficients are multiplied by the forecasts of each
#' covariate. Returns a forecast conditional on forecasts of each covariate.
#' Used to create a forecast for the present period or replicate a forecast made
#' at a specific period in the past.
#'
#' @param lm_call Linear model call of the class lm.
#' @param time_vec Vector of any class that is equal in length to the data
#'   in \code{lm_call}.
#' @param ... One or more forecasts of class Forecast, one forecast for each
#'   covariate in the linear model.
#'
#' @return \code{\link{Forecast}} object that contains the conditional forecast.
#'
#' @seealso
#' For a detailed example see the help vignette:
#' \code{vignette("lmForc", package = "lmForc")}
#'
#' @examples 
#'
#' x1_forecast <- Forecast(
#'    origin   = as.Date(c("2012-06-30", "2012-06-30", "2012-06-30", "2012-06-30")),
#'    future   = as.Date(c("2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30")),
#'    forecast = c(4.14, 4.04, 4.97, 5.12),
#'    realized = NULL,
#'    h_ahead  = NULL
#' )
#'
#' x2_forecast <- Forecast(
#'    origin   = as.Date(c("2012-06-30", "2012-06-30", "2012-06-30", "2012-06-30")),
#'    future   = as.Date(c("2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30")),
#'    forecast = c(6.01, 6.05, 6.55, 7.45),
#'    realized = NULL,
#'    h_ahead  = NULL
#')
#' 
#' date <- as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
#'                   "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", 
#'                   "2012-03-31", "2012-06-30"))
#' y  <- c(1.09, 1.71, 1.09, 2.46, 1.78, 1.35, 2.89, 2.11, 2.97, 0.99)
#' x1 <- c(4.22, 3.86, 4.27, 5.60, 5.11, 4.31, 4.92, 5.80, 6.30, 4.17)
#' x2 <- c(10.03, 10.49, 10.85, 10.47, 9.09, 10.91, 8.68, 9.91, 7.87, 6.63)
#' data <- data.frame(date, y, x1, x2)
#' 
#' conditional_forc(
#'   lm_call = lm(y ~ x1 + x2, data),
#'   time_vec = data$date,
#'   x1_forecast, x2_forecast
#' )
#'

#===============================================================================
# Conditional Forecast
#===============================================================================

#' @export

conditional_forc <- function(lm_call, time_vec, ...) {

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
    stop("* all forecasts must have identical future values.")
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

  # For each future value, find the latest origin in all forecasts.
  origin_vecs <- lapply(forecasts, function(x) x@origin)
  origin_vec  <- Reduce(pmax, origin_vecs)

  forecast_vecs <- lapply(forecasts, function(x) x@forecast)

  # Calculate conditional forecast.
  forecast <- lm_call$coefficients[[1]] +
    rowSums(mapply("*", lm_call$coefficients[2:num_coefs], forecast_vecs))

  Forecast(
    origin   = origin_vec,
    future   = forecasts[[1]]@future,
    forecast = forecast,
    realized = lm_call$model[[1]][match(forecasts[[1]]@future, time_vec)],
    h_ahead  = forecasts[[1]]@h_ahead
  )

}
