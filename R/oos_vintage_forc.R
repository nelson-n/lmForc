
#' Out-of-sample linear model forecast conditioned on vintage forecasts
#'
#' \code{oos_vintage_forc} takes a linear model call, a vector of time data
#' associated with the linear model, and a forecast for each covariate in the
#' linear model. For each period in the vintage forecasts, coefficients are
#' updated based on information that would have been available at the forecast
#' origin. Coefficients are then multiplied by vintage forecast values. Returns
#' an out-of-sample forecast conditional on vintage forecasts that
#' \strong{would} have been available at the forecast origin. Replicates the
#' forecasts that a linear model would have produced.
#'
#' @param lm_call Linear model call of the class lm.
#' @param time_vec Vector of any class that is equal in length to the data
#'   in \code{lm_call}.
#' @param ... Set of forecasts of class Forecast, one forecast for each
#'   covariate in the linear model.
#'
#' @return \code{\link{Forecast}} object that contains the out-of-sample
#'   forecast.
#'
#' @seealso
#' For a detailed example see the help vignette:
#' \code{vignette("lmForc", package = "lmForc")}
#'
#' @examples \dontrun{
#' 
#' oos_vintage_forc(
#'   lm_call = lm(y ~ x1 + x2, data),
#'   time_vec = data$date,
#'   x1_forecast_vintage, x2_forecast_vintage
#' )
#' 
#' }

#===============================================================================
# OOS Vintage Forecast
#===============================================================================

#' @export

oos_vintage_forc <- function(lm_call, time_vec, ...) {

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

  # For each future value, find the latest origin in all forecasts.
  origin_vecs <- lapply(forecasts, function(x) x@origin)
  origin_vec  <- Reduce(pmax, origin_vecs)

  lm_call$call$data <- quote(train_data)

  origin   <- origin_vec
  future   <- forecasts[[1]]@future
  forecast <- vector(mode = "double", length = length(origin_vec))
  realized <- lm_call$model[[1]][match(forecasts[[1]]@future, time_vec)]
  h_ahead  <- forecasts[[1]]@h_ahead

  # Run forecast loop.
  for (i in 1:length(origin_vec)) {

    train_data <- lm_call$model[time_vec <= origin_vec[i], ]
    train_lm   <- eval(lm_call$call)

    coefs  <- train_lm$coefficients
    covars <- sapply(forecasts, function(x) x@forecast[i])

    forecast[[i]] <- coefs[[1]] + sum(coefs[2:length(coefs)] * covars)

  }

  Forecast(origin = origin, future = future, forecast = forecast,
           realized = realized, h_ahead = h_ahead)

}
