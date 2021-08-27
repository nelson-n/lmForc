
#' Out-of-sample lagged linear model forecast conditioned on realized values
#'
#' \code{oos_lag_forc} takes a linear model call, an integer number of
#' periods ahead to forecast, a period to end the initial coefficient estimation
#' and begin forecasting, and an optional vector of time data associated with
#' the linear model. Linear model data is lagged by \code{h_ahead} periods and
#' the linear model is re-estimated with data up to \code{estimation_end} to
#' create a lagged linear model. Coefficients are multiplied by present period
#' realized values of the covariates to create a forecast for \code{h_ahead}
#' periods ahead. This process is iteratively repeated for each period after
#' \code{estimation_end} with coefficients updating in each period. Returns an
#' out-of-sample forecast conditional on realized values that \strong{would}
#' have been available at the forecast origin. Tests the out-of-sample
#' performance of a linear model had it been lagged and conditioned on available
#' information.
#'
#' @param lm_call Linear model call of the class lm.
#' @param h_ahead Integer representing the number of periods ahead that is being
#'   forecasted.
#' @param estimation_end Value of any class representing when to end the initial
#'   coefficient estimation period and begin forecasting.
#' @param time_vec Vector of any class that is equal in length to the data
#'   in \code{lm_call}.
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
#' oos_lag_forc(
#'   lm_call = lm(y ~ x1 + x2, data),
#'   h_ahead = 4L,
#'   estimation_end = as.Date("2010-01-01"),
#'   time_vec = data$date
#' )
#'
#' oos_lag_forc(
#'   lm_call = lm(y ~ x1 + x2, data),
#'   h_ahead = 4L,
#'   estimation_end = 130L
#' )
#' 
#' }

#===============================================================================
# OOS Lag Forecast
#===============================================================================

#' @export

oos_lag_forc <- function(lm_call, h_ahead, estimation_end, time_vec = NULL) {

  # Input validation.
  if (class(lm_call) != "lm") {
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

  # Function to lag a vector n steps.
  vector_lag <- function(vector, n) {
    vector <- c(rep(NA, n), vector[1:(length(vector) - n)])
    return(vector)
  }

  # Find OOS forecast period and prepare forecasting loop.
  if (is.null(time_vec) == TRUE) {
    time_vec <- 1:nrow(lm_call$model)
  }

  if (is.null(time_vec) == FALSE) {
    estimation_end <- which(time_vec <= estimation_end)
    estimation_end <- estimation_end[length(estimation_end)]
  }

  oos_index <- estimation_end:(nrow(lm_call$model) - h_ahead)

  lm_call$call$data <- quote(train_data)

  origin   <- time_vec[oos_index]
  future   <- time_vec[oos_index + h_ahead]
  forecast <- vector(mode = "double", length = length(oos_index))
  realized <- lm_call$model[[1]][oos_index + h_ahead]

  # Run forecasting loop.
  for (i in 1:length(oos_index)) {

    index <- oos_index[[i]]
    train_data <- lm_call$model[1:index, ]

    # Lag train_data by h_ahead.
    train_data[, 2:length(lm_call$coefficients)] <-
      sapply(train_data[, 2:length(lm_call$coefficients)], function(x) vector_lag(x, n = h_ahead))

    train_lm   <- eval(lm_call$call)
    coefs      <- train_lm$coefficients

    realized_vals <- lm_call$model[index, ]
    forecast[[i]] <- coefs[[1]] + sum(coefs[2:length(coefs)] * realized_vals[2:length(realized_vals)])

  }

  Forecast(origin = origin, future = future, forecast = forecast,
           realized = realized, h_ahead = h_ahead)

}
