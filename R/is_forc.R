
#' In-sample linear model forecast
#'
#' \code{is_forc} takes a linear model call and an optional vector of time
#' data associated with the linear model. The linear model is estimated once
#' over the entire sample period and the coefficients are multiplied by the
#' realized values in each period of the sample. Returns an in-sample forecast
#' conditional on realized values.
#'
#' @param lm_call Linear model call of the class lm.
#' @param time_vec Vector of any class that is equal in length to the data
#'   in \code{lm_call}.
#'
#' @return \code{\link{Forecast}} object that contains the in-sample forecast.
#'
#' @seealso
#' For a detailed example see the help vignette:
#' \code{vignette("lmForc", package = "lmForc")}
#'
#' @examples \dontrun{
#' 
#' is_forc(
#'   lm_call = lm(y ~ x1 + x2, data),
#'   time_vec = data$date
#' )
#'
#' is_forc(
#'   lm_call = lm(y ~ x1 + x2, data)
#' )
#' }

#===============================================================================
# IS Forecast
#===============================================================================

#' @export

is_forc <- function(lm_call, time_vec = NULL) {

  # Input validation.
  if (class(lm_call) != "lm") {
    stop("* lm_call must be must be of the lm function form: lm_call = lm(y = x1 + x2, data)")
  }

  if (is.null(time_vec) == FALSE & length(time_vec) != nrow(lm_call$model)) {
    stop(paste0("* Length of time_vec must equal the number of rows in the lm_call data.\n",
                "  * Length of time_vec: ", length(time_vec),
                "\n  * Number of rows in lm_call data: ", nrow(lm_call$model),
                "\n  * This may be caused by NAs in the data."))
  }

  if (is.null(time_vec) == TRUE) {
    time_vec <- 1:nrow(lm_call$model)
  }

  num_coefs <- length(lm_call$coefficients)

  # Calculate in sample forecast.
  forecast <- lm_call$coefficients[[1]] +
    rowSums(mapply("*", lm_call$coefficients[2:num_coefs], lm_call$model[2:num_coefs]))

  Forecast(
    origin   = time_vec,
    future   = time_vec,
    forecast = forecast,
    realized = lm_call$model[[1]],
    h_ahead  = 0
  )

}
