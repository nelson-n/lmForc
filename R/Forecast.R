
#' S4 class for storing forecasts
#'
#' An S4 class for storing forecasts. An object of the Forecast class has equal
#' length vectors that contain the time the forecast was made, the future time
#' being forecasted, the forecast, and realized values if available. Optionally
#' includes the number of periods ahead being forecasted.
#'
#' @slot origin A vector of any class representing the time when the forecast
#'    was made.
#' @slot future A vector of any class representing the time that is being
#'    forecasted, i.e. when the forecast will be realized.
#' @slot forecast A numeric vector of forecasts.
#' @slot realized Optional numeric vector of realized values, i.e. the true
#'    value at the future time.
#' @slot h_ahead Optional length-one object representing the number of periods
#'    ahead being forecasted.
#'
#' @seealso
#' For a detailed example see the help vignette:
#' \code{vignette("lmForc", package = "lmForc")}

#===============================================================================
# Forecast Class
#===============================================================================

#' @export

# Define Forecast class.
setClass("Forecast",

  slots = c(
    origin   = "ANY",
    future   = "ANY",
    forecast = "numeric",
    realized = "numeric",
    h_ahead  = "ANY"
    ),

  prototype = list(
    origin   = NA,
    future   = NA,
    forecast = NA_real_,
    realized = NA_real_,
    h_ahead  = NA
  )

)

#' Create an object of the Forecast class
#'
#' An S4 class for storing forecasts. An object of the Forecast class has equal
#' length vectors that contain the time the forecast was made, the future time
#' being forecasted, the forecast, and realized values if available. Optionally
#' includes the number of periods ahead being forecasted.
#'
#' @param origin A vector of any class representing the time when the forecast
#'    was made.
#' @param future A vector of any class representing the time that is being
#'    forecasted, i.e. when the forecast will be realized.
#' @param forecast A numeric vector of forecasts.
#' @param realized Optional numeric vector of realized values, i.e. the true
#'    value at the future time.
#' @param h_ahead Optional length-one object representing the number of periods
#'    ahead being forecasted.
#'
#' @seealso
#' For a detailed example see the help vignette:
#' \code{vignette("lmForc", package = "lmForc")}
#'
#' @examples
#' my_forecast <- Forecast(
#'    origin   = c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31"),
#'    future   = c("2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31"),
#'    forecast = c(4.21, 4.27, 5.32, 5.11),
#'    realized = c(4.40, 4.45, 4.87, 4.77),
#'    h_ahead  = 4L
#' )
#'
#' origin(my_forecast) <- c("2010-04-01", "2010-07-01", "2010-10-01", "2011-01-01")
#' future(my_forecast) <- c("2012-04-01", "2012-07-01", "2012-10-01", "2013-01-01")
#' forc(my_forecast) <- c(8.87, 7.61, 7.56, 5.96)
#' realized(my_forecast) <- c(6.64, 6.10, 6.33, 6.67)
#' h_ahead(my_forecast) <- 8L
#'
#' origin(my_forecast)
#' future(my_forecast)
#' forc(my_forecast)
#' realized(my_forecast)
#' h_ahead(my_forecast)
#'
#' @importFrom methods new
#'
#' @export

# Helper for Forecast class construction.
Forecast <- function(origin, future, forecast, realized = NULL, h_ahead = NULL) {

  if (is.null(realized) == TRUE) {
    realized <- rep(NA_real_, length(forecast))
  }

  new("Forecast",
      origin = origin,
      future = future,
      forecast = forecast,
      realized = realized,
      h_ahead = h_ahead)

}

# Validator for Forecast class construction.
setValidity("Forecast", function(object) {

  if (
    length(object@origin)   != length(object@future) |
    length(object@future)   != length(object@forecast) |
    length(object@forecast) != length(object@realized)
      ) {
    "@origin, @future, @forecast, and @realized must be the same length"
  }

  else if (class(object@forecast) != "numeric") {
    "@forecast must be a numeric object"
  }

  else if (class(object@realized) != "numeric") {
    "@realized must be a numeric object"
  }

  else if (length(object@h_ahead) > 1) {
    "@h_ahead must be of length one"
  }

  else {TRUE}

})

## Create "show" method for Forecast class.

#' Print Forecast object to console.
#'
#' \code{show} takes a \code{\link{Forecast}} object and prints it to console.
#'
#' @param object Forecast object.
#'
#' @return Printed \code{\link{Forecast}} object.
#'
#' @examples 
#' 
#' my_forecast <- Forecast(
#'   origin   = as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31")),
#'   future   = as.Date(c("2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31")),
#'   forecast = c(4.21, 4.27, 5.32, 5.11),
#'   realized = c(4.40, 4.45, 4.87, 4.77),
#'   h_ahead  = 4L
#' )
#' 
#' print(my_forecast)
#'
#' @importFrom methods show
#'
#' @export

setMethod("show", "Forecast", function(object) {

  dataframe <- data.frame(object@origin, object@future, object@forecast, object@realized)
  colnames(dataframe) <- c("origin", "future", "forecast", "realized")

  cat("h_ahead =", object@h_ahead, "\n\n")
  base::print.data.frame(dataframe)

})

## Create "str" method for Forecast class.

#' Display internal structure structure of Forecast object to the console.
#'
#' \code{str} takes a \code{\link{Forecast}} object and prints its internal 
#' structure to the console.
#'
#' @param object Forecast object.
#'
#' @return Structure of \code{\link{Forecast}} object.
#'
#' @examples 
#' 
#' my_forecast <- Forecast(
#'   origin   = as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31")),
#'   future   = as.Date(c("2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31")),
#'   forecast = c(4.21, 4.27, 5.32, 5.11),
#'   realized = c(4.40, 4.45, 4.87, 4.77),
#'   h_ahead  = 4L
#' )
#' 
#' str(my_forecast)
#'
#' @importFrom utils str
#' @importFrom utils capture.output
#'
#' @export

setMethod("str", "Forecast", function(object) {

  desc <- capture.output(utils:::str.default((object)))
  desc[[1]] <- paste0("Forecast of length ", length(object@forecast))
  
  cat(
    desc[[1]], "\n", 
    desc[[2]], "\n", 
    desc[[3]], "\n", 
    desc[[4]], "\n", 
    desc[[5]], "\n", 
    desc[[6]] 
  )
  
})

#===============================================================================
# Method for Subsetting Forecast Objects
#===============================================================================

#' Subset Forecast object.
#'
#' \code{[]} takes a \code{\link{Forecast}} object and subsets it.
#'
#' @param Forecast Forecast object.
#' @param x ANY
#' @param i ANY
#' @param j ANY
#' @param ... ANY
#' @param drop ANY
#'
#' @return Subsetted \code{\link{Forecast}} object.
#'
#' @export

setMethod("[", c("Forecast", "ANY", "ANY", "ANY"), 
          
  function(x, i, j, ..., drop = TRUE) {
  
    x@origin   <- x@origin[i]
    x@future   <- x@future[i]
    x@forecast <- x@forecast[i]
    x@realized <- x@realized[i]
  
    validObject(x)
    x
  
})

#===============================================================================
# Origin Setter/Getter Generics and Methods
#===============================================================================

#' Set the origin slot of a Forecast object
#'
#' \code{origin} takes a \code{\link{Forecast}} object and sets the origin
#' vector of the forecast.
#'
#' @param Forecast Forecast object.
#' @param value Vector of values assigned to the origin slot of the Forecast.
#'
#' @return \code{\link{Forecast}} object that contains the new origin vector.
#'
#' @examples \dontrun{
#' 
#' origin(Forecast) <- c("2015-01-01", "2015-01-02", "2015-01-03")
#' 
#' }
#'
#' @export

setGeneric("origin<-", function(Forecast, value) standardGeneric("origin<-"))

#' Set origin slot of a Forecast object
#'
#' \code{origin} takes a \code{\link{Forecast}} object and sets the origin
#' vector of the forecast.
#'
#' @param Forecast Forecast object.
#' @param value Vector of values assigned to the origin slot of the Forecast.
#'
#' @return \code{\link{Forecast}} object that contains the new origin vector.
#'
#' @examples \dontrun{
#' 
#' origin(Forecast) <- c("2015-01-01", "2015-01-02", "2015-01-03")
#' 
#' }
#'
#' @importFrom methods validObject
#'
#' @export

setMethod("origin<-", "Forecast", function(Forecast, value) {
  Forecast@origin <- value
  validObject(Forecast)
  Forecast
})

#' Get the origin slot of a Forecast object
#'
#' \code{origin} takes a \code{\link{Forecast}} object and gets the origin
#' vector of the forecast.
#'
#' @param Forecast Forecast object.
#'
#' @return Vector of origin values stored in the \link{Forecast} object.
#'
#' @examples \dontrun{
#' 
#' origin(Forecast)
#' 
#' }
#'
#' @export

setGeneric("origin", function(Forecast) standardGeneric("origin"))

#' Get the origin slot of a Forecast object
#'
#' \code{origin} takes a \code{\link{Forecast}} object and gets the origin
#' vector of the forecast.
#'
#' @param Forecast Forecast object.
#'
#' @return Vector of origin values stored in the \link{Forecast} object.
#'
#' @examples \dontrun{
#' 
#' origin(Forecast)
#' 
#' }
#'
#' @export

setMethod("origin", "Forecast", function(Forecast) Forecast@origin)

#===============================================================================
# Future Setter/Getter Generics and Methods
#===============================================================================

#' Set the future slot of a Forecast object
#'
#' \code{future} takes a \code{\link{Forecast}} object and sets the future
#' vector of the forecast.
#'
#' @param Forecast Forecast object.
#' @param value Vector of values assigned to the future slot of the Forecast.
#'
#' @return \code{\link{Forecast}} object that contains the new future vector.
#'
#' @examples \dontrun{
#' 
#' future(Forecast) <- c("2015-03-01", "2015-03-02", "2015-03-03")
#' 
#' }
#'
#' @export

setGeneric("future<-", function(Forecast, value) standardGeneric("future<-"))

#' Set future slot of a Forecast object
#'
#' \code{future} takes a \code{\link{Forecast}} object and sets the future
#' vector of the forecast.
#'
#' @param Forecast Forecast object.
#' @param value Vector of values assigned to the future slot of the Forecast.
#'
#' @return \code{\link{Forecast}} object that contains the new future vector.
#'
#' @examples \dontrun{
#' 
#' future(Forecast) <- c("2015-03-01", "2015-03-02", "2015-03-03")
#' 
#' }
#'
#' @importFrom methods validObject
#'
#' @export

setMethod("future<-", "Forecast", function(Forecast, value) {
  Forecast@future <- value
  validObject(Forecast)
  Forecast
})

#' Get the future slot of a Forecast object
#'
#' \code{future} takes a \code{\link{Forecast}} object and gets the future
#' vector of the forecast.
#'
#' @param Forecast object.
#'
#' @return Vector of future values stored in the \link{Forecast} object.
#'
#' @examples \dontrun{
#' 
#' future(Forecast)
#' 
#' }
#'
#' @export

setGeneric("future", function(Forecast) standardGeneric("future"))

#' Get the future slot of a Forecast object
#'
#' \code{future} takes a \code{\link{Forecast}} object and gets the future
#' vector of the forecast.
#'
#' @param Forecast object.
#'
#' @return Vector of future values stored in the \link{Forecast} object.
#'
#' @examples \dontrun{
#' 
#' future(Forecast)
#' 
#' }
#'
#' @export

setMethod("future", "Forecast", function(Forecast) Forecast@future)

#===============================================================================
# Forecast (forc) Setter/Getter Generics and Methods
#===============================================================================

#' Set forecast slot of a Forecast object
#'
#' \code{forc} takes a \code{\link{Forecast}} object and sets the forecast
#' vector of the forecast.
#'
#' @param Forecast Forecast object.
#' @param value Vector of values assigned to the forecast slot of the Forecast.
#'
#' @return \code{\link{Forecast}} object that contains the new forecast vector.
#'
#' @examples \dontrun{
#' 
#' forc(Forecast) <- c(2.45, 2.76, 3.31)
#' 
#' }
#'
#' @export

setGeneric("forc<-", function(Forecast, value) standardGeneric("forc<-"))

#' Set forecast slot of a Forecast object
#'
#' \code{forc} takes a \code{\link{Forecast}} object and sets the forecast
#' vector of the forecast.
#'
#' @param Forecast Forecast object.
#' @param value Vector of values assigned to the forecast slot of the Forecast.
#'
#' @return \code{\link{Forecast}} object that contains the new forecast vector.
#'
#' @examples \dontrun{
#' 
#' forc(Forecast) <- c(2.45, 2.76, 3.31)
#' 
#' }
#'
#' @importFrom methods validObject
#'
#' @export

setMethod("forc<-", "Forecast", function(Forecast, value) {
  Forecast@forecast <- value
  validObject(Forecast)
  Forecast
})

#' Get the forecast slot of a Forecast object
#'
#' \code{forc} takes a \code{\link{Forecast}} object and gets the
#' forecast vector of the forecast.
#'
#' @param Forecast Forecast object.
#'
#' @return Vector of forecast values stored in the \link{Forecast} object.
#'
#' @examples \dontrun{
#' 
#' forc(Forecast)
#' 
#' }
#'
#' @export

setGeneric("forc", function(Forecast) standardGeneric("forc"))

#' Get the forecast slot of a Forecast object
#'
#' \code{forc} takes a \code{\link{Forecast}} object and gets the
#' forecast vector of the forecast.
#'
#' @param Forecast Forecast object.
#'
#' @return Vector of forecast values stored in the \link{Forecast} object.
#'
#' @examples \dontrun{
#' 
#' forc(Forecast)
#' 
#' }
#'
#' @export

setMethod("forc", "Forecast", function(Forecast) Forecast@forecast)

#===============================================================================
# Realized Setter/Getter Generics and Methods
#===============================================================================

#' Set realized slot of a Forecast object
#'
#' \code{realized} takes a \code{\link{Forecast}} object and sets the realized
#' vector of the forecast.
#'
#' @param Forecast Forecast object.
#' @param value Vector of values assigned to the realized slot of the Forecast.
#'
#' @return \code{\link{Forecast}} object that contains the new realized vector.
#'
#' @examples \dontrun{
#' 
#' realized(Forecast) <- c("2015-03-01", "2015-03-02", "2015-03-03")
#' 
#' }
#'
#' @export

setGeneric("realized<-", function(Forecast, value) standardGeneric("realized<-"))

#' Set realized slot of a Forecast object
#'
#' \code{realized} takes a \code{\link{Forecast}} object and sets the realized
#' vector of the forecast.
#'
#' @param Forecast Forecast object.
#' @param value Vector of values assigned to the realized slot of the Forecast.
#'
#' @return \code{\link{Forecast}} object that contains the new realized vector.
#'
#' @examples \dontrun{
#' 
#' realized(Forecast) <- c("2015-03-01", "2015-03-02", "2015-03-03")
#' 
#' }
#'
#' @importFrom methods validObject
#'
#' @export

setMethod("realized<-", "Forecast", function(Forecast, value) {
  Forecast@realized <- value
  validObject(Forecast)
  Forecast
})

#' Get the realized slot of a realized object
#'
#' \code{realized} takes a \code{\link{Forecast}} object and gets the realized
#' vector of the forecast.
#'
#' @param Forecast Forecast object.
#'
#' @return Vector of realized values stored in the \link{Forecast} object.
#'
#' @examples \dontrun{
#' 
#' realized(Forecast)
#' 
#' }
#'
#' @export

setGeneric("realized", function(Forecast) standardGeneric("realized"))

#' Get the realized slot of a realized object
#'
#' \code{realized} takes a \code{\link{Forecast}} object and gets the realized
#' vector of the forecast.
#'
#' @param Forecast Forecast object.
#'
#' @return Vector of realized values stored in the \link{Forecast} object.
#'
#' @examples \dontrun{
#' 
#' realized(Forecast)
#' 
#' }
#'
#' @export

setMethod("realized", "Forecast", function(Forecast) Forecast@realized)

#===============================================================================
# H_ahead Setter/Getter Generics and Methods
#===============================================================================

#' Set h_ahead slot of a Forecast object
#'
#' \code{h_ahead} takes a \code{\link{Forecast}} object and sets the h_ahead
#' vector of the forecast.
#'
#' @param Forecast Forecast object.
#' @param value Vector of values assigned to the h_ahead slot of the Forecast.
#'
#' @return \code{\link{Forecast}} object that contains the new h_ahead vector.
#'
#' @examples \dontrun{
#' 
#' h_ahead(Forecast) <- 4L
#' 
#' }
#'
#' @export

setGeneric("h_ahead<-", function(Forecast, value) standardGeneric("h_ahead<-"))

#' Set h_ahead slot of a Forecast object
#'
#' \code{h_ahead} takes a \code{\link{Forecast}} object and sets the h_ahead
#' vector of the forecast.
#'
#' @param Forecast Forecast object.
#' @param value Vector of values assigned to the h_ahead slot of the Forecast.
#'
#' @return \code{\link{Forecast}} object that contains the new h_ahead vector.
#'
#' @examples \dontrun{
#' 
#' h_ahead(Forecast) <- 4L
#' 
#' }
#'
#' @importFrom methods validObject
#'
#' @export

setMethod("h_ahead<-", "Forecast", function(Forecast, value) {
  Forecast@h_ahead <- value
  validObject(Forecast)
  Forecast
})

#' Get the h_ahead slot of a h_ahead object
#'
#' \code{h_ahead} takes a \code{\link{Forecast}} object and gets the h_ahead
#' vector of the forecast.
#'
#' @param Forecast Forecast object.
#'
#' @return Vector of h_ahead values stored in the \link{Forecast} object.
#'
#' @examples \dontrun{
#' 
#' h_ahead(Forecast)
#' 
#' }
#'
#' @export

setGeneric("h_ahead", function(Forecast) standardGeneric("h_ahead"))

#' Get the h_ahead slot of a h_ahead object
#'
#' \code{h_ahead} takes a \code{\link{Forecast}} object and gets the h_ahead
#' vector of the forecast.
#'
#' @param Forecast Forecast object.
#'
#' @return Vector of h_ahead values stored in the \link{Forecast} object.
#'
#' @examples \dontrun{
#' 
#' h_ahead(Forecast)
#' 
#' }
#'
#' @export

setMethod("h_ahead", "Forecast", function(Forecast) Forecast@h_ahead)

#===============================================================================
# Forecast Accuracy Generics and Methods
#===============================================================================

#' Calculate MSE of a Forecast object
#'
#' \code{mse} takes a \code{\link{Forecast}} object and returns the MSE of the
#' forecast. MSE is calculated as:
#' \code{1/length(forecast) * sum((realized - forecast)^2)}
#'
#' @param Forecast Forecast object.
#'
#' @return MSE value.
#'
#' @examples 
#' 
#' my_forecast <- Forecast(
#'   origin   = as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31")),
#'   future   = as.Date(c("2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31")),
#'   forecast = c(4.21, 4.27, 5.32, 5.11),
#'   realized = c(4.40, 4.45, 4.87, 4.77),
#'   h_ahead  = 4L
#' )
#' 
#' mse(my_forecast)
#' 
#' @export

setGeneric("mse", function(Forecast) standardGeneric("mse"))

#' Calculate MSE of a Forecast object
#'
#' \code{mse} takes a \code{\link{Forecast}} object and returns the MSE of the
#' forecast. MSE is calculated as:
#' \code{1/length(forecast) * sum((realized - forecast)^2)}
#'
#' @param Forecast Forecast object.
#'
#' @return MSE value.
#'
#' @examples 
#' 
#' my_forecast <- Forecast(
#'   origin   = as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31")),
#'   future   = as.Date(c("2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31")),
#'   forecast = c(4.21, 4.27, 5.32, 5.11),
#'   realized = c(4.40, 4.45, 4.87, 4.77),
#'   h_ahead  = 4L
#' )
#' 
#' mse(my_forecast)
#' 
#' @export

setMethod("mse", "Forecast", function(Forecast) {
  
  forecast <- forc(Forecast)[!(is.na(forc(Forecast)) | is.na(realized(Forecast)))]
  realized <- realized(Forecast)[!(is.na(forc(Forecast)) | is.na(realized(Forecast)))]
  
  1/length(forecast) * sum((realized - forecast)^2)
  
})

#' Calculate RMSE of a Forecast object
#'
#' \code{rmse} takes a \code{\link{Forecast}} object and returns the RMSE of the
#' forecast. RMSE is calculated as: \code{sqrt(mse)}
#'
#' @param Forecast Forecast object.
#'
#' @return RMSE value.
#'
#' @examples 
#' 
#' my_forecast <- Forecast(
#'   origin   = as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31")),
#'   future   = as.Date(c("2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31")),
#'   forecast = c(4.21, 4.27, 5.32, 5.11),
#'   realized = c(4.40, 4.45, 4.87, 4.77),
#'   h_ahead  = 4L
#' )
#' 
#' rmse(my_forecast)
#' 
#' @export

setGeneric("rmse", function(Forecast) standardGeneric("rmse"))

#' Calculate RMSE of a Forecast object
#'
#' \code{rmse} takes a \code{\link{Forecast}} object and returns the RMSE of the
#' forecast. RMSE is calculated as: \code{sqrt(mse)}
#'
#' @param Forecast Forecast object.
#'
#' @return RMSE value.
#'
#' @examples 
#' 
#' my_forecast <- Forecast(
#'   origin   = as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31")),
#'   future   = as.Date(c("2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31")),
#'   forecast = c(4.21, 4.27, 5.32, 5.11),
#'   realized = c(4.40, 4.45, 4.87, 4.77),
#'   h_ahead  = 4L
#' )
#' 
#' rmse(my_forecast)
#' 
#' @export

setMethod("rmse", "Forecast", function(Forecast) {
  sqrt(mse(Forecast))
})

#' Calculate MAE of a Forecast object
#'
#' \code{mae} takes a \code{\link{Forecast}} object and returns the MAE of the
#' forecast. MAE is calculated as:
#' \code{1/length(forecast) * sum(abs(forecast - realized))}
#'
#' @param Forecast Forecast object.
#'
#' @return MAE value.
#'
#' @examples 
#' 
#' my_forecast <- Forecast(
#'   origin   = as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31")),
#'   future   = as.Date(c("2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31")),
#'   forecast = c(4.21, 4.27, 5.32, 5.11),
#'   realized = c(4.40, 4.45, 4.87, 4.77),
#'   h_ahead  = 4L
#' )
#' 
#' mae(my_forecast)
#' 
#' @export

setGeneric("mae", function(Forecast) standardGeneric("mae"))

#' Calculate MAE of a Forecast object
#'
#' \code{mae} takes a \code{\link{Forecast}} object and returns the MAE of the
#' forecast. MAE is calculated as:
#' \code{1/length(forecast) * sum(abs(forecast - realized))}
#'
#' @param Forecast Forecast object.
#'
#' @return MAE value.
#'
#' @examples 
#' 
#' my_forecast <- Forecast(
#'   origin   = as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31")),
#'   future   = as.Date(c("2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31")),
#'   forecast = c(4.21, 4.27, 5.32, 5.11),
#'   realized = c(4.40, 4.45, 4.87, 4.77),
#'   h_ahead  = 4L
#' )
#' 
#' mae(my_forecast)
#' 
#' @export

setMethod("mae", "Forecast", function(Forecast) {
  
  forecast <- forc(Forecast)[!(is.na(forc(Forecast)) | is.na(realized(Forecast)))]
  realized <- realized(Forecast)[!(is.na(forc(Forecast)) | is.na(realized(Forecast)))]
  
  1/length(forecast) * sum(abs(forecast - realized))
})

#' Calculate MAPE of a Forecast object
#'
#' \code{mape} takes a \code{\link{Forecast}} object and returns the MAPE of the
#' forecast. MAPE is calculated as:
#' \code{1/length(forecast) * sum(abs(realized - forecast) / realized)}
#'
#' @param Forecast Forecast object.
#'
#' @return MAPE value.
#'
#' @examples 
#' 
#' my_forecast <- Forecast(
#'   origin   = as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31")),
#'   future   = as.Date(c("2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31")),
#'   forecast = c(4.21, 4.27, 5.32, 5.11),
#'   realized = c(4.40, 4.45, 4.87, 4.77),
#'   h_ahead  = 4L
#' )
#' 
#' mape(my_forecast)
#'
#' @export

setGeneric("mape", function(Forecast) standardGeneric("mape"))

#' Calculate MAPE of a Forecast object
#'
#' \code{mape} takes a \code{\link{Forecast}} object and returns the MAPE of the
#' forecast. MAPE is calculated as:
#' \code{1/length(forecast) * sum(abs(realized - forecast) / realized)}
#'
#' @param Forecast Forecast object.
#'
#' @return MAPE value.
#'
#' @examples 
#' 
#' my_forecast <- Forecast(
#'   origin   = as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31")),
#'   future   = as.Date(c("2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31")),
#'   forecast = c(4.21, 4.27, 5.32, 5.11),
#'   realized = c(4.40, 4.45, 4.87, 4.77),
#'   h_ahead  = 4L
#' )
#' 
#' mape(my_forecast)
#'
#' @export

setMethod("mape", "Forecast", function(Forecast) {
  
  forecast <- forc(Forecast)[!(is.na(forc(Forecast)) | is.na(realized(Forecast)))]
  realized <- realized(Forecast)[!(is.na(forc(Forecast)) | is.na(realized(Forecast)))]
  
  1/length(forecast) * sum(abs(realized - forecast) / realized) 
  
})

#' Calculate R2 of a Forecast object
#'
#' \code{R2} takes a \code{\link{Forecast}} object and returns the R2 of the
#' forecast. R2 is calculated as:
#' \code{cor(forecast, realized)^2}
#'
#' @param Forecast Forecast object.
#'
#' @return R2 value.
#'
#' @examples 
#' 
#' my_forecast <- Forecast(
#'   origin   = as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31")),
#'   future   = as.Date(c("2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31")),
#'   forecast = c(4.21, 4.27, 5.32, 5.11),
#'   realized = c(4.40, 4.45, 4.87, 4.77),
#'   h_ahead  = 4L
#' )
#' 
#' R2(my_forecast)
#'
#' @export

setGeneric("R2", function(Forecast) standardGeneric("R2"))

#' Calculate R2 of a Forecast object
#'
#' \code{R2} takes a \code{\link{Forecast}} object and returns the R2 of the
#' forecast. R2 is calculated as:
#' \code{cor(forecast, realized)^2}
#'
#' @param Forecast Forecast object.
#'
#' @return R2 value.
#'
#' @examples 
#' 
#' my_forecast <- Forecast(
#'   origin   = as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31")),
#'   future   = as.Date(c("2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31")),
#'   forecast = c(4.21, 4.27, 5.32, 5.11),
#'   realized = c(4.40, 4.45, 4.87, 4.77),
#'   h_ahead  = 4L
#' )
#' 
#' R2(my_forecast)
#' 
#' @importFrom stats cor
#'
#' @export

setMethod("R2", "Forecast", function(Forecast) {
  
  forecast <- forc(Forecast)[!(is.na(forc(Forecast)) | is.na(realized(Forecast)))]
  realized <- realized(Forecast)[!(is.na(forc(Forecast)) | is.na(realized(Forecast)))]
  
  stats::cor(forecast, realized)^2
  
})

#===============================================================================
# forc2df Function
#===============================================================================

#' Collect a Forecast object to a data frame
#'
#' \code{forc2df} takes one or more objects of the Forecast class and collects
#' them into a data frame. Returns a data frame with all of the information that
#' was stored in the Forecast objects. If multiple forecasts are being
#' collected, all forecasts must have identical future and realized values.
#'
#' @param ... One or multiple forecasts of the class Forecast.
#'
#' @return \code{data.frame} object that contains forecast information.
#'
#' @seealso
#' For a detailed example see the help vignette:
#' \code{vignette("lmForc", package = "lmForc")}
#'
#' @examples \dontrun{
#'
#' forc2df(x1_forecast)
#'
#' forc2df(x1_forecast, x2_forecast)
#'
#'}
#'
#' @export

forc2df <- function(...) {

  forecast_names <- as.character(sys.call())[-1]
  forecasts <- list(...)

  # Input validation.
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

  if (length(unique(lapply(forecasts, function(x) x@realized))) > 1) {
    stop("* all forecasts must have identical realized values.")
  }

  # For each future value, find the latest origin in all forecasts.
  origin_vecs <- lapply(forecasts, function(x) x@origin)
  origin      <- Reduce(pmax, origin_vecs)

  future <- forecasts[[1]]@future
  realized <- forecasts[[1]]@realized

  forecast_vecs <- lapply(forecasts, function(x) x@forecast)
  forecasts <- data.frame(do.call(cbind, forecast_vecs))
  
  if (length(forecast_names)) == 1 {
    colnames(forecasts) <- "forecast"
  } else {
    colnames(forecasts) <- forecast_names
  }
  
  cbind(origin, future, forecasts, realized)
}
