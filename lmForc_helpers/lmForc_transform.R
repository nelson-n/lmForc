
library(lmForc)

#===============================================================================
# convert_bytime
#===============================================================================

#' # Convert a list of h_ahead format Forecast objects to a time format Forecast 
#' object.
#'
#' Given a list of forecasts with different h_ahead values, converts the forecasts
#' to time format based on the time object passed to the values argument. Converts
#' Forecast objects that have have homogenous h_ahead values to Forecast objects
#' with homogenous origin or future values. 
#'
#' @param forcs List of Forecast objects.
#' @param values Single time object or a vector of time objects. 
#' @param slot Character representing whether the list of Forecasts will be converted
#'   to homogenous origin or future values. Must be either "origin" or "future".
#'
#' @return Single Forecast object or list of Forecast objects in time format.
#'
#' @examples 
#' 
#' # The following forecasts are in h_ahead format. All forecasts come from the 
#' # same source (forc1) and have the same origin values. However, the forecasts
#' # are for different periods ahead.
#' 
#' forc1_1h <- Forecast(
#'   origin = as.Date(c("2010-02-17", "2010-05-14", "2010-07-22", "2010-12-22")),
#'   future = as.Date(c("2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31")),
#'   forecast = c(4.27, 3.36, 4.78, 5.45),
#'   realized = c(4.96, 4.17, 4.26, 4.99),
#'   h_ahead = 1
#' )
#' 
#' forc1_2h <- Forecast(
#'   origin = as.Date(c("2010-02-17", "2010-05-14", "2010-07-22", "2010-12-22")),
#'   future = as.Date(c("2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30")),
#'   forecast = c(3.77, 3.82, 4.53, 4.89),
#'   realized = c(4.17, 4.26, 4.99, 5.33),
#'   h_ahead = 2
#' )
#' 
#' forc1_3h <- Forecast(
#'   origin = as.Date(c("2010-02-17", "2010-05-14", "2010-07-22", "2010-12-22")),
#'   future = as.Date(c("2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30")),
#'   forecast = c(3.52, 4.22, 5.03, 5.78),
#'   realized = c(4.26, 4.99, 5.33, 5.21),
#'   h_ahead = 3
#' )
#' 
#' forcs <- list(forc1_1h, forc1_2h, forc1_3h)
#' 
#' convert_bytime(forcs, value = as.Date("2010-05-14"), slot = "origin")
#' 
#' convert_bytime(
#'   forcs, 
#'   value = as.Date(c("2010-07-22", "2010-12-22")), 
#'   slot = "origin"
#' )
#' 

#-------------------------------------------------------------------------------
# convert_bytime
#-------------------------------------------------------------------------------

convert_bytime <- function(forcs, values, slot) {
  
  forcs_list <- vector(mode = "list", length = length(values))
  
  for (i in 1:length(values)) {
    
    value <- values[[i]]
    forc <- subset_bytime(forcs, values = value, slot = slot)
    
    for (o in 1:length(forc)) {
      
      if (o == 1) {
        origin <- c(origin(forc[[1]]))
        future <- c(future(forc[[1]]))
        forecast <- c(forc(forc[[1]]))
        realized <- c(realized(forc[[1]]))
      } else {
        origin <- c(origin, origin(forc[[o]]))
        future <- c(future, future(forc[[o]]))
        forecast <- c(forecast, forc(forc[[o]]))
        realized <- c(realized, realized(forc[[o]]))
      }
    }
    
    forc <- Forecast(
      origin = origin,
      future = future,
      forecast = forecast,
      realized = realized,
      h_ahead = NA
    )
    
    forcs_list[[i]] <- forc
  }
  
  if (length(forcs_list) == 1) {
    return(forcs_list[[1]])
  } else {
    return(forcs_list)
  }
}

#===============================================================================
# transform_bytime
#===============================================================================

#' # Convert a list of h_ahead format Forecast objects to a list of time format 
#' Forecast objects.
#' 
#' Given a list of forecasts with different h_ahead values, converts all forecasts
#' in the list to time format. Transforms a list of Forecast objects that have 
#' homogenous h_ahead values to a list of Forecast objects with homogenous origin 
#' or future values. 
#'
#' @param forcs List of Forecast objects.
#' @param slot Character representing whether the list of Forecasts will be converted
#'   to a list of Forecasts with homogenous origin or future values. Must be either 
#'   "origin" or "future".
#'
#' @return List of Forecast objects in time format.
#'
#' @examples 
#' 
#' # The following forecasts are in h_ahead format. All forecasts come from the 
#' # same source (forc1) and have the same origin values. However, the forecasts
#' # are for different periods ahead.
#' 
#' forc1_1h <- Forecast(
#'   origin = as.Date(c("2010-02-17", "2010-05-14", "2010-07-22", "2010-12-22")),
#'   future = as.Date(c("2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31")),
#'   forecast = c(4.27, 3.36, 4.78, 5.45),
#'   realized = c(4.96, 4.17, 4.26, 4.99),
#'   h_ahead = 1
#' )
#' 
#' forc1_2h <- Forecast(
#'   origin = as.Date(c("2010-02-17", "2010-05-14", "2010-07-22", "2010-12-22")),
#'   future = as.Date(c("2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30")),
#'   forecast = c(3.77, 3.82, 4.53, 4.89),
#'   realized = c(4.17, 4.26, 4.99, 5.33),
#'   h_ahead = 2
#' )
#' 
#' forc1_3h <- Forecast(
#'   origin = as.Date(c("2010-02-17", "2010-05-14", "2010-07-22", "2010-12-22")),
#'   future = as.Date(c("2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30")),
#'   forecast = c(3.52, 4.22, 5.03, 5.78),
#'   realized = c(4.26, 4.99, 5.33, 5.21),
#'   h_ahead = 3
#' )
#' 
#' forcs <- list(forc1_1h, forc1_2h, forc1_3h)
#' 
#' transform_bytime(forcs, slot = "origin")
#' 

#-------------------------------------------------------------------------------
# transform_bytime
#-------------------------------------------------------------------------------

transform_bytime <- function(forcs, slot = "future") {
  
  slot_func <- eval(parse(text = paste0("lmForc::", slot)))
  values <- slot_func(forcs[[1]])
  convert_bytime(forcs, values = values, slot = slot)
  
}

#===============================================================================
# convert_byh
#===============================================================================

#' # Convert a list of time format Forecast objects to a h_ahead format Forecast 
#' object.
#'
#' Given a list of forecasts with homogenous origin or future values, converts the 
#' forecasts to h_ahead format based on the index passed to the index argument.
#' Subsets all forecasts at the index value and aggregates these forecasts into
#' an h_ahead Forecast object with h_ahead equal to the value passed to the
#' h_aheads argument.
#'
#' @param forcs List of Forecast objects with the same number of observations.
#' @param index Numeric or logical value or vector.
#' @param h_aheads Value or vector of h_ahead values that is equal in length
#'   to the index argument.
#'
#' @return Single Forecast object or list of Forecast objects in h_ahead format.
#'
#' @examples 
#' 
#' # The following forecasts are in time format. Each forecast was made at a
#' different time and represents a forecast for a number of h_ahead periods 
#' ahead.
#' 
#' forc1_t1 <- Forecast(
#'   origin = as.Date(c("2010-02-17", "2010-02-17", "2010-02-17")),
#'   future = as.Date(c("2010-06-30", "2010-09-30", "2010-12-31")),
#'   forecast = c(4.27, 3.77, 3.52),
#'   realized = c(4.96, 4.17, 4.26),
#'   h_ahead = NA
#' )
#' 
#' forc1_t2 <- Forecast(
#'   origin = as.Date(c("2010-05-14", "2010-05-14", "2010-05-14")),
#'   future = as.Date(c("2010-09-30", "2010-12-31", "2011-03-31")),
#'   forecast = c(3.36, 3.82, 4.22),
#'   realized = c(4.17, 4.26, 4.99),
#'   h_ahead = NA
#' )
#' 
#' forc1_t3 <- Forecast(
#'   origin = as.Date(c("2010-07-22", "2010-07-22", "2010-07-22")),
#'   future = as.Date(c("2010-12-31", "2011-03-31", "2011-06-30")),
#'   forecast = c(4.78, 4.53, 5.03),
#'   realized = c(4.26, 4.99, 5.33),
#'   h_ahead = NA
#' )
#' 
#' forc1_t4 <- Forecast(
#'   origin = as.Date(c("2010-12-22", "2010-12-22", "2010-12-22")),
#'   future = as.Date(c("2011-03-31", "2011-06-30", "2011-09-30")),
#'   forecast = c(5.45, 4.89, 5.78),
#'   realized = c(4.99, 5.33, 5.21),
#'   h_ahead = NA
#' )
#' 
#' forcs <- list(forc1_t1, forc1_t2, forc1_t3, forc1_t4)
#' 
#' convert_byh(forcs, index = 1L, h_aheads = 1)
#' 
#' convert_byh(forcs, index = 1:2, h_aheads = c(1, 2))
#' 

#-------------------------------------------------------------------------------
# convert_byh
#-------------------------------------------------------------------------------

convert_byh <- function(forcs, index, h_aheads) {
  
  # Input validation.
  if (length(unique(lapply(forcs, function(x) length(origin(x))))) > 1) {
    stop(" * All Forecast objects in forcs must have the same number of observations.")
  }
  
  if (!(class(index) %in% c("integer", "logical"))) {
    stop(paste0("* index must be of class integer or logical.\n",
                "  * index is currently of the class: ", class(index)))
  }
  
  if (length(index) != length(h_aheads)) {
    stop("* index must be the same length as h_aheads.")
  }
  
  forcs_list <- vector(mode = "list", length = length(index))
  
  for (i in 1:length(index)) {
    
    row <- index[[i]]
    h_ahead <- h_aheads[[i]]
    forc <- subset_forcs(forcs, row)
    
    for (o in 1:length(forc)) {
      
      if (o == 1) {
        origin <- c(origin(forc[[1]]))
        future <- c(future(forc[[1]]))
        forecast <- c(forc(forc[[1]]))
        realized <- c(realized(forc[[1]]))
      } else {
        origin <- c(origin, origin(forc[[o]]))
        future <- c(future, future(forc[[o]]))
        forecast <- c(forecast, forc(forc[[o]]))
        realized <- c(realized, realized(forc[[o]]))
      }
    }
    
    forc <- Forecast(
      origin = origin,
      future = future,
      forecast = forecast,
      realized = realized,
      h_ahead = h_ahead
    )
    
    forcs_list[[i]] <- forc
  }
  
  if (length(forcs_list) == 1) {
    return(forcs_list[[1]])
  } else {
    return(forcs_list)
  }
}

#===============================================================================
# transform_byh
#===============================================================================

#' # Convert a list of time format Forecast objects to a list of h_ahead format 
#' Forecast objects.
#' 
#' Given a list of forecasts with homogenous origin or future values, converts 
#' all forecasts in the list to h_ahead format.  
#'
#' @param forcs List of Forecast objects.
#' @param h_aheads Vector of h_ahead values that is equal in length to the 
#'   number of Forecast objects in forcs.
#'
#' @return List of Forecast objects in h_ahead format.
#'
#' @examples 
#' 
#' # The following forecasts are in time format. Each forecast was made at a
#' different time and represents a forecast for a number of h_ahead periods 
#' ahead.
#' 
#' forc1_t1 <- Forecast(
#'   origin = as.Date(c("2010-02-17", "2010-02-17", "2010-02-17")),
#'   future = as.Date(c("2010-06-30", "2010-09-30", "2010-12-31")),
#'   forecast = c(4.27, 3.77, 3.52),
#'   realized = c(4.96, 4.17, 4.26),
#'   h_ahead = NA
#' )
#' 
#' forc1_t2 <- Forecast(
#'   origin = as.Date(c("2010-05-14", "2010-05-14", "2010-05-14")),
#'   future = as.Date(c("2010-09-30", "2010-12-31", "2011-03-31")),
#'   forecast = c(3.36, 3.82, 4.22),
#'   realized = c(4.17, 4.26, 4.99),
#'   h_ahead = NA
#' )
#' 
#' forc1_t3 <- Forecast(
#'   origin = as.Date(c("2010-07-22", "2010-07-22", "2010-07-22")),
#'   future = as.Date(c("2010-12-31", "2011-03-31", "2011-06-30")),
#'   forecast = c(4.78, 4.53, 5.03),
#'   realized = c(4.26, 4.99, 5.33),
#'   h_ahead = NA
#' )
#' 
#' forc1_t4 <- Forecast(
#'   origin = as.Date(c("2010-12-22", "2010-12-22", "2010-12-22")),
#'   future = as.Date(c("2011-03-31", "2011-06-30", "2011-09-30")),
#'   forecast = c(5.45, 4.89, 5.78),
#'   realized = c(4.99, 5.33, 5.21),
#'   h_ahead = NA
#' )
#' 
#' forcs <- list(forc1_t1, forc1_t2, forc1_t3, forc1_t4)
#' 
#' transform_byh(forcs, h_aheads = c(1, 2, 3))
#' 

#-------------------------------------------------------------------------------
# transform_byh
#-------------------------------------------------------------------------------

transform_byh <- function(forcs, h_aheads) {
  
  nrows <- length(origin(forcs[[1]]))
  
  if (length(h_aheads) != nrows) {
    stop("* The length of h_aheads must match the number of observations of 
         each Forecast objects in forcs.")
  }
  
  convert_byh(forcs, index = 1:nrows, h_aheads = h_aheads)
}

