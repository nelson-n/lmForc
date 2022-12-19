
library(lmForc)

#===============================================================================
# Subsetting Functions
#===============================================================================

#===============================================================================
# subset_forcs
#===============================================================================

#' Subset a list of Forecast objects by index.
#'
#' General function for subsetting all forecasts in a list of Forecast objects.
#'
#' @param forcs List of Forecast objects.
#' @param index Numeric or logical value or vector.
#'
#' @return List of subsetted Forecast objects.
#'
#' @examples 
#' 
#' forc1_1h <- Forecast(
#'   origin = as.Date(c("2010-02-17", "2010-05-14", "2010-07-22", "2010-12-05", "2011-03-10")),
#'   future = as.Date(c("2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30")),
#'   forecast = c(4.27, 3.36, 4.78, 5.45, 5.12),
#'   realized = c(4.96, 4.17, 4.26, 4.99, 5.38),
#'   h_ahead = 1
#' )
#' 
#' forc2_1h <- Forecast(
#'   origin = as.Date(c("2010-02-17", "2010-05-14", "2010-07-22", "2010-12-22", "2011-03-27")),
#'   future = as.Date(c("2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30")),
#'   forecast = c(4.01, 3.89, 3.31, 4.33, 4.61),
#'   realized = c(4.96, 4.17, 4.26, 4.99, 5.38),
#'   h_ahead = 1
#' )
#' 
#' forcs <- list(forc1_1h, forc2_1h)
#' 
#' subset_forcs(forcs, 1:4)
#' 
#' subset_forcs(forcs, origin(forc1_1h) >= as.Date("2010-12-31"))
#' 

#-------------------------------------------------------------------------------
# subset_forcs
#-------------------------------------------------------------------------------

#' @export

# Subset a list of Forecast objects by index.
subset_forcs <- function(forcs, index) {
  
  # Input validation.
  if (is.list(forcs) != TRUE) {
    stop(paste0("* forcs must be of class list.\n",
                "  * forcs is currently of the class: ", class(forcs)))
  }
  
  if (all(sapply(forcs, class) == "Forecast") != TRUE) {
    stop("* All forcs objects must be of class Forecast.")
  }
  
  if (!(class(index) %in% c("integer", "logical"))) {
    stop(paste0("* index must be of class integer or logical.\n",
                "  * index is currently of the class: ", class(index)))
  }
  
  lapply(forcs, function(x) x[index])
}
  
#===============================================================================
# subset_bytime
#===============================================================================

#' # Subset a list of Forecast objects by origin or future values.
#'
#' Function for subsetting all forecasts in a list of Forecast objects based
#' on origin or future values.
#'
#' @param forcs List of Forecast objects.
#' @param values Single time object or a vector of time objects. The class of the values
#'   must match the class of the origin and future values in the list of Forecast objects.
#' @param slot Character representing whether the list of Forecasts will be subset
#'   by origin or future values. Must be either "origin" or "future".
#'
#' @return List of subsetted Forecast objects.
#'
#' @examples 
#' 
#' forc1_1h <- Forecast(
#'   origin = as.Date(c("2010-02-17", "2010-05-14", "2010-07-22", "2010-12-05", "2011-03-10")),
#'   future = as.Date(c("2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30")),
#'   forecast = c(4.27, 3.36, 4.78, 5.45, 5.12),
#'   realized = c(4.96, 4.17, 4.26, 4.99, 5.38),
#'   h_ahead = 1
#' )
#' 
#' forc2_1h <- Forecast(
#'   origin = as.Date(c("2010-02-17", "2010-05-14", "2010-07-22", "2010-12-22", "2011-03-27")),
#'   future = as.Date(c("2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30")),
#'   forecast = c(4.01, 3.89, 3.31, 4.33, 4.61),
#'   realized = c(4.96, 4.17, 4.26, 4.99, 5.38),
#'   h_ahead = 1
#' )
#' 
#' forcs <- list(forc1_1h, forc2_1h)
#' 
#' subset_bytime(forcs, values = as.Date("2010-05-14"), slot = "origin")
#'
#' subset_bytime(
#'    forcs, 
#'    values = as.Date(c("2010-09-30", "2010-12-31", "2011-03-31")), 
#'    slot = "future"
#' )
#'

#-------------------------------------------------------------------------------
# subset_bytime
#-------------------------------------------------------------------------------

#' @export

subset_bytime <- function(forcs, values, slot) {
  
  # Input validation.
  if (is.list(forcs) != TRUE) {
    stop(paste0("* forcs must be of class list.\n",
                "  * forcs is currently of the class: ", class(forcs)))
  }
  
  if (all(sapply(forcs, class) == "Forecast") != TRUE) {
    stop("* All forcs objects must be of class Forecast.")
  }
  
  if (!(slot %in% c("origin", "future"))) {
    stop('* slot must be either "origin" or "future".')
  }
  
  slot_func <- eval(parse(text = paste0("lmForc::", slot)))
  
  if (length(unique(lapply(forcs, function(x) class(slot_func(x))))) > 1) {
    stop(paste0("* Not all Forecast objects in forcs have ", slot, " values of the same class.",
                " All forecasts in the list of Forecast objects must have ", slot, " values of the same class."))
  }
  
  if (class(values) != class(slot_func(forcs[[1]]))) {
    stop(paste0("* the class of values and the class of the time slot are different.\n",
                "  * values is currently of the class: ", class(values), "\n",
                "  * ", slot, " is currently of the class: ", class(slot_func(forcs[[1]])
  )))}
  
  for (i in 1:length(forcs)) {
    forcs[[i]] <- forcs[[i]][slot_func(forcs[[i]]) %in% values]
  }
  
  return(forcs)
}

#===============================================================================
# subset_identical
#===============================================================================

#' # Subset a list of Forecast objects to identical origin or future values.
#'
#' Function for subsetting all forecasts in a list of Forecast objects to 
#' overlapping origin or future values.
#'
#' @param forcs List of Forecast objects.
#' @param slot Character representing whether the list of Forecasts will be subset
#'   to identical origin or future values. Must be either "origin" or "future".
#'
#' @return List of subsetted Forecast objects with identical future or origin values.
#'
#' @examples 
#' 
#' forc1_1h <- Forecast(
#'   origin = as.Date(c("2010-02-17", "2010-05-14", "2010-07-22", "2010-12-05", "2011-03-10")),
#'   future = as.Date(c("2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30")),
#'   forecast = c(4.27, 3.36, 4.78, 5.45, 5.12),
#'   realized = c(4.96, 4.17, 4.26, 4.99, 5.38),
#'   h_ahead = 1
#' )
#' 
#' forc2_1h <- Forecast(
#'   origin = as.Date(c("2010-02-17", "2010-05-14", "2010-07-22", "2010-12-22", "2011-03-27")),
#'   future = as.Date(c("2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30")),
#'   forecast = c(4.01, 3.89, 3.31, 4.33, 4.61),
#'   realized = c(4.96, 4.17, 4.26, 4.99, 5.38),
#'   h_ahead = 1
#' )
#' 
#' forcs <- list(forc1_1h, forc2_1h)
#' 
#' subset_identical(forcs, slot = "origin")
#'

#-------------------------------------------------------------------------------
# subset_identical
#-------------------------------------------------------------------------------

subset_identical <- function(forcs, slot) {
  
  # Input validation.
  if (is.list(forcs) != TRUE) {
    stop(paste0("* forcs must be of class list.\n",
                "  * forcs is currently of the class: ", class(forcs)))
  }
  
  if (all(sapply(forcs, class) == "Forecast") != TRUE) {
    stop("* All forcs objects must be of class Forecast.")
  }
  
  if (!(slot %in% c("future", "origin"))) {
    stop('* slot must be either "future" or "origin".')
  }
  
  slot_func <- eval(parse(text = paste0("lmForc::", slot)))
  slot_class <- lapply(forcs, function(x) class(slot_func(x)))
  
  if (length(unique(slot_class)) > 1) {
    stop(paste0(
      "* the class of the ", slot, " values in each in all forcs objects must be identical."))
  }
  
  values <- Reduce(intersect, lapply(forcs, slot_func))
  values <- slot_func(forcs[[1]])[unclass(slot_func(forcs[[1]])) %in% values]
  subset_bytime(forcs, values, slot)
}

