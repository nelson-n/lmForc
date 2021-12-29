
#' States weighted forecast
#' 
#' \code{states_weighted_forc} takes two or more forecasts, a data frame, matrix,
#' or array of matching variables, an optional vector of time data associated with
#' the matching variables, a matching window size, a matching function, and an
#' error function. For each forecast period, \code{matching_vars} are standardized 
#' and the current state of the world is set as the the past \code{matching_window}
#' periods of the matching variables. The current state is compared to all past
#' periods of the matching variables using the matching function. The current state
#' is matched to the past state that minimizes the matching function. The forecast
#' error function is then used to compute the accuracy of each forecast over the matched
#' past state. Forecast weights are computed based on this forecast accuracy,
#' and the current period forecast is subsequently computed based on the forecast
#' weights. Produces a weighted average of multiple forecasts based on how each
#' forecast performed during the past state that is most similar to the current
#' state of the world.
#' 
#' Forecasts are weighted in each period with the function below. The error
#' function used is MSE or RMSE depending on user selection. This example shows
#' MSE errors.
#' \deqn{weight = (1 / MSE(forecast)) / (1 / sum(MSE(forecasts)))}
#' 
#' @param ... Two or more forecasts of class Forecast.
#' @param matching_vars data frame, array, or matrix of variables used to match 
#'   the current state of the world to a past state.
#' @param time_vec Vector of any class that is equal in length to the data
#'   in \code{matching_vars}.
#' @param matching_window Integer representing the window size over which the
#'   current state of the world is matched to a past state. Forecasts are also
#'   weighted based on their accuracy over \code{matching_window} periods.
#' @param matching Character, "euclidean", "mse", or "rmse". Selects the function
#'   used to match the current state of the world to a past state.
#' @param errors Character, either "mse" or "rmse". Selects whether forecast
#'   accuracy is evaluated using mean squared errors or root mean squared
#'   errors.
#' @param return_weights Boolean, selects whether the weights used to weight
#'   forecasts in each period are returned. If TRUE, a data frame of weights and 
#'   matched periods is returned to the Global Environment.
#'
#' @return \code{\link{Forecast}} object that contains the state weighted forecast.
#'
#' @seealso
#' For a detailed example see the help vignette:
#' \code{vignette("lmForc", package = "lmForc")}
#'
#' @examples 
#' 
#' date <- as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
#'                   "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31",
#'                   "2012-03-31", "2012-06-30"))
#' 
#' future <- as.Date(c("2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31",
#'                     "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31",
#'                     "2013-03-31", "2013-06-30"))
#' 
#' y  <- c(1.09, 1.71, 1.09, 2.46, 1.78, 1.35, 2.89, 2.11, 2.97, 0.99)
#' x1 <- c(4.22, 3.86, 4.27, 5.60, 5.11, 4.31, 4.92, 5.80, 6.30, 4.17)
#' x2 <- c(10.03, 10.49, 10.85, 10.47, 9.09, 10.91, 8.68, 9.91, 7.87, 6.63)
#' 
#' data <- data.frame(date, y, x1, x2)
#' matching_vars <- data[, c("x1", "x2")]
#' 
#' y1_forecast <- Forecast(
#'   origin = date,
#'   future = future,
#'   forecast = c(1.33, 1.36, 1.38, 1.68, 1.60, 1.55, 1.32, 1.22, 1.08, 0.88),
#'   realized = c(1.78, 1.35, 2.89, 2.11, 2.97, 0.99, 1.31, 1.41, 1.02, 1.05),
#'   h_ahead = 4L
#' )
#' 
#' y2_forecast <- Forecast(
#'   origin = date,
#'   future = future,
#'   forecast = c(0.70, 0.88, 1.03, 1.05, 1.01, 0.82, 0.95, 1.09, 1.07, 1.06),
#'   realized = c(1.78, 1.35, 2.89, 2.11, 2.97, 0.99, 1.31, 1.41, 1.02, 1.05),
#'   h_ahead = 4L
#' )
#' 
#' states_weighted_forc(
#'   y1_forecast, y2_forecast,
#'   matching_vars = matching_vars,
#'   time_vec = data$date,
#'   matching_window = 2L,
#'   matching = "euclidean",
#'   errors = "mse",
#'   return_weights = FALSE
#' )
#' 
#' states_weighted_forc(
#'   y1_forecast, y2_forecast,
#'   matching_vars = matching_vars,
#'   time_vec = data$date,
#'   matching_window = 3L,
#'   matching = "rmse",
#'   errors = "rmse"
#' )
#' 

#===============================================================================
# States Weighted Forecast
#===============================================================================

#' @export

states_weighted_forc <- function(..., matching_vars, time_vec = NULL, matching_window, 
  matching = "euclidean", errors = "mse", return_weights = FALSE) {
  
  forecasts <- list(...)
  
  # If time_vec is null create numeric time_vec.
  if (is.null(time_vec) == TRUE) {
    time_vec <- 1:nrow(matching_vars)
  }
  
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
  
  if (length(unique(lapply(forecasts, function(x) class(x@origin)))) > 1) {
    stop(paste0("* origin values of all forecasts must be of the same class.\n",
                "  * origin values are currently of the class: ",
                paste(lapply(forecasts, function(x) class(x@origin)), collapse = ", ")))
  }

  if (length(unique(lapply(forecasts, function(x) x@future))) > 1) {
    stop("* all forecasts must have identical future values.")
  }
  
  if (!(class(matching_vars)[[1]] %in% c("data.frame", "matrix", "array"))) {
    stop(paste0(" * matchings_vars must be of class data.frame, matrix, or array.\n",
                "  * matching_vars are currently of the class: ",
                paste(class(matching_vars)[[1]])))
  }

  if (all(sapply(matching_vars, is.numeric)) == FALSE) {
    stop("* all matching_vars must be numeric.")
  }
  
  if (is.null(time_vec) == FALSE & class(forecasts[[1]]@origin) != class(time_vec)) {
    stop(paste0("* The class of time_vec must equal the class of forecast origin values.\n",
                "  * time_vec is of class: ", class(time_vec),
                "\n  * forecast origin values are of class: ", class(forecasts[[1]]@origin)))
  }

  if (is.null(time_vec) == FALSE & length(time_vec) != nrow(matching_vars)) {
    stop(paste0("* Length of time_vec must equal the number of rows in the matching_vars data.\n",
                "  * Length of time_vec: ", length(time_vec),
                "\n  * Number of rows in lm_call data: ", nrow(matching_vars)))
  }

  if (is.integer(matching_window) != TRUE) {
    stop("* matching_window must be an integer: matching_window = 4L")
  }

  if (length(matching_window) > 1) {
    stop("* matching_window must be of length one: matching_window = 4L")
  }

  if (!(errors %in% c("mse", "rmse"))) {
    stop('* errors must be either "mse" or "rmse": errors = "mse"')
  }
  
  if (!(matching %in% c("euclidean", "mse", "rmse"))) {
    stop('* matching must be either "euclidean", "mse", or "rmse": matching = "euclidean"')
  }

  # For each future value, find the latest origin in all forecasts.
  origin_vecs <- lapply(forecasts, function(x) x@origin)
  origin_vec  <- Reduce(pmax, origin_vecs)
  
  if (time_vec[[1]] > origin_vec[[1]]) {
    stop(paste0("* matching_vars data must begin before or at the same period as the first forecast origin.\n",
                "  * First matching_vars observation: ", time_vec[[1]],
                "\n  * First forecast origin: ", origin_vec[[1]]))
  }
  
  if (time_vec[[length(time_vec)]] < origin_vec[[length(origin_vec)]]) {
    stop(paste0("* matching_vars data must end after or at the same period as the last forecast origin.\n",
                "  * Last matching_vars observation: ", time_vec[[length(time_vec)]],
                "\n  * Last forecast origin: ", origin_vec[[length(origin_vec)]]))
  }

  # Standardization function. 
  standardize <- function(x) (x - mean(x)) / sd(x)
  
  # Matching functions.
  euclidean <- function(x, y) sqrt(sum((x - y)^2))
  
  # MSE function.
  mse <- function(x, y) {
    1/length(x) * sum((y - x)^2)
  }
  
  # RMSE function.
  rmse <- function(x, y) {
    sqrt(mse(x, y))
  }
  
  # Specify matching function and error function.
  matching_function <- eval(parse(text = matching))
  error_function <- eval(parse(text = errors))
  
  # Find periods over which the matching algorithm can be performed.
  forecast_periods <- which(origin_vec >= forecasts[[1]]@future[[matching_window]])
  
  # Prevent overlapping windows in first observations.
  if ((forecast_periods[[1]] - matching_window) <= matching_window) {
    forecast_periods <- forecast_periods[-c(1:(matching_window - (forecast_periods[[1]] - matching_window - 1)))]
  }
  
  # Subset matching_vars and time_vec to observations that will be used.
  matching_vars <- matching_vars[which(time_vec >= origin_vec[[1]] & time_vec <= origin_vec[[length(origin_vec)]]), ] 
  matching_vars <- as.matrix(matching_vars)
  time_vec <- time_vec[which(time_vec >= origin_vec[[1]] & time_vec <= origin_vec[[length(origin_vec)]])] 
  
  # Standardize matching vars.
  matching_vars <- apply(matching_vars, MARGIN = 2, standardize)
  
  # Prepare forecasting loop.
  weights <- vector(mode = "list", length = length(forecast_periods))
  matched_state_begin <- vector(mode = "numeric", length = length(forecast_periods))
  matched_state_end <- vector(mode = "numeric", length = length(forecast_periods))
  weighted_forc <- vector(mode = "numeric", length = length(forecast_periods))
    
  for (i in 1:length(forecast_periods)) {
    
    forecast_period <- forecast_periods[[i]]
    
    # Find current state of the world.
    current_window <- max(which(time_vec <= origin_vec[[forecast_period]]))
    current_window <- (current_window - matching_window + 1):current_window
    current_state <- as.matrix(matching_vars[current_window, ])
    
    # Find candidate past states for matching.
    past_states <- as.matrix(matching_vars[1:(current_window[[1]] - 1), ])
    num_past_states <- (nrow(past_states) - matching_window + 1)
    
    # Match current state of the world to all candidate past states.
    state_matches <- vector(mode = "numeric", length = num_past_states)
    
    for (ps in 1:num_past_states) {
      
      matching_score <- 0
      past_state <- as.matrix(past_states[(ps:(ps + matching_window - 1)), ])
      
      # Sum matching scores for each matching variable.
      for (rn in 1:ncol(past_state)) {
        matching_score <- matching_score + matching_function(current_state[, rn], past_state[, rn])
      }
      
      state_matches[[ps]] <- matching_score
      
    }
    
    # Find best state match and calculate forecast weights over this period.
    weighting_period <- which.min(state_matches):(which.min(state_matches) + matching_window - 1)
    
    wp_forecasts <- lapply(forecasts, function(x) x@forecast[weighting_period])
    wp_realized  <- lapply(forecasts, function(x) x@realized[weighting_period])
    
    errors <- mapply(error_function, wp_forecasts, wp_realized)
    weight <- (1 / errors) / sum(1 / errors)
    
    weights[[i]] <- weight
    
    # Record start and end of matched period.
    matched_state_begin[[i]] <- weighting_period[[1]]
    matched_state_end[[i]] <- weighting_period[[length(weighting_period)]]
    
    # Apply weights to current forecasts.
    c_forecasts <- sapply(forecasts, function(x) x@forecast[forecast_period])
    weighted_forc[[i]] <- sum(c_forecasts * weight)
    
  }
    
  if (return_weights == TRUE) {
    
    # Find forecast names.
    argnames   <- sys.call()
    argnames   <- unlist(lapply(argnames[-1], as.character))
    forc_names <- paste0(argnames[1:length(forecasts)])

    weights <- data.frame(do.call(rbind.data.frame, weights))

    weights <- cbind.data.frame(
      origin_vec[forecast_periods],
      forecasts[[1]]@future[forecast_periods],
      weights,
      time_vec[matched_state_begin],
      time_vec[matched_state_end]
    )

    colnames(weights) <- c("origin", "future", forc_names, "matched_state_begin", "matched_state_end")
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
