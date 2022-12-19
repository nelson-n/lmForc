
library(lmForc)
library(ggplot2)

#===============================================================================
# accuracy_table
#===============================================================================

#' Convert a list of a list of Forecast objects into a data.frame displaying
#' the accuracy of each forecast at various h_ahead forecast horizons.
#' 
#' Given a list object that contains lists of Forecast objects at different 
#' h_ahead forecast horizons, creates a data.frame with one column for each
#' forecast and rows showing the accuracy of each forecast at various h_ahead
#' forecast horizons. Provides a quick way to compare the accuracy of various
#' forecasts at various h_ahead forecast horizons. For each h_ahead forecast
#' horizon \code{h} in \code{horizon}, each forecast in the list of forecasts is 
#' subsetted to index \code{h} and the accuracy of each Forecast object is calculated using 
#' the accuracy function specificed in \code{metric}. All forecast accuracies 
#' for a given \code{h} are compiled into one row of the output data.frame. The 
#' output data.frame has \code{ncol} equal to the length of the forcs argument and \code{nrow} 
#' equal to the length of the horizon argument. Note that the order of the Forecast 
#' objects in each list matters. If one of the integers in the horizon argument is 4,
#' then the function will compute the forecast accuracy of each Forecast object at index 
#' 4 of the inner list and output these values as one row in the output data.frame. 
#' If the Forecast objects in the inner list are unordered or of incomparable 
#' length, the output data.frame will not contain fair accuracy comparisons.
#' 
#' @param forcs List of a list of Forecast objects. The outer list contains a 
#' list for each forecast. The inner list contains Forecast objects at ordered 
#' h_ahead forecast horizons.
#' @param horizon Vector of numeric values representing the h_ahead forecast 
#' horizons to compare in the output data.frame.
#' @param metric Accuracy metric used to compute forecast accuracy. Must be one of:
#' "mse", "rmse", "mae", "mape", "R2".
#' @param colnames Character vector of column names equal in length to forcs.
#' @param rownames Character vector of row names equal in length to horizon.
#'
#' @return data.frame displaying the accuracy of each forecast at various time
#' horizons.
#' 

#-------------------------------------------------------------------------------
# accuracy_table
#-------------------------------------------------------------------------------

accuracy_table <- function(forcs, horizon, metric, colnames, rownames) {
  
  acc_func <- eval(parse(text = metric))
  unique_lens <- vector(mode = "numeric", length = length(horizon))
  table_rows <- vector(mode = "list", length = length(horizon))
  
  # Input validation.
  if (!(metric %in% c("mse", "rmse", "mae", "mape"))) {
    stop('* metric must be either "mse", "rmse", "mae", or "mape": metric = "mse"')
  }
  
  if (length(colnames) != length(forcs)) {
    stop("The length of colnames must equal the length of forcs.")
  }
  
  if (length(rownames) != length(horizon)) {
    stop("The length of rownames must equal the length of horizon.")
  }
  
  # Iterate across forecast horizons.
  for (i in 1:length(horizon)) {
    
    h <- horizon[[i]]
    
    # Check if the future values of each forecast are identical.
    unique_lens[[i]] <- length(unique(lapply(forcs, function(x) future(x[[h]]))))
    
    # Calculate forecast accuracy.
    table_rows[[i]] <- sapply(forcs, function(x) acc_func(x[[h]]))
    
  }
  
  # Warn if the future values at any forecast horizon are not all equal.
  warn_h <- paste(as.character(horizon[which(unique_lens > 1)]), collapse = ", ")
  
  if (any(unique_lens > 1)) {
    warning(paste0(
      "* Future values are not all equal at horizons: ", warn_h, 
      ".\n Forecast accuracy will be computed over different sample periods."
    ))
  }
  
  table <- do.call(rbind, table_rows)
  colnames(table) <- colnames
  rownames(table) <- rownames
  return(table)
  
}

#===============================================================================
# plot_forc
#===============================================================================

#' Generate a ggplot of one or multiple Forecast objects.
#' 
#' Given a single Forecast object or a list of Forecast objects, creates a ggplot
#' call that shows each forecast and the realized value on the y-axis and the
#' future time object on the x-axis. The ggplot is rendered and the ggplot call
#' is returned to the console. Parameters of the ggplot such as axis range and 
#' line colors may be adjusted with additional arguments. Note that all Forecast 
#' objects must be forecasting the same entity and thus have identical future values.
#' 
#' @param forcs Single Forecast object or list of Forecast objects.
#' @param labels Character vector of line names.
#' @param colors Character vector of line colors.
#' @param xlab Character value used to label the x-axis.
#' @param ylab Character value used to label the y-axis.
#' @param title Character value used to set the plot title.
#' @param caption Character value or vector used to set the plot caption.
#' @param ylim Numeric vector of length two that contains: (y-axis minimum, y-axis maximum).
#' @param legend.position Numeric vector of length two that sets the legend position.
#' @param return_df Boolean, optionally returns a data.frame of all forecasts in case
#' the user wants to manually edit the ggplot call and run it again.
#'
#' @return ggplot displaying forecasted and realized values.
#' 

#-------------------------------------------------------------------------------
# plot_forc
#-------------------------------------------------------------------------------

plot_forc <- function(forcs, labels, colors, xlab, ylab, title = "", caption = "",
  ylim = NULL, legend.position = c(.10, .90), return_df = FALSE) {
  
  # Input validation.
  if (length(forcs) != length(labels)) {
    stop("* length of forcs must equal the length of labels.")
  }
  
  if (length(forcs) != length(colors)) {
    stop("* length of forcs must equal the length of colors.")
  }
  
  # Convert individual Forecast object to data.frame.
  if (class(forcs) == "Forecast") df <- forc2df(forcs)
  
  # Convert list of Forecast objects to data.frame.
  if (class(forcs) == "list") {
    
    for (i in 1:length(forcs)) {
      if (i == 1) forcs_str <- "forcs[[1]]"
      else if (i == length(forcs)) forcs_str <- paste0(forcs_str, ", forcs[[", i, "]]")
      else forcs_str <- paste0(forcs_str, ", forcs[[", i, "]]")
    }
    
    eval(parse(text = paste0("df <- forc2df(", forcs_str, ")")))
  }
  colnames(df) <- c("origin", "future", paste0("forc", 1:length(forcs)), "realized")
    
  # Plotting call.
  
  # Data, aesthetics, and realized values geom_line.
  data <- "ggplot(data = df, aes(x = future)) + 
    geom_line(aes(y = realized, colour = 'Realized Values'), lwd = 0.6) + \n"
  
  # Generate geom_line for each forecast.
  geom_lines <- vector(mode = "character", length = length(forcs))
  
  for (i in 1:length(forcs)) {
    geom_lines[[i]] <- paste0(
      "   geom_line(aes(y = forc", i, ", colour = '", labels[[i]], "'), lwd = 0.6) + \n"
    )
  }
  geom_lines = paste(geom_lines, collapse = " ")
  
  # Convert breaks, colors, legend.position to vector call.
  colors <- paste0("c('", paste0(c("black", colors), collapse = "', '"), "')")
  breaks <- paste0("c('", paste0(c("Realized Values", labels), collapse = "', '"), "')")
  legend.position <- paste0("c('", paste0(legend.position, collapse = "', '"), "')")
  
  # Set scale, labels, and theme.
  aes <- paste0("scale_color_manual(
    breaks = ", breaks, ", 
    values = ", colors, "
  ) + 
  labs(title = '", title, "', 
    x = '", xlab, "', 
    y = '", ylab, "', 
    caption = '", caption, "',
    colour = NULL
  ) + 
  theme_light() + 
  theme(legend.position = ", legend.position, ")"
   )
  
  # Set y-limits.
  if (is.null(ylim) == FALSE) {
    lim <- paste0(" +
  coord_cartesian(ylim = c(", ylim[[1]], ", ", ylim[[2]], "))")
  } else {
    lim <- ""
  }
  
  # Create ggplot call and print it to console.
  call <- paste(data, geom_lines, aes, lim, collapse = " ")
  cat(call)
  
  # Return either ggplot object or data.frame.
  if (return_df == TRUE) {
    return(df)
  } else {
    return(eval(parse(text = call)))
}

