# Functions for NSW COVID analysis
# =================================

# Richard T. Gray

# This script contains miscellaneous functions useful for looking at 
# NSW COVID case data. Generally little functions that don't deserve their
# own file.

#' Plot cases overtime
#'
#' This function simple produces a nice plot of cases over time.
#' 
#' @details Needs the script code/PlotOptions.R to be sourced
#' 
#' @param cases_data data frame of with column of confirmed cases 
#' (confirm <int>) by date (date <date>)
#' @param plot_title string giving the title of the plot 
#' 
#' @import ggplot2
#' 
CasesPlot <- function(cases_data, plot_title) {
  
  plot <- ggplot(data = cases_data, aes(x = date, y = confirm)) +
    geom_line(colour = "red") +
    geom_point(colour = "red") +
    labs(x = "Date", y = "Daily cases", title = plot_title) +
    expand_limits(y = 0) +
    PlotOptions()  
  
  return(plot)
  
}
