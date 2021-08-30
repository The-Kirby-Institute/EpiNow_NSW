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

####

GetKeyEstimates <- function(estimates, regional, regionName = NULL) {
   
  if(regional) {
    output <- estimates$summary$results$estimates$summarised %>%
      filter(region == regionName) 
  } else {
    output <- estimates$estimates$summarised
  }
  
  return(output)
  
}

GetKeyResults <- function(output, indicator) {
  
  keyIndicators <- c("R", "growth_rate", "infections", "reported_cases")
  
  if (indicator %in% keyIndicators) {
    
    return(output %>% 
        filter(variable == indicator) %>% 
        select(-strat, -type)) 
  } else {
    stop("Indicator entered not a key indicator") 
  }
}

SaveKeyResults <- function(results, output_dir, indicator) {
  
  filename <- ifelse(indicator == "R", "rt", indicator)
  
  data.table::fwrite(results,file.path(output_dir, paste0(filename, ".csv")))
  
  return(invisible(NULL))
}

SaveAllIndicators <- function(key_estimates, output_dir, output_date) {
  
  keyIndicators <- c("R", "growth_rate", "infections", "reported_cases")
  
  for (indicator in keyIndicators) {
    SaveKeyResults(
      GetKeyResults(key_estimates, indicator), 
      file.path(output_dir, output_date), 
      indicator)
  }
  
  return(invisible(NULL))
}
