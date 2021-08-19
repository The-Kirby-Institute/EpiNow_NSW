
#' Get Regions Cumulative Reported Cases
#'
#' @description 
#' Tweak of EpiNow2 get_regions_with_most_reports() to get each regions 
#' cumulative reported cases over previous set time window in order from 
#' highest to lowest.
#' @param time_window Numeric, number of days to include from latest date in data.
#' Defaults to 7 days.
#' @param no_regions Numeric, number of regions to return. Defaults to 6.
#' @inheritParams regional_epinow
#' @return A character vector of regions with the highest reported cases
#' @importFrom data.table copy setorderv
#' @importFrom lubridate days
#' @export
RegionsCumulativeReports <- function(reported_cases, time_window = 7) {
  
  cum_reports <- data.table::copy(reported_cases)
  cum_reports <-
    cum_reports[, .SD[date >= (max(date, na.rm = TRUE) - lubridate::days(time_window))],
      by = "region"
    ]
  cum_reports <- cum_reports[, .(confirm = sum(confirm, na.rm = TRUE)), by = "region"]
  cum_reports <- data.table::setorderv(cum_reports, cols = "confirm", order = -1)
  
  return(cum_reports)
}
