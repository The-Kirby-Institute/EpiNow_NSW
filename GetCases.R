#' Extract cases for NSW 
#' 
#' This function is used to extract the number of daily cases within each 
#' LGA from the case data and put into the right format for input into 
#' EpiNow2
#' 
#' @details    
#' 
#' @param raw_data Data frame with raw cases data acquire from NSW COVID
#' datasets website. 
#' @param region_level String specify level of region can be "NSW", 
#' "Sydney", or "LGA" TODO: LHD, postcode?
#' @param date_run Date specify date EpiNow will run to.
#' @param regions Vector of strings specifying regions of interest. 
#' Default is "all" which means all regions are included. 
#' 
#' @return Date table with 3 columns: date, confirm, region (dropped if
#' only one region)
#' 
#' @author Richard T Gray, \email{Rgray@kirby.unsw.edu.au}
#' 
#' @export
#' 
#'  
GetCases <- function(raw_data, region_level, start_date, regions = NULL) {
  
  # Put raw data into right format
  cases <- raw_data %>% 
    select(date = notification_date, 
      source = likely_source_of_infection, 
      region = lga_name19) %>%
    mutate(import_status = ifelse(source == "Overseas", "imported", 
      "local")) %>%
    filter(import_status == "local", date >= start_date) %>%
    select(-source, -import_status)
    
  if (region_level == "NSW") {
    cases <- cases %>%
      group_by(date) %>%
      summarise(confirm = n()) %>%
      ungroup() %>%
      select(date, confirm) %>%
      arrange(date)
    
  } else if (region_level == "Sydney") {
    # TODO
    # sydenyLGAs <- c()
  } else if (region_level == "LGA") {
    cases <- cases %>%
      group_by(date, region) %>%
      summarise(confirm = n()) %>%
      ungroup() %>%
      select(date, confirm, region) %>%
      arrange(date)
    
    if (!is.null(regions)) {
      if (regions[1] != "all") {
        cases <- cases %>%
          dplyr::filter(region %in% regions)
      }
    }
    
  } else {
    stop("Unknown region level option")
  }
  
  return(cases)
  
}
