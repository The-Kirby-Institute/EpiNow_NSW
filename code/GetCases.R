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
#' @param date_option 
#' @param start_date Date specifying date EpiNow will run from.
#' 
#' @return Date table with 3 columns: date, confirm, region (dropped if
#' only one region)
#' 
#' @author Richard T Gray, \email{Rgray@kirby.unsw.edu.au}
#' 
#' @export
#' 
GetCases <- function(raw_data, data_option, start_date) {
  
  # Put raw data into right format
  if (data_option == "nsw_website") {
    cases <- raw_data %>% 
      select(date = notification_date, 
        source = likely_source_of_infection, 
        region = lga_name19) %>%
      mutate(import_status = ifelse(source == "Overseas", "imported", 
        "local")) %>%
      filter(import_status == "local", date >= start_date) %>%
      select(-source, -import_status) 
  } else if (data_option == "file") {
    # Need to tweak this to whatever file you are using
    # cases <- raw_data %>%
    #   select(date = EARLIEST_CONFIRMED_OR_PROBABLE,
    #     region = LGA_NAME_2020,
    #     confirm = count)
  } else {
    stop("Unknown data option")
  }
  
  # First aggregate cases by LGA
  cases <- cases %>%
      group_by(date, region) %>%
      summarise(confirm = n()) %>%
      ungroup() %>%
      select(date, confirm, region) %>%
      arrange(date) %>%
      arrange(region)
  
  # Then aggregate cases to get overall NSW cases
   nswCases <- cases %>%
      group_by(date) %>%
      summarise(confirm = sum(confirm)) %>%
      ungroup() %>%
      mutate(region = "NSW") %>%
      arrange(date)
  
  # TODO: Add aggregates for Sydney LGAs and regional LGAs 
   
  # Merge
  cases <- cases %>% bind_rows(nswCases)

  return(cases)
  
}
