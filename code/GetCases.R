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
    
    if ("likely_source_of_infection" %in% names(raw_data)) {
      cases <- raw_data %>% 
        select(date = notification_date, 
          source = likely_source_of_infection, 
          region = lga_name19,
          postcode) %>%
        mutate(import_status = ifelse(source == "Overseas", "imported", 
          "local")) %>%
        filter(import_status == "local", date >= start_date) %>%
        select(-source, -import_status) 
    } else {
      cases <- raw_data %>% 
        select(date = notification_date, 
          region = lga_name19,
          postcode) %>%
        filter(date >= start_date) 
    }

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
  casesLga <- cases %>%
    group_by(date, region) %>%
    summarise(confirm = n()) %>%
    ungroup() %>%
    select(date, confirm, region) %>%
    arrange(date) %>%
    arrange(region)
  
  # Separately aggregate cases by postcode
  casesPostcode <- cases %>%
    group_by(date, postcode) %>%
    summarise(confirm = n()) %>%
    ungroup() %>%
    select(date, confirm, region = postcode) %>%
    mutate(region = as.character(region)) %>%
    arrange(date) %>%
    arrange(region)
  
  # Aggregate cases to get overall NSW cases
  nswCases <- casesLga %>%
    group_by(date) %>%
    summarise(confirm = sum(confirm)) %>%
    ungroup() %>%
    mutate(region = "NSW") %>%
    arrange(date)
  
  # Aggregate cases to get overall in Greater Sydney
  greaterSydLGAs <- c("Bayside (A)", "Blacktown (C)", "Blue Mountains (C)", 
    "Burwood (A)", "Camden (A)", "Campbelltown (C) (NSW)", "Canada Bay (A)", 
    "Canterbury-Bankstown (A)", "Central Coast (C) (NSW)", 
    "Cumberland (A)", "Fairfield (C)", "Georges River (A)", "Hawkesbury (C)", 
    "Hornsby (A)", "Hunters Hill (A)", "Inner West (A)", "Ku-ring-gai (A)", 
    "Lane Cove (A)", "Liverpool (C)", "Mosman (A)", "North Sydney (A)", 
    "Northern Beaches (A)", "Parramatta (C)", "Penrith (C)", "Randwick (C)", 
    "Ryde (C)", "Strathfield (A)", "Sutherland Shire (A)", "Sydney (C)", 
    "The Hills Shire (A)", "Waverley (A)", "Willoughby (C)", "Wollondilly (A)", 
    "Wollongong (C)", "Woollahra (A)")
  
  sydneyCases <- casesLga %>%
    filter(region %in% greaterSydLGAs) %>%
    group_by(date) %>%
    summarise(confirm = sum(confirm)) %>%
    ungroup() %>%
    mutate(region = "Greater Sydney") %>%
    arrange(date)
  
  # Aggregate cases to get overall in regional LGAs
  regionalCases <- casesLga %>%
    filter(!(region %in% greaterSydLGAs)) %>%
    group_by(date) %>%
    summarise(confirm = sum(confirm)) %>%
    ungroup() %>%
    mutate(region = "All Regional NSW") %>%
    arrange(date)
  
  # Sub-regional areas
  regionalLGAs <- read_csv("data/list_regionalLHDs_LGA.csv",
                           col_names = c('X', 'grouplhd', 'LHD_NAME', 'LGA_NAME_2020'),
                           col_types = c('i', "c", "c", "c"),
                           skip = 1) %>%
    select(group = grouplhd, lhd = LHD_NAME, lga = LGA_NAME_2020)
  
  northernCases <- casesLga %>%
    filter(region %in% filter(regionalLGAs, group == "north_region_lhd")$lga) %>%
    group_by(date) %>%
    summarise(confirm = sum(confirm)) %>%
    ungroup() %>%
    mutate(region = "Northern NSW") %>%
    arrange(date)
  
  westernCases <- casesLga %>%
    filter(region %in% filter(regionalLGAs, group == "west_region_lhd")$lga) %>%
    group_by(date) %>%
    summarise(confirm = sum(confirm)) %>%
    ungroup() %>%
    mutate(region = "Western NSW") %>%
    arrange(date)
  
  
  southernCases <- casesLga %>%
    filter(region %in% filter(regionalLGAs, group == "south_region_lhd")$lga) %>%
    group_by(date) %>%
    summarise(confirm = sum(confirm)) %>%
    ungroup() %>%
    mutate(region = "Southern NSW") %>%
    arrange(date)
  
  # Merge LGA and region level cases and return with postcode in a list
  cases <- list()
  cases[[1]] <- casesLga %>% 
    bind_rows(nswCases) %>%
    bind_rows(sydneyCases) %>%
    bind_rows(regionalCases) %>%
    bind_rows(northernCases) %>%
    bind_rows(westernCases) %>%
    bind_rows(southernCases)
  
  cases[[2]] <- casesPostcode
  
  return(cases)
  
}
