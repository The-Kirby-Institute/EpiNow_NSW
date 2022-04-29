#' Retrieve raw NSW case data
#' 
#' 
LoadData <- function(option, data_file, save_data = FALSE, save_file = "data/") {
  
  if (option == "file") {
    save_data <- FALSE
  }
  
  if (option == "nsw_website") {
    
    
    # nsw_data_url <- "https://data.nsw.gov.au/data/dataset/97ea2424-abaf-4f3e-a9f2-b5c883f42b6a/resource/2776dbb8-f807-4fb2-b1ed-184a6fc2c8aa/download/confirmed_cases_table4_location_likely_source.csv"
    # nsw_data_url <- "https://data.nsw.gov.au/data/dataset/aefcde60-3b0c-4bc0-9af1-6fe652944ec2/resource/21304414-1ff1-4243-a5d2-f52778048b29/download/confirmed_cases_table1_location.csv"
    nsw_data_url <- "https://data.nsw.gov.au/data/dataset/aefcde60-3b0c-4bc0-9af1-6fe652944ec2/resource/5d63b527-e2b8-4c42-ad6f-677f14433520/download/confirmed_cases_table1_location_agg.csv"
    cases <- readr::read_csv(nsw_data_url, progress = F, col_types = cols())
    
    # De-aggregate the case data
    cases = 
      cases %>%
      group_by(notification_date, postcode, lhd_2010_code, lhd_2010_name, lga_code19, lga_name19) %>%
      summarise(cases = sum(confirmed_cases_count, na.rm = T)) %>%
      ungroup()
    cases = as.data.frame(cases)
    cases = cases[rep(seq(1, nrow(cases)), cases[,7]),-7]
    
    # Save
    save_file <- paste0(save_file, option, "-")
    
  } else if (option == "file") {
    cases <- readr::read_csv(data_file)
    
  } else {
    stop("Unknown raw data option")
  }
  
  if (save_data) {
    write_csv(cases, paste0(save_file, Sys.Date()))
  }
  
  
  # Run a check to see how up to date the file is
  how_old = today() - max(cases$notification_date)
  if ( as.numeric(how_old) > 7 ){
    stop('Dataset is more the 7 days old, check the data source.')
  } else {
    cat(green('Dataset is', as.numeric(how_old), 'days old\n'))
  }
  
  return(cases) 
}
