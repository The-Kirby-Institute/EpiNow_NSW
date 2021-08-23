#' Retrieve raw NSW case data
#' 
#' 
LoadData <- function(option, data_file, save_data = FALSE, 
  save_file = "data/") {
  
  if (option == "file") {
    save_data <- FALSE
  }
  
  if (option == "nsw_website") {
    nsw_data_url <- "https://data.nsw.gov.au/data/dataset/97ea2424-abaf-4f3e-a9f2-b5c883f42b6a/resource/2776dbb8-f807-4fb2-b1ed-184a6fc2c8aa/download/confirmed_cases_table4_location_likely_source.csv" 
    cases <- readr::read_csv(nsw_data_url)
    
    save_file <- paste0(save_file, option, "-")
    
  } else if (option == "file") {
    cases <- readr::read_csv(data_file)
    
  } else {
    stop("Unknown raw data option")
  }
  
  if (save_data) {
    write_csv(cases, paste0(save_file, Sys.Date()))
  }
  
  return(cases) 
}
