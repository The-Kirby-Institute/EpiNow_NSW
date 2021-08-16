## This script generates the delays distribution using the international 
## linelist from NCoVUtils or from a local list of case data. 
## 
## This script can take a long time to run to produce the delay_defs

# Restart R and set working directory to source file location

# Packages ----------------------------------------------------------------

require(data.table, quietly = TRUE)
require(EpiNow2, quietly = TRUE)
require(lubridate, quietly = TRUE)

# Options -----------------------------------------------------------------
delayData <- "local" # "international" or "local"

# Local file to load if dataDelay = "local"
localFile <- "data/delay_count_onsetdate.csv"

# Get linelist ------------------------------------------------------------

if (delayData == "international") {
  linelist <- data.table::fread("https://raw.githubusercontent.com/epiforecasts/NCoVUtils/master/data-raw/linelist.csv")
  
  print("Processing international linelist ...")
  delays <- linelist[!is.na(date_onset_symptoms)][, 
    .(report_delay = as.numeric(lubridate::dmy(date_confirmation) - 
        as.Date(lubridate::dmy(date_onset_symptoms))))]
  
  delays <- delays$report_delay
  
  # Set up cores -----------------------------------------------------
  if (!interactive()){
    options(future.fork.enable = TRUE)
  }
  
  future::plan("multiprocess", workers = round(future::availableCores()))
  
  # Fit the reporting delay -------------------------------------------------
  print("Generating international delay definitions ...")
  delay_defs <- estimate_delay(delays, bootstraps = 100, samples = 1000)
  
  saveRDS(delay_defs, "data/international_delays.rds")
  
} else if (delayData == "local") {
  
  if (is.null(localFile)) {
    stop("Local linelist not specified") 
  }
  
  data <- read_csv(localFile)
  #Get the delay distribution for all regions:
  print("Generating loacl delay definitions ...")
  delay_defs <- estimate_delay(data$delay, bootstraps = 100, 
    samples = 1000)
  
  saveRDS(delay_defs, "data/local_delays.rds")
  
} else {
  stop("Unknown delay data option")
}
