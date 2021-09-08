## This script generates the delays distribution using the international 
## linelist from NCoVUtils or from a local list of case data. 
## 
## This script can take a long time to run to produce the delay_defs

# Restart R and set working directory to source file location

# Packages ----------------------------------------------------------------

require(data.table, quietly = TRUE)
require(tidyverse, quietly = TRUE)
require(EpiNow2, quietly = TRUE)
# require(lubridate, quietly = TRUE)

source("code/PlotOPtions.R")

# Options -----------------------------------------------------------------
delayData <- "local" # "international" or "local"

# Local file to load if dataDelay = "local"
localFile <- "data/delay_count_onsetdate.csv"

saveFig <- TRUE

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
  delay_defs <- estimate_delay(delays, samples = 1000, bootstraps = 100)
  
  saveRDS(delay_defs, "data/international_delays.rds")
  
  if (saveFig) {
    # Plot a figure of the delay distribution 
    delayPlot <- ggplot() +
      geom_histogram(aes(x = delays), binwidth = 1, alpha = 0.6) +
      xlab("Reporting Delay") + ylab("Number of cases") + 
      geom_vline(xintercept = 0, linetype = "dashed") +
      PlotOptions()
    
    ggsave("data/international_delay_histogram.png" , plot = delayPlot)
  }
  
} else if (delayData == "local") {
  
  if (is.null(localFile)) {
    stop("Local linelist not specified") 
  }
  
  data <- read_csv(localFile)
  #Get the delay distribution for all regions:
  print("Generating local delay definitions ...")
  delay_defs <- estimate_delay(data$delay, max_value = 21, bootstraps = 1)
  
  saveRDS(delay_defs, "data/local_delays.rds")
  
  if (saveFig) {
    # Plot a figure of the delay distribution 
    delayPlot <- ggplot(data = data, aes( x = delay)) +
      geom_histogram(binwidth = 1, alpha = 0.6) +
      xlab("Reporting Delay") + ylab("Number of cases") + 
      geom_vline(xintercept = 0, linetype = "dashed") +
      PlotOptions()
    
    ggsave("data/local_delay_histogram.png" , plot = delayPlot)
  }
  
} else {
  stop("Unknown delay data option")
}


