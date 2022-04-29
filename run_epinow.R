setwd("D:/UNSW/Team NSW Health COVID-19 Data - General/Delta Outbreak/Code/EpiNow_NSW")
# Now casting script for running EpiNow2 on NSW COVID case data
# =============================================================

## SETUP
# Open as a project (setting working directory to source and restarting R)

# Packages
suppressMessages(require(EpiNow2, quietly = TRUE))
suppressMessages(require(tidyverse, quietly = TRUE))
suppressMessages(require(lubridate, quietly = TRUE))
suppressMessages(require(future, quietly = TRUE))
suppressMessages(require(crayon, quietly = TRUE))
options(dplyr.summarise.inform=FALSE)

# Functions 
source("code/LoadData.R")
source("code/GetCases.R")
source("code/SpecifyRegions.R")
source("code/RegionalSummary.R")
source("code/regional_summary_NSW.R")
source("code/plot_summary_NSW.R")
source("code/RegionsCumulativeReports.R")
source("code/Functions.R")




## USER INPUTS
# Setup options for running EpiNow2


# Set the number of cores for running
n_cores = 25


# Set start and end dates
dateRun <- NULL                     # Run to this date: NULL = last date in data file
startDate <- today() + weeks(-8)    # Filter to after this date (speeds things up) 


# Specify where the case data is coming from see LoadData() function for details. 
dataOption <- "nsw_website"         # "nsw_website" or "file"
delayOption <- "nsw_data"           # nsw_data (default), "testing", "international" (what does this do?)


# Specify which regions will be inputed into EpiNow and where the results 
# will be stored in the results/ folder. These can be entered manually
# or via the use of the SpecifyRegions() function. 


# Sets the input and output directories
postcodeRun <- F
if (postcodeRun) {
  
  # If running on the postcode level
  specificRegions <- SpecifyRegions("postcode")[[1]]
  resultsFolder <- SpecifyRegions("postcode")[[2]]
  
} else {
  
  # If running on the LGA level
  specificRegions <- SpecifyRegions("all")[[1]]
  # specificRegions <- c("NSW", "Greater Sydney", "Northern NSW", "Western NSW", "Southern NSW")
  # specificRegions <- c("Cessnock (C)", "Lake Macquarie (C)", "Maitland (C)", 
  #                      "Newcastle (C)", "Port Stephens (A)", "Singleton (A)")
  
  resultsFolder <- SpecifyRegions("all")[[2]]
  # resultsFolder <- "Main_regions"
}


# Produce summary comparison results for regions, LGAs, and postcodes
summariseResults <- TRUE




##  Sort out case data}
# Gets the data ready for EpiNow2


# Load raw case data 
nsw_raw_cases <- LoadData(dataOption)


# Save website data for later
if (dataOption == "nsw_website") {
  write_csv(nsw_raw_cases, 
            file = file.path("data", 
            paste0("NSW_Health_Website_Data-", Sys.Date())))
}


# Get data into the right shape for analysis


# Load postcode and locality file
casesAll <- GetCases(nsw_raw_cases, dataOption, startDate)
if (postcodeRun) {
  
  # If running at postcode level
  cases <- casesAll[[2]]
  dateCases <- max(cases$date) # Need to check to make sure postcode file name correct
  postcodes <- 
    read_csv(paste0("data/postcode_suburb_name-", dateCases, ".csv"), show_col_types = FALSE) %>%
    # select(postcode = Postcode, name = ...4) %>% 
    mutate(region = as.character(postcode), 
           name = paste0(as.character(postcode), "-", str_replace(name, "/","_")))
  specificRegions <- postcodes$postcode
  
} else {
  
  # If running at LGA level
  cases <- casesAll[[1]]
  dateCases <- max(cases$date)
}   


# Filter out dates from the end of the data
if(!is.null(dateRun)) {
  cases <- filter(cases, date <= dateRun)
}


# Filter out regions we are not interested in
if (specificRegions[1] != "all") {
  cases <- filter(cases, region %in% specificRegions)
}


# Convert postcode numbers to names
if (postcodeRun) {
  cases <- 
    cases %>%
    left_join(postcodes, by = "region") %>%
    select(date, confirm, region = name)
}




##  Set-up outputs
# Sets up output directories for EpiNow2


# Sets up aggregate regions
aggregateRegions <- c("NSW", "Greater Sydney", "Northern NSW", "Western NSW", "Southern NSW", "All Regional NSW")


# Separate out LGA and aggregate regions if not postcode run
if (!postcodeRun) {
  regionCases <- filter(cases, region %in% aggregateRegions)
  lgaCases <- filter(cases, !(region %in% aggregateRegions))
  
  multiRegions <- unique(regionCases$region)
  multiRegionRun <- ifelse(length(multiRegions) > 1, TRUE, FALSE)
  
  multiLGAs <- unique(lgaCases$region)
  multiLGArun <- ifelse(length(multiLGAs) > 1, TRUE, FALSE)
} else {
  postcodeCases <- cases
}


# Make results folder
dir.create(file.path("results", resultsFolder), showWarnings = FALSE)




##  Load EpiNow delay parameters and rt prior
# Sets up priors for epi-parameters


if (delayOption == "nsw_data") {
  
  # Load delay_defs fitted from local NSW data. This needs to be generated
  # separately using the EpiNow_update_delay.R script.
  reporting_delay <- readRDS("data/local_delays.rds")
  
  
} else if (delayOption == "international") {
  
  # Out of date delay estimate from international sources
  reporting_delay <- readRDS("data/international_delays.rds")
  
  
} else if (delayOption == "testing") {
  
  # Basic set-up for testing purposes
  reporting_delay <- 
    list(mean = EpiNow2::convert_to_logmean(4, 1),
         mean_sd = 0.1, 
         sd = EpiNow2::convert_to_logsd(4, 1),
         sd_sd = 0.1, max = 15)
  
  
} else {
  stop("Unknown reporting delay option")
}


# Other delays from the literature
generation_time <- EpiNow2::get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <- EpiNow2::get_incubation_period(disease = "SARS-CoV-2", source = "lauer")


# Specify prior for RT. Currently assumed to be low given the presence of
# restrictions with some LGAs having no cases. We use this assumed prior 
# to prevent EpiNow forecasting massive increases in cases when numbers
# are small.  
rtPrior <- rt_opts(prior = list(mean = 1, sd = 0.5))






##  Set-up simulations
# Use future package to run things in parallel
options(future.fork.enable = TRUE)
future::plan("multisession", workers = n_cores)
# ncores <- round(future::availableCores() / 2)




##  Run EpiNow
# Run EpiNow2


# Initialize outputs for aggregate regions, LGAs, and postcodes
regionEstimates <- NULL
lgaEstimates <- NULL
postcodeEstimates <- NULL


# Run EpiNow2
if (!postcodeRun) {
  
  # Run EpiNow for aggregate regions
  if (multiRegionRun) {
    
    # More than one aggregate region so run regional version of EpiNow
    print("Generating multiple aggregate region estimates...")
    regionEstimates <- EpiNow2::regional_epinow(reported_cases = regionCases, 
      generation_time = generation_time,
      delays = delay_opts(incubation_period, reporting_delay),
      rt = rtPrior,
      stan = stan_opts(),
      target_folder = file.path("results", resultsFolder),
      logs = file.path("logs", Sys.Date()),
      return_output = TRUE, 
      verbose = TRUE)
    
    # Produce aggregate region summary results.
    # Stored in ./results/resultsFolder/Regions-summary/ 
    if (summariseResults) {
      print("Producing aggregate region summary ...")
      RegionalSummary(regionEstimates, resultsFolder, "Regions-summary", dateCases) 
    }
    
    # Run function to save CSV file of key indicators for each region
    for (region in names(regionEstimates$regional)) {
      keyEstimates <- GetKeyEstimates(regionEstimates, multiRegionRun, region)
      SaveAllIndicators(keyEstimates, 
        file.path("results", file.path(resultsFolder, region)),
        dateCases)
      SaveAllIndicators(keyEstimates, 
        file.path("results", file.path(resultsFolder, region)), 
        "latest")
    }
    
  } else {
    
    # 0 or 1 regions in multiRegions
    if (nrow(regionCases) != 0) {
      
      # Single region so run normal EpiNow
      print("Generating Single LGA estimates...")
      regionEstimates <- EpiNow2::epinow(
        # Need to remove region column from cases for this to run
        reported_cases = dplyr::select(regionCases, -region),
        generation_time = generation_time,
        delays = delay_opts(incubation_period, reporting_delay),
        rt = rtPrior,
        stan = stan_opts(),
        target_folder = file.path("results", resultsFolder,
          lgaRegions),
        logs = file.path("logs", Sys.Date()),
        return_output = TRUE, 
        verbose = TRUE)
      
      # Run function to save CSV file of key indicators 
      keyEstimates <- GetKeyEstimates(regionEstimates, FALSE)
      SaveAllIndicators(keyEstimates, 
        file.path("results", resultsFolder, multiRegions),
        dateCases)
      SaveAllIndicators(keyEstimates, 
        file.path("results", resultsFolder, multiRegions), 
        "latest")
    }
  }
  
  
  # Run EpiNow for LGAs
  if (multiLGArun) {
    
    # More than one LGA so run regional version of EpiNow
    # print("Generating Multiple LGA estimates...")
    lgaEstimates <- EpiNow2::regional_epinow(reported_cases = lgaCases, 
      generation_time = generation_time,
      delays = delay_opts(incubation_period, reporting_delay),
      rt = rtPrior,
      stan = stan_opts(),
      target_folder = file.path("results", resultsFolder),
      logs = file.path("logs", Sys.Date()),
      return_output = TRUE, 
      verbose = TRUE)
    
    # Produce LGA summary results.
    # Stored in ./results/resultsFolder/LGA-summary/ 
    if (summariseResults) {
      print("Producing LGA summary ...")
      RegionalSummary(lgaEstimates, resultsFolder, "LGA-summary", dateCases) 
    }
    
    # Run function to save CSV file of key indicators for each region
    for (region in names(lgaEstimates$regional)) {
      keyEstimates <- GetKeyEstimates(lgaEstimates, multiLGArun, region)
      SaveAllIndicators(keyEstimates, 
        file.path("results", file.path(resultsFolder, region)),
        dateCases)
      SaveAllIndicators(keyEstimates, 
        file.path("results", file.path(resultsFolder, region)), 
        "latest")
    }
    
    
  } else {
    
    # 0 or 1 LGAs in lgaRegions
    if (nrow(lgaCases) != 0) {
      
      # Single LGA so run normal EpiNow
      print("Generating Single LGA estimates...")
      lgaEstimates <- EpiNow2::epinow(
        # Need to remove region column from cases for this to run
        reported_cases = dplyr::select(lgaCases, -region),
        generation_time = generation_time,
        delays = delay_opts(incubation_period, reporting_delay),
        rt = rtPrior,
        stan = stan_opts(),
        target_folder = file.path("results", resultsFolder,
          lgaRegions),
        logs = file.path("logs", Sys.Date()),
        return_output = TRUE, 
        verbose = TRUE)
      
      # Run function to save CSV file of key indicators 
      keyEstimates <- GetKeyEstimates(lgaEstimates, FALSE)
      SaveAllIndicators(keyEstimates, 
        file.path("results", resultsFolder, lgaRegions),
        dateCases)
      SaveAllIndicators(keyEstimates, 
        file.path("results", resultsFolder, lgaRegions), 
        "latest")
    }
  }
  
} else {
   print("Generating Postcode estimates...")
    postcodeEstimates <- EpiNow2::regional_epinow(reported_cases = postcodeCases, 
      generation_time = generation_time,
      delays = delay_opts(incubation_period, reporting_delay),
      rt = rtPrior,
      stan = stan_opts(),
      target_folder = file.path("results", resultsFolder),
      logs = file.path("logs", Sys.Date()),
      return_output = TRUE, 
      verbose = TRUE)
    
    # Produce LGA summary results.
    # Stored in ./results/resultsFolder/LGA-summary/ 
    if (summariseResults) {
      
      print("Producing postcodes summary ...")
      RegionalSummary(postcodeEstimates, resultsFolder, "Postcode-summary", dateCases) 
    }
    
    # Run function to save CSV file of key indicators for each region
    for (region in names(postcodeEstimates$regional)) {
      keyEstimates <- GetKeyEstimates(postcodeEstimates, TRUE, region)
      
      SaveAllIndicators(keyEstimates, 
        file.path("results", file.path(resultsFolder, region)),
        dateCases)
      SaveAllIndicators(keyEstimates, 
        file.path("results", file.path(resultsFolder, region)), 
        "latest")
    }
}

print("Tidying up...")
# Save everything in the results folder just in case
save(startDate, dateRun, specificRegions, dataOption, 
  delayOption, cases, regionEstimates, lgaEstimates, postcodeEstimates, 
  resultsFolder, generation_time, incubation_period, reporting_delay,
  file = file.path("results", resultsFolder, paste0("Epinow_results-", 
    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".rds")))



##  Summarise results}
## Useful Commands for looking at individual region results
# > summary(estimates)
# > plot(estimates)
# > summary(estimates, type = "parameters", params = "R")



##  Garbage collection}
# For the love of god, please stop saving >8GB .RData files!!!
rm(list = ls(all.names = T))
gc()




##  PART 2 RUN REGIONAL SUMMARY
# Script to produce a summary plot for specified regions
# ======================================================

# Results need to have been generated by EpiNow first

##  Set-up

# Packages
require(EpiNow2, quietly = TRUE)
require(tidyverse, quietly = TRUE)
require(lubridate, quietly = TRUE)

options(dplyr.summarise.inform=FALSE)

# Functions 

# source("code/SpecifyRegions.R")
# source("code/RegionalSummary.R")
# source("code/regional_summary_NSW.R")
source("code/plot_summary_NSW.R")
source("code/RegionsCumulativeReports.R")
source("code/Functions.R")




##  User inputs

resultsDir <- file.path("results", "All", "LGA-summary", "latest")



##  Functions
save_ggplot <- function(plot, name, height = 12, width = 12, ...) {
  suppressWarnings(
    suppressMessages(
      ggplot2::ggsave(file.path(resultsDir, name),
                      plot,
                      dpi = 300, width = width,
                      height = height, ...
      )
    )
  )
}


##  Load results

# Load regional summary results results 
summary_data <- data.table::fread(file.path(resultsDir, "summary_data.csv"))
reported_cases <- data.table::fread(file.path(resultsDir, "reported_cases.csv"))

reported_cases <- data.table::setDT(reported_cases) %>%
  mutate(date = as.Date(date))



##  Separate regions
# Filter out regions we want

sydneyLGAs <- c("Bayside (A)", "Blacktown (C)", "Blue Mountains (C)", 
                "Burwood (A)", "Camden (A)", "Campbelltown (C) (NSW)", "Canada Bay (A)", 
                "Canterbury-Bankstown (A)", "Central Coast (C) (NSW)", 
                "Cumberland (A)", "Fairfield (C)", "Georges River (A)", "Hawkesbury (C)", 
                "Hornsby (A)", "Hunters Hill (A)", "Inner West (A)", "Ku-ring-gai (A)", 
                "Lane Cove (A)", "Liverpool (C)", "Mosman (A)", "North Sydney (A)", 
                "Northern Beaches (A)", "Parramatta (C)", "Penrith (C)", "Randwick (C)", 
                "Ryde (C)", "Strathfield (A)", "Sutherland Shire (A)", "Sydney (C)", 
                "The Hills Shire (A)", "Waverley (A)", "Willoughby (C)", "Wollondilly (A)", 
                "Wollongong (C)", "Woollahra (A)")

# Sub-regional areas
regionalLGAs <- read_csv("data/list_regionalLHDs_LGA.csv", 
                         col_types = cols_only(grouplhd = "c", LHD_NAME = "c", LGA_NAME_2020 ="c")) %>%
  rename(group = grouplhd, lhd = LHD_NAME, lga = LGA_NAME_2020)

northernLGAs <- regionalLGAs %>%  filter(group == "north_region_lhd")
westernLGAs <- regionalLGAs %>%  filter(group == "west_region_lhd")
southernLGAs <- regionalLGAs %>%  filter(group == "south_region_lhd")

summary_data_sydney <- filter(summary_data, region %in% sydneyLGAs)
reported_cases_sydney <- filter(reported_cases, region %in% sydneyLGAs)

summary_data_northern <- filter(summary_data, region %in% northernLGAs$lga)
reported_cases_northern <- filter(reported_cases, region %in% northernLGAs$lga)

summary_data_western <- filter(summary_data, region %in% westernLGAs$lga)
reported_cases_western <- filter(reported_cases, region %in% westernLGAs$lga)

summary_data_southern <- filter(summary_data, region %in% southernLGAs$lga)
reported_cases_southern <- filter(reported_cases, region %in% southernLGAs$lga)



##  produce plots}

# Produce plots for each region
max_plot <- 10

# 
max_cases_sydney <- round(max(reported_cases_sydney$confirm, na.rm = TRUE) * 
                            max_plot, 0)
max_cases_northern <- round(max(reported_cases_northern$confirm, na.rm = TRUE) * 
                              max_plot, 0)
max_cases_western <- round(max(reported_cases_western$confirm, na.rm = TRUE) * 
                             max_plot, 0)
max_cases_southern <- round(max(reported_cases_southern$confirm, na.rm = TRUE) * 
                              max_plot, 0)

summary_plot_Sydney <- plot_summary_NSW(summary_data_sydney,
                                        x_lab = "Region", log_cases = FALSE, max_cases = max_cases_sydney,
                                        reported_cases = reported_cases_sydney)

summary_plot_Northern <- plot_summary_NSW(summary_data_northern,
                                          x_lab = "Region", log_cases = FALSE, max_cases = max_cases_northern,
                                          reported_cases = reported_cases_northern)

summary_plot_Western <- plot_summary_NSW(summary_data_western,
                                         x_lab = "Region", log_cases = FALSE, max_cases = max_cases_western,
                                         reported_cases = reported_cases_western)

summary_plot_Southern <- plot_summary_NSW(summary_data_southern,
                                          x_lab = "Region", log_cases = FALSE, max_cases = max_cases_southern,
                                          reported_cases = reported_cases_southern)

save_ggplot(summary_plot_Sydney, "summary_plot_SydneyLGAs.png",
            width = ifelse(length(sydneyLGAs) > 60,
                           ifelse(length(sydneyLGAs) > 120, 36, 24), 12))

# 
save_ggplot(summary_plot_Northern, "summary_plot_NorthernLGAs.png",
            width = ifelse(length(northernLGAs) > 60,
                           ifelse(length(northernLGAs) > 120, 36, 24), 12))

save_ggplot(summary_plot_Western, "summary_plot_WesternLGAs.png",
            width = ifelse(length(westernLGAs) > 60,
                           ifelse(length(westernLGAs) > 120, 36, 24), 12))

save_ggplot(summary_plot_Southern, "summary_plot_SouthernLGAs.png",
            width = ifelse(length(southernLGAs) > 60,
                           ifelse(length(southernLGAs) > 120, 36, 24), 12))
