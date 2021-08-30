#' Select specific regions for EpiNow estimates and corresponding results
#' folder.
#' 
SpecifyRegions <- function(option = "all") {

  if (option == "all") {
    regionList <- "all"
    regionFolder <- "All"
  } else if (option == "KeyLGAs") {
    regionList <- c("Campbelltown (C) (NSW)", "Blacktown (C)",
      "Fairfield (C)", "Canterbury-Bankstown (A)", "Liverpool (C)",
      "Georges River (A)", "Parramatta (C)", "Cumberland (A)",
      "Penrith (C)")
    regionFolder <- "LGAs-concern"
  } else if (option == "Sydney") {
    # Includes Blue Mountains, Central Coast, Shellharbour
    regionList <- c("Bayside (A)", "Blacktown (C)", "Blue Mountains (C)",
      "Burwood (A)", "Cabonne (A)", "Camden (A)", "Campbelltown (C) (NSW)",
      "Canada Bay (A)", "Canterbury-Bankstown (A)", "Central Coast (C) (NSW)",
      "Clarence Valley (A)", "Cumberland (A)", "Fairfield (C)", 
      "Georges River (A)", "Glen Innes Severn (A)", "Goulburn Mulwaree (A)", 
      "Hawkesbury (C)", "Hornsby (A)")
    regionFolder <- "Sydney-metro"
  } else if (option == "Regions") {
    regionList <- c("Armidale Regional (A)", "Bathurst Regional (A)"   )
    regionFolder <- "Regions"
  } else if (option == "TestLGAs") {
    regionList <- c("Campbelltown (C) (NSW)", "Fairfield (C)")
    regionFolder <- "Test-LGAs"
  } else if (option == "manual") {  
    # Do nothing and enter manually
    regionList <- NULL
    regionFolder <- NULL
    print("Enter regions and results folder manually")
  } else {
   stop("Unknown regions option") 
  }
  
  return(list(regionList, regionFolder))
}
  
  
