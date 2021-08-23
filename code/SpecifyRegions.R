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
  
  
