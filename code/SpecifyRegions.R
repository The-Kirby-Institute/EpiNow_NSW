#' Select specific regions for EpiNow estimates and corresponding results
#' folder.
#' 
SpecifyRegions <- function(level, option = "all") {
  
  if (level %in% c("NSW", "Sydney")) {
    option == "all"
  }
  
  if (option == "all") {
    regionList <- "all"
    regionFolder <- level
  } else if (option == "KeyLGAs") {
    regionList <- c("Campbelltown (C) (NSW)", "Blacktown (C)",
      "Fairfield (C)", "Canterbury-Bankstown (A)", "Liverpool (C)",
      "Georges River (A)", "Parramatta (C)", "Cumberland (A)",
      "Penrith (C)")
    regionFolder <- "LGAs-concern"
  } else if (option == "TestLGAs") {
    regionList <- c("Campbelltown (C) (NSW)", "Fairfield (C)")
    regionFolder <- "Test-LGAs"
  } else {
   stop("Uknown regions option") 
  }
  
  return(list(regionList, regionFolder))
}
  
  
