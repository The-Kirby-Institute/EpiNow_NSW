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
  } else if (option == "postcode") { 
    regionList <- as.character(c(2170, 2161, 2770, 2144, 2200, 2190, 2168, 2160, 2196, 
      2142, 2145, 2148, 2162, 2199, 2760, 2165, 2830, 2761, 2195, 2560, 
      2197, 2198, 2176, 2747, 2166, 2163, 2141, 2767, 2164, 2150, 2171, 2147, 
      2210, 2017, 2565, 2750, 2016, 2035, 2768, 2135, 2207, 2036, 2566, 2570,
      2192, 2177, 2117, 2212, 2765, 2759, 2143, 2261, 2763, 2766, 2567, 2151, 
      2209, 2564, 2140, 2211, 2000))
    regionFolder <- "Postcodes"
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
  
  
