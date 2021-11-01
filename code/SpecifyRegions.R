#' Select specific regions for EpiNow estimates and corresponding results
#' folder.
#' 
SpecifyRegions <- function(option = "all") {
  
  if (option == "all") {
    regionList <- "all"
    regionFolder <- "All"
  } else if (option == "metro_lgas") {
    regionList <- c("Cumberland (A)","Canterbury-Bankstown (A)","Blacktown (C)","Fairfield (C)",
      "Liverpool (C)","Penrith (C)", "Parramatta (C)","Campbelltown (C) (NSW)",
      "Georges River (A)", "Bayside (A)","Strathfield (A)", "Burwood (A)",
      "Blue Mountains (C)", "Camden (A)","Canada Bay (A)","Central Coast (C) (NSW)",
      "Hawkesbury (C)", "Hornsby (A)", "Hunters Hill (A)", "Inner West (A)",
      "Ku-ring-gai (A)", "Lane Cove (A)", "Mosman (A)", "North Sydney (A)",
      "Northern Beaches (A)", "Randwick (C)", "Ryde (C)", "Sutherland Shire (A)",
      "Sydney (C)", "Waverley (A)", "Willoughby (C)", "Wollondilly (A)",
      "Wollongong (C)","Woollahra (A)",
      "Shellharbour (C)","Shoalhaven (C)", "Kiama (A)", "Lithgow (C)")
    regionFolder <- "Metro_LGAs"
  } else if (option == "syd_lgas_concern") {
    # Includes Blue Mountains, Central Coast, Shellharbour
    regionList <- c("Cumberland (A)","Canterbury-Bankstown (A)","Blacktown (C)","Fairfield (C)",
      "Liverpool (C)","Penrith (C)", "Parramatta (C)","Campbelltown (C) (NSW)",
      "Georges River (A)", "Bayside (A)","Strathfield (A)", "Burwood (A)")
    regionFolder <- "Sydney_LGAs_Concern"
  } else if (option == "Sydney-Other-LGAs") {
    regionList <- c("Armidale Regional (A)", "Bathurst Regional (A)"   )
    regionFolder <- "Regions"
  } else if (option == "postcode") { 
    regionList <- "postcodes" # Read in from postcode_suburb_name.csv
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
