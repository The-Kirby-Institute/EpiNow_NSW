#' Select specific regions for EpiNow estimates
#' 
#' 
SpecifyRegions <- function(option = "all") {
  
  if (option == "all") {
    regionList <- "all"
  } else if (option == "KeyLGAs") {
    regionList <- c("Campbelltown (C) (NSW)", "Blacktown (C)",
      "Fairfield (C)", "Canterbury-Bankstown (A)", "Liverpool (C)",
      "Georges River (A)", "Parramatta (C)", "Cumberland (A)",
      "Penrith (C)")
  } else {
   stop("Uknown regions option") 
  }
  
  return(regionList)
}
  
  
