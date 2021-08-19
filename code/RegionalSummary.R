#' Function to produce regional summary results
#' 
RegionalSummary <- function(output, folder) {
    
    results_dir = file.path("results", folder)
    target_date = "latest"
    summary_dir =  file.path(results_dir, "regional-summary", "latest")
    
    regional_summary_NSW(
      regional_output = output$regional,
      reported_cases = output$summary$reported_cases,
      summary_dir = summary_dir,
      target_date = target_date)
  }
