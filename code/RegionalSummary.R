#' Function to produce regional summary results
#' 
RegionalSummary <- function(output, folder, summary_folder, date) {
    
    results_dir = file.path("results", folder)
    target_date = "latest"
    summary_dir =  file.path(results_dir, summary_folder, date)
    
    regional_summary_NSW(
      regional_output = output$regional,
      reported_cases = output$summary$reported_cases,
      summary_dir = summary_dir,
      target_date = target_date)
    
    copy_results_to_latest(summary_dir, 
      file.path(results_dir, summary_folder, "latest"))
    
  }
