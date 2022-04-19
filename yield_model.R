library(roxygen2)

#' yield_model, Model crop yields over time
#'
#' @param clim The data frame of pre-formatted climate data
#' @param year_start The year to start modeling on, defaults to first year of data
#' @param year_end The year to end modeling on, defaults to last year of data
#'
#' @return A data frame of modeled yields for each year
yield_model <- function(clim, 
                        temp_param = -0.015, 
                        precip_param = -0.07,
                        year_start = min(clim$year), 
                        year_end = max(clim$year)) {
  
  # Build return data frame
  columns <- c("year", "almond_yield")
  yield_results <- data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(yield_results) <- columns

  # Build time span of data from input params
  span = seq(year_start, year_end, 1)

  # Iterate over each year of desired span
  for (yr in span) {
    
    # Get average minimum temperature from Feb of current year
    min_T_feb = clim %>% 
      filter(year == yr, month == 2) %>% 
      summarize(min_T_feb = mean(tmin_c)) %>% 
      pull()
    
    # Get total precipitation from Jan of current year
    precip_jan = clim %>% 
      filter(year == yr, month == 1) %>% 
      summarize(precip_jan = sum(precip)) %>% 
      pull()
    
    # Get almond yield for year
    almond_yield = temp_param*(min_T_feb) - 0.0046*((min_T_feb)^2) + precip_param*(precip_jan) + 0.0043*((precip_jan)^2) + 0.28
  
    # Add results to return dataframe
    yield_results <- yield_results %>% add_row(
      year = yr,
      almond_yield = almond_yield
    )
  }

  yield_results
}

