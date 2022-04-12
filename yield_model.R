library(roxygen2)

#' yield_model, Model crop yields over time
#'
#' @param clim The data frame of pre-formatted climate data
#' @param year_start The year to start modeling on, defaults to first year of data
#' @param year_end The year to end modeling on, defaults to last year of data
#'
#' @return A data frame of modeled yields for each year
yield_model <- function(clim, year_start = min(clim$year), year_end = max(clim$year)) {
  # Build return data frame
  columns <- c("year", "almond_yield")
  yield_results <- data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(yield_results) <- columns

  # Build time span of data from input params
  span = seq(year_start, year_end, 1)

  # Iterate over each year of desired span
  for (year in span) {
    # Get almond yield for year
    almond_yield <- almond_yield_for_year(clim, year)

    # Add results to return dataframe
    yield_results <- yield_results %>% add_row(
      year = year,
      almond_yield = almond_yield
    )
  }

  yield_results
}

#' almond_yield_for_year, Model almond yield for given year
#'
#' @param clim The data frame of pre-formatted climate data
#' @param yr The year of data to calculate yield for
#'
#' @return The calculated yield of almonds
almond_yield_for_year <- function(clim, yr) {
  # Filter data to year
  year_data <- clim %>% filter(year == yr)

  # Return NA for year with no data
  if (nrow(year_data) == 0) {
    return(NA)
  }

  # Average minimum temps from Feb of current year
  min_T_feb <- mean((year_data %>% filter(month == 2) %>% select(tmin_c))$tmin_c)

  # Total precipitation from Jan of current year
  precip_jan <- sum(year_data %>% filter(month == 1) %>% select(precip))

  # Almond yield equation, Lobell et. al. 2006
  yield = -0.015*(min_T_feb) - 0.0046*((min_T_feb)^2) - 0.07*(precip_jan) + 0.0043*((precip_jan)^2) + 0.28
  yield  
}
