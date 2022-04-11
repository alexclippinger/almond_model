
yield_model <- function(clim, params) {
  
  # Create data frame with min feb temp for each wy
  min_temp_feb <- clim %>% 
    filter(month == 2) %>% 
    group_by(wy) %>% 
    summarize(min_temp_feb = min(tmin_c)) %>% 
    ungroup() 
  
  # Create data frame with sum of jan precip for each wy
  precip_jan <- clim %>% 
    filter(month == 1) %>% 
    group_by(wy) %>% 
    summarize(precip_jan = sum(precip)) %>% 
    ungroup()
  
  # yield equation
  yield_eq = -0.015*(min_T_feb) - 0.0046*(min_T_feb)^2 - 0.07*(precip_jan) + 0.0043*(precip_jan)^2 + 0.28
  
  return(yield)
}