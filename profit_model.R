profit_model <- function(yield_results, 
                         price = 10  #, year
                         ) {
  
  profit_results <- yield_results %>% 
    #filter(year == year) %>% 
    mutate(price_anomaly = -0.71*almond_yield - 1.04,
           profit = almond_yield*price - almond_yield*price_anomaly) %>% 
    select(-price_anomaly)
  
  return(profit_results)
}