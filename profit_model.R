profit_model <- function(yield_results, 
                         price = 10  #, year
                         ) {
  
  profit_results <- yield_results %>% 
    #filter(year == year) %>% 
    mutate(profit = almond_yield*price)
  
  return(profit_results)
}