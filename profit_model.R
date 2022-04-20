#' profit_model, Model profit based on yield and market price
#'
#' @param yield_results the yield anomaly dataframe from yield_model results
#' @param yield_coeff linear coefficient of yield vs price anomaly regression (Lobell 2006, Table 5)
#' @param intercept intercept of yield vs price anomaly regression (Lobell 2006, Table 5)
#' @param average_yield_per_acre average yield per acre in lbs
#' @param cost_per_acre average operational costs per acre
#' @param price_per_lb average price of almonds per lb
#'
#' @return a dataframe appended with the profit results ($/acre) for a given yield anomaly
#'
#' source: relationship of yield anomaly to price anomaly derived from Lobell et. al. 2006, Table 5
profit_model <- function(
                  yield_results,
                  yield_coeff = -0.71,
                  intercept = -1.04,
                  average_yield_per_acre = 2200,
                  cost_per_acre = 3897,
                  price_per_lb = 2.50
                ) {
  profit_results <- yield_results %>%
    mutate(
      # Calculate yield anomaly % based on average yield number
      yield_anomaly_pct = (almond_yield/average_yield_per_acre)*100,
      # Calculate price anomaly % based on yield anomaly %
      price_anomaly_pct = (yield_coeff * yield_anomaly_pct) + intercept,
      # Calculate adjusted price based on price per lb
      adjusted_price = price_per_lb * (1 + price_anomaly_pct/100),
      # Calculate profit based on adjusted price, yield, and cost per acre
      profit = ((average_yield_per_acre+almond_yield) * adjusted_price) - cost_per_acre
    ) %>% select(year, almond_yield, profit)

  return(profit_results)
}
