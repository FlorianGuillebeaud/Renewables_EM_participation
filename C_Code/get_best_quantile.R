## Assignment 2 ## 
## Renewables in electricity market ## 
## Author : Florian Guillebeaud ## 
###################################
###################################
source("C_Code/quantile_distribution.R")

get_best_quantile = function(contracted, wp_next_day){
  quantiles = quantile_distribution(wp_next_day)
  best_quantile = vector()
  for (i in 1:length(contracted)){
    index = max(which((as.numeric(quantiles[i,] - contracted[i]))<=0))
    best_quantile[i] = quantiles[i,index]
  }
  return(best_quantile) 
}
