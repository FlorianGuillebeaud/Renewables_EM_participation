## Assignment 2 ## 
## Renewables in electricity market ## 
## Author : Florian Guillebeaud ## 

## calculate the performance ratio gamma
## gamma = 1 : no deviatios => perfect prediction

source("C_Code/balancing_new.R")

performance_ratio = function(day, contracted, measure, schedule, reg_up, reg_down){
  
  
  temp = balancing(day, contracted, measure, schedule, reg_up, reg_down)
  # actual_revenue = sum(temp$down_regulation_costs + temp$up_regulation_costs , na.rm = TRUE)
  actual_revenue = sum(temp$revenues , na.rm = TRUE)
  ideal_revenue = sum(balancing(day, measure, measure, schedule, reg_up, reg_down)$revenues, na.rm = TRUE)

  return((1-actual_revenue/ideal_revenue)*100)
}
