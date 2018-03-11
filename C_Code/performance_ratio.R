## Assignment 2 ## 
## Renewables in electricity market ## 
## Author : Florian Guillebeaud ## 

## calculate the performance ratio gamma
## gamma = 1 : no deviatios => perfect prediction

source("C_Code/balancing_sc2.R")
source("C_Code/balancing.R")

performance_ratio = function(contracted, measure, schedule, reg_up, reg_down){
  
  # if scenario 2              
  # temp = balancing_revenues_sc2(contracted, measure, schedule, reg_up)
  # actual_revenue = sum(temp$revenues , na.rm = TRUE)
  # ideal_revenue = sum(balancing_revenues_sc2(measure, measure, schedule, reg_up)$revenues, na.rm = TRUE)

    # else 
  temp = balancing(contracted, measure, schedule, reg_up, reg_down)
  actual_revenue = sum(temp$down_regulation_costs + temp$up_regulation_costs , na.rm = TRUE)
  ideal_revenue = sum(balancing(measure, measure, schedule, reg_up, reg_down)$revenues, na.rm = TRUE)

  return((1-actual_revenue/ideal_revenue)*100)
  # imbalance_costs = vector()
  # 
  # for (i in 1:length(contracted)){
  #   if (contracted[i] < measure[i]){
  #     # over-production
  #     extra_energy = measure[i]-contracted[i]
  #     imbalance_costs[i] = extra_energy*reg_down[i]
  #   } else if (contracted[i] > measure[i]){
  #     # under-production
  #     missing_energy = contracted[i]-measure[i]
  #     imbalance_costs[i] = missing_energy*reg_up[i]
  #   } else {
  #     imbalance_costs[i] = 0
  #   }
  # }
  # total_imbalance_costs = sum(imbalance_costs)
  # ideal_revenue = sum(balancing_revenues(measure, measure, spot_price, reg_up, reg_down)$revenues, na.rm = TRUE)
  # 
  # results = list(gamma = (1-total_imbalance_costs/ideal_revenue)*100,
  #                total_imbalance_costs = total_imbalance_costs,
  #                revenues_PTU = sum(measure*spot_price))
  # 
  # return(results)
}
