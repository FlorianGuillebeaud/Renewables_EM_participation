## Assignment 2 ## 
## Renewables in electricity market ## 
## Author : Florian Guillebeaud ## 

## calculate the performance ratio gamma
## gamma = 1 : no deviatios => perfect prediction



performance_ratio = function(contracted, measure, spot_price, reg_up, reg_down){
  
  imbalance_costs = vector()
  
  for (i in 1:length(contracted)){
    if (contracted[i] < measure[i]){
      # over-production
      extra_energy = measure[i]-contracted[i]
      imbalance_costs[i] = extra_energy*reg_down[i]
    } else if (contracted[i] > measure[i]){
      # under-production
      missing_energy = contracted[i]-measure[i]
      imbalance_costs[i] = missing_energy*reg_up[i]
    } else {
      imbalance_costs[i] = 0
    }
  } 
  total_imbalance_costs = sum(imbalance_costs)
  revenues_PTU = sum(measure*spot_price)
  
  return((1-total_imbalance_costs/revenues_PTU)*100)
}