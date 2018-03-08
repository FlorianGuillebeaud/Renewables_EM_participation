## Assignment 2 ## 
## Renewables in electricity market ## 
## Author : Florian Guillebeaud ## 

## Calculate balancing revenues

## RULES 
# If producing more than expected, each extra energy unit is sold at down-regulation price
# If producing less than expected, each missing energy unit is bought at up-regulation price
# else => spot price

####
# contracted : wp contracted for N hours
# measure : wp measure for the N hours contracteded
# spot_price : spot price for the N hours (/hourly)
# reg_up : up regulation price for the N hours (/hourly)
# reg_down : down regulation price for the N hours (/hourly)

balancing_revenues = function(contracted, measure, spot_price, reg_up, reg_down)
{
  # make sure everything is numerical
  contracted = as.numeric(contracted)
  measure = as.numeric(measure)
  spot_price = as.numeric(spot_price)
  reg_up = as.numeric(reg_up)
  reg_down = as.numeric(reg_down)
  
  # initialisation 
  revenues = vector(length = length(contracted))
  hours_helpg_syst = 0 # count
  hours_handicp_syst = 0 # count
  
  for (i in 1:length(contracted)){
    if (contracted[i] < measure[i]){
      # over-production
      hours_helpg_syst = hours_helpg_syst + 1 
      extra_energy = measure[i]-contracted[i]
      revenues[i] = measure[i]*spot_price[i] + extra_energy*reg_down[i] # sold => +
    } else if (contracted[i] > measure[i]){
      # under-production
      hours_handicp_syst = hours_handicp_syst + 1
      missing_energy = contracted[i]-measure[i]
      revenues[i] = measure[i]*spot_price[i] - missing_energy*reg_up[i] # bought => -
    } else {
      hours_helpg_syst = hours_helpg_syst + 1
      well_spotted = contracted[i]
      revenues[i] = contracted[i]*spot_price[i] # contracted = measurements
    }
  } 
  results = list(revenues = revenues, 
                 hours_helpg_syst = hours_helpg_syst,
                 hours_handicp_syst = hours_handicp_syst )
  return(results)
}
