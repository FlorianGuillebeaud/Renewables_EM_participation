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

#### 
source("C_Code/get_schedule.R")

balancing_sc2 = function(contracted, measure, schedule, reg_up)
{
  # make sure everything is numerical
  contracted = as.numeric(contracted)
  measure = as.numeric(measure)
  reg_up = as.numeric(reg_up)
  
  
  # initialisation 
  revenues = vector(length = length(contracted))
  hours_helpg_syst = 0 # count
  hours_handicp_syst = 0 # count
  surplus = shortage = 0 # count
  down_regulation_costs = up_regulation_costs = 0 # count
  
  for (i in 1:length(contracted)){
    
    if (is.na(schedule[i])== FALSE){ #we are scheduled !
      if (contracted[i] > measure[i]){
        # under-production
        hours_handicp_syst = hours_handicp_syst + 1
        missing_energy = contracted[i]-measure[i]
        # the revenues can still be negative (if null prices and under_production)
        revenues[i] = measure[i]*schedule[i] - missing_energy*reg_up[i] # bought => -
        
        up_regulation_costs = up_regulation_costs + missing_energy*reg_up[i]
        shortage = shortage + missing_energy
      }else {
        # capacity_bidded is set : I earn what I bidded (but maybe I could have bidded higher ! )
        hours_helpg_syst = hours_helpg_syst + 1
        well_spotted = contracted[i]
        revenues[i] = contracted[i]*schedule[i] # contracted = measurements
      }
    }else{ # we are not scheduled
      revenues[i] = NA 
    }
  } 
  results = list(revenues = revenues, 
                 shortage = shortage, 
                 up_regulation_costs = up_regulation_costs,
                 hours_helpg_syst = hours_helpg_syst,
                 hours_handicp_syst = hours_handicp_syst )
  return(results)
}
