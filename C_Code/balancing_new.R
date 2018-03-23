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
# measured : wp measured for the N hours contracteded
# spot_price : spot price for the N hours (/hourly)
# reg_up : up regulation price for the N hours (/hourly)
# reg_down : down regulation price for the N hours (/hourly)

balancing = function(i, contracted, measured, schedule, reg_up, reg_down)
{
  # make sure everything is numerical
  contracted = as.numeric(contracted)
  measured = as.numeric(measured)
  reg_up = as.numeric(reg_up)
  reg_down = as.numeric(reg_down)
  
  # initialisation 
  revenues = ba_revenue = da_revenue = vector(length = length(contracted))
  hours_helpg_syst = 0 # count
  hours_handicp_syst = 0 # count
  well_spotted = 0 # count
  surplus = shortage = 0 # count
  down_regulation_costs = up_regulation_costs = 0 # count
  
  for (k in 1:length(contracted))
  {
    if (is.na(schedule[k])== FALSE)
    { # we are scheduled in the DAY AHEAD => let's see the revenues!
      #cat(paste0("We are contracted : ", k , "/ ", length(contracted)), "\n")
      
      if (is.na(contracted[k])==TRUE  | is.na(measured[k])==TRUE)
      {
        # cat(paste0("missing value at day : ",i, " at the hour : ", k ))
        da_revenue[k] = NA
        ba_revenue[k] = NA
        revenues[k] = NA
      } else
      {
        if (contracted[k] < measured[k])
        {
          # local surplus
          hours_helpg_syst = hours_helpg_syst + 1 
          extra_energy = measured[k]-contracted[k]
          
          da_revenue[k] = contracted[k]*schedule[k]
          ba_revenue[k] = extra_energy*reg_down[k]
          revenues[k] = da_revenue[k] + ba_revenue[k] # sold => +
          
          down_regulation_costs = down_regulation_costs + extra_energy*reg_down[k]
          surplus = surplus + extra_energy
        } else if (contracted[k] > measured[k])
        {
          # local shortage
          hours_handicp_syst = hours_handicp_syst + 1
          missing_energy = contracted[k]-measured[k]
          
          da_revenue[k] = contracted[k]*schedule[k]
          ba_revenue[k] = missing_energy*reg_up[k] # bought => -
          revenues[k] = da_revenue[k] - ba_revenue[k]
          
          up_regulation_costs = up_regulation_costs + missing_energy*reg_up[k]
          shortage = shortage + missing_energy
        } else 
        {
          # balance
          hours_helpg_syst = hours_helpg_syst + 1 
          da_revenue[k] = contracted[k]*schedule[k]
          ba_revenue[k] = 0
          revenues[k] = da_revenue[k] + ba_revenue[k] 
        }
      }
    }else 
    { # We are NOT scheduled in the day-ahead but we produce and intervene in the balancing market
      da_revenue[k] = 0
      
      if (is.na(contracted[k])==TRUE  | is.na(measured[k])==TRUE)
      {
        # cat(paste0("missing value at day : ",i, " at the hour : ", k ))
        da_revenue[k] = NA
        ba_revenue[k] = NA
        revenues[k] = NA
      } else 
      {
        if (contracted[k] < measured[k])
        {
          # local surplus
          hours_helpg_syst = hours_helpg_syst + 1 
          extra_energy = measured[k]-contracted[k]
          
          ba_revenue[k] = extra_energy*reg_down[k]
          revenues[k] = da_revenue[k] + ba_revenue[k] # sold => +
          
          down_regulation_costs = down_regulation_costs + extra_energy*reg_down[k]
          surplus = surplus + extra_energy
        } else if (contracted[k] > measured[k])
        {
          # local shortage
          hours_handicp_syst = hours_handicp_syst + 1
          missing_energy = contracted[k]-measured[k]
          
          ba_revenue[k] = missing_energy*reg_up[k] # bought => -
          revenues[k] = da_revenue[k] - ba_revenue[k]
          
          up_regulation_costs = up_regulation_costs + missing_energy*reg_up[k]
          shortage = shortage + missing_energy
        } else 
        {
          # balance
          hours_helpg_syst = hours_helpg_syst + 1 
          ba_revenue[k] = 0
          revenues[k] = da_revenue[k] + ba_revenue[k] 
        }
        
        # cat(paste0("Day : ", i, " - I am not scheduled at h : ", k), "\n")
      }
    } 
  }
  
  results = list(da_revenue = da_revenue,
                 ba_revenue = ba_revenue,
                 revenues = revenues,  # sum of the previous two
                 surplus = surplus,
                 shortage = shortage,
                 down_regulation_costs = down_regulation_costs,
                 up_regulation_costs = up_regulation_costs,
                 hours_helpg_syst = hours_helpg_syst,
                 hours_handicp_syst = hours_handicp_syst,
                 well_spotted = well_spotted)
  return(results)
}





