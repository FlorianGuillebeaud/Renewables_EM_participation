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

balancing = function(contracted, measure, schedule, reg_up, reg_down)
{
  # make sure everything is numerical
  contracted = as.numeric(contracted)
  measure = as.numeric(measure)
  reg_up = as.numeric(reg_up)
  reg_down = as.numeric(reg_down)
  
  # initialisation 
  revenues = vector(length = length(contracted))
  hours_helpg_syst = 0 # count
  hours_handicp_syst = 0 # count
  well_spotted = 0 # count
  surplus = shortage = 0 # count
  down_regulation_costs = up_regulation_costs = 0 # count
  
  for (k in 1:length(contracted))
  {
    if (is.na(schedule[k])== FALSE)
    { # we are scheduled => let's see the revenues!
      #cat(paste0("We are contracted : ", k , "/ ", length(contracted)), "\n")
      if( (schedule[k] == reg_up[k]) & (schedule[k] == reg_down[k])) # balance
      {
        #cat(paste0("Perfect global balance / "))
        balancing_price = schedule[k]
        #cat(paste0("balancing price = ", balancing_price), "\n")
        
        if (contracted[k] < measure[k])
        {
          # local surplus
          hours_helpg_syst = hours_helpg_syst + 1 
          extra_energy = measure[k]-contracted[k]
          revenues[k] = contracted[k]*schedule[k] + extra_energy*balancing_price # sold => +
          
          down_regulation_costs = down_regulation_costs + extra_energy*balancing_price
          surplus = surplus + extra_energy
        } 
        else if (contracted[k] > measure[k])
        {
          # local shortage 
          hours_handicp_syst = hours_handicp_syst + 1
          missing_energy = contracted[k]-measure[k]
          revenues[k] = contracted[k]*schedule[k] - missing_energy*balancing_price # bought => -
          
          up_regulation_costs = up_regulation_costs + missing_energy*balancing_price
          shortage = shortage + missing_energy
        } 
        else 
        {
          # I am "helping"
          hours_helpg_syst = hours_helpg_syst + 1
          well_spotted = well_spotted + 1
          revenues[k] = contracted[k]*balancing_price
        } 
        
        
      }else if((schedule[k] < reg_up[k]) & (schedule[k] > reg_down[k]))  
      {
        # weird situation where during one hour syst is both short and long
        #cat(paste0("both reg up and down at : ", k), "\n")
        if (contracted[k] < measure[k]){
          # surplus : reg_down
          balancing_price = reg_down[k]
          #cat(paste0("balancing price = ", balancing_price), "\n")
          
          hours_helpg_syst = hours_helpg_syst + 1 
          extra_energy = measure[k]-contracted[k]
          revenues[k] = contracted[k]*schedule[k] + extra_energy*balancing_price # sold => +
          
          down_regulation_costs = down_regulation_costs + extra_energy*balancing_price
          surplus = surplus + extra_energy
        } else if (contracted[k] > measure[k]){
          # shortage : reg_up
          balancing_price = reg_up[k]
          
          hours_handicp_syst = hours_handicp_syst + 1
          missing_energy = contracted[k]-measure[k]
          revenues[k] = contracted[k]*schedule[k] - missing_energy*balancing_price # bought => -
          
          up_regulation_costs = up_regulation_costs + missing_energy*balancing_price
          shortage = shortage + missing_energy
        } else {
          # I am "helping"
          hours_helpg_syst = hours_helpg_syst + 1
          well_spotted = well_spotted + 1
          revenues[k] = contracted[k]*schedule[k]
        }
      }else if(schedule[k] < reg_up[k]) # shortage 
      {
        # overall system in shortage => blancing price = reg_up price
        balancing_price = reg_up[k]
        #cat(paste0(i, " - sys shortage - balancing_price / ", balancing_price), "\n")
        
        if (contracted[k] < measure[k]){
          # surplus : I am helping => will NOT rewarded 

          hours_helpg_syst = hours_helpg_syst + 1 
          extra_energy = measure[k]-contracted[k]
          revenues[k] = contracted[k]*schedule[k] + extra_energy*schedule[k] 
          
          down_regulation_costs = down_regulation_costs + extra_energy*schedule[k]
          surplus = surplus + extra_energy
        } else if (contracted[k] > measure[k]){
          # shortage : I am NOT helping => will get penalized 
          hours_handicp_syst = hours_handicp_syst + 1
          missing_energy = contracted[k]-measure[k]
          revenues[k] = contracted[k]*schedule[k] - missing_energy*balancing_price # bought => -
          
          up_regulation_costs = up_regulation_costs + missing_energy*balancing_price
          shortage = shortage + missing_energy
        } else {
          # I am "helping"
          hours_helpg_syst = hours_helpg_syst + 1
          well_spotted = well_spotted + 1
          revenues[k] = contracted[k]*schedule[k]
        }
      }else if (schedule[k] > reg_down[k]) # surplus
      {
        # overall system in surplus => balancing price = reg_down price
        balancing_price = reg_down[k]      
        #cat(paste0(k, " - sys surplus - balancing_price / ", balancing_price), "\n")
        
        
        if (contracted[k] < measure[k]){
          # surplus : I am NOT helping => will get penalized
          hours_helpg_syst = hours_helpg_syst + 1 
          extra_energy = measure[k]-contracted[k]
          revenues[k] = contracted[k]*schedule[k] + extra_energy*balancing_price # sold => +
          
          down_regulation_costs = down_regulation_costs + extra_energy*balancing_price
          surplus = surplus + extra_energy
        } else if (contracted[k] > measure[k]){
          # shortage : I am not helping => will get penalized 
          hours_handicp_syst = hours_handicp_syst + 1
          missing_energy = contracted[k]-measure[k]
          revenues[k] = contracted[k]*schedule[k] - missing_energy*schedule[k] # bought => -
          
          up_regulation_costs = up_regulation_costs + missing_energy*schedule[k]
          shortage = shortage + missing_energy
        } else {
          # I am "helping" / not rewarded
          hours_helpg_syst = hours_helpg_syst + 1
          well_spotted = well_spotted + 1
          revenues[k] = contracted[k]*schedule[k]
        }
      }else 
      {
        revenues[k] = NA
        #cat(paste0("I am not schedule : ", k))
      } # we are NOT scheduled => no revenues ! 
    } 
  }
  
  results = list(revenues = revenues, 
                 surplus = surplus,
                 shortage = shortage,
                 down_regulation_costs = down_regulation_costs,
                 up_regulation_costs = up_regulation_costs,
                 hours_helpg_syst = hours_helpg_syst,
                 hours_handicp_syst = hours_handicp_syst,
                 well_spotted = well_spotted)
  return(results)
}





