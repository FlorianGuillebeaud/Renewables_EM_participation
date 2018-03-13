## Assignment 2 ## 
## Renewables in electricity market ## 
## Author : Florian Guillebeaud ## 
###################################
###################################

# Scenario 1
# We bid the forecast 
# We are scheduled at the spot price

###################################
###################################

setwd("~/Documents/DTU/B_Semester-2/31761_Renew_ElectricityMarkets/Assignments/Assignment2")

###################################
###################################

source("C_Code/read_wp.R") # ! quantities in kW
source("C_Code/read_elspot.R") # ! price is given in DKK/MWh
source("C_Code/read_regulations.R")

source("C_Code/get_schedule.R")
source("C_Code/balancing.R")
source("C_Code/performance_ratio.R")

###################################
###################################

# 2016-12-31 At 12:00 we bid for the next day 2017-01-01 energy as block unit to be scheduled from 00:00 to 00:00 
# the forecast is made at 11:00 and 12:00

###################################
###################################

revenues = matrix(0,nrow = 1, ncol = 24)
balancing_quantities =  matrix(0,nrow = 1, ncol = 5)
hours_helpg_syst = hours_handicp_syst = 0 

# since we bid at 12:00 the forecasted amount, we have access at closest to
# the forecast made at 11:00 the d-day   
dati_temp = matrix(noquote(unique(data_wp$dati)), nrow = length(unique(data_wp$dati)), ncol =1 )
dati_to_consider <- dati_temp[seq(1, length(dati_temp), 2)] # when the forecast is issued 


###################################
###################################

# Turn plot_results to TRUE to see a plot every month
plot_results = TRUE
for (i in 1:(length(dati_to_consider)-1)){
  
  # Find the index from where extract the wind data for each forecasted day to be considered
  temp_dati = data_wp[data_wp$dati==dati_to_consider[i] & data_wp$hors==13,]
  index_next_day = as.numeric(matrix(noquote((row(temp_dati, TRUE)[1])),1,1))
  
  # assign the wind predictions for next day
  wp_next_day = data_wp[(index_next_day-1):(index_next_day+22),]
  
  # remember the date bidded (the following)
  date_bidded = as.numeric(matrix(noquote(wp_next_day$date_daily[1])),1,1)
  
  # Quantity Bid # 
  ## We bid what forecasted ##
  # contracted = as.numeric(wp_next_day$fore)/10^3 # MWh
  ## Perfect forecast ## 
  # contracted = as.numeric(wp_next_day$meas)/10^3 # MWh
  ## Persistence forecast (using the last measured power value at 11h)
  # if (i==1) {
  #   # We don't the data for this day / we consider the forecast
  #   temp_quantity = as.numeric(matrix(noquote(wp_next_day$fore[1]),1,1))
  #   contracted = rep(temp_quantity/10^3,24)
  # }else
  # {
  #   # We take the last measured power value at 11h
  #   index_last_meas = index_next_day-28-1 
  #   contracted = rep(as.numeric(matrix(noquote(data_wp[index_last_meas,]$meas),1,1))/10^3,24)
  # }
  ## Random bid
  contracted = runif(24,0,20) # MW
  
  # in case of NA values
  contracted[is.na(contracted)] <- 0
  
  # what we actually produced
  measured = as.numeric(wp_next_day$meas)/10^3 # MWh
  # in case of NA values
  measured[is.na(measured)] <- 0
  
  # the prices we bid 
  bid_price = rep(0,24)
  
  # what is the spot price for this day ? 
  elspot = elspot_price_2017[elspot_price_2017$date_daily==date_bidded,]$DK1
  # average the missing values by looking at the hour before and after
  for (j in 2:(length(elspot)-1)){
    if(is.na(elspot[j])==TRUE) {
      elspot[j] = 0.5*(elspot[j-1]+elspot[j+1])
      cat(paste0("Missing value in the spot price dataset : index ", j), "\n")
    }
  }
  
  # what are the regulation prices ? 
  reg_down = regulating_prices_2017[regulating_prices_2017$date_daily==date_bidded,]$DK1_DOWN
  for (j in 1:length(reg_down)){ 
    if(is.na(reg_down[j])==TRUE){
      reg_down[j]<-elspot[j] # if reg_price missing : it's assumed it's spot_price
      cat(paste0("Missing value in the regulating down dataset : index ", j), "\n")
    }}
  reg_up = regulating_prices_2017[regulating_prices_2017$date_daily==date_bidded,]$DK1_UP
  for (j in 1:length(reg_up)){ 
    if(is.na(reg_up[j])==TRUE){
      reg_up[j]<-elspot[j] # if reg_price missing : it's assumed it's spot_price
      cat(paste0("Missing value in the regulating up dataset : index ", j), "\n")
    }}
  
  # Are we scheduled ?
  schedule = get_schedule(bid_price,elspot)
  
  ## plots
  if((plot_results == TRUE) & (i%%31 == 0)){
  cat(paste0("look at the : " , i), "\n")
  plot(elspot, type = "o", ylab = "spot price [DKK/MWh]",xlab = "Time of the day [h]", 
       ylim = c(min(reg_down, na.rm = TRUE), max(reg_up, na.rm = TRUE)))
  points(reg_up, type = "o", col = "red", lty = 4)
  points(reg_down, type = "o", col = "blue", lty = 2)
  legend("topleft", legend = c("spot price", "up-regulation price", "down-regulation price"),
  col = c("black", "red", "blue"), lty = c(1,4,2), pch = "o", cex = 0.75)
  title(main = paste0("Date of interest : ", date_bidded))
  
  plot(1:24,contracted, col = "red", type = "o", ylim = c(min(contracted,measured),max(contracted,measured)), ylab = "power [MW]", xlab = "Time of the day [h]")
  points(1:24, measured, col = "black", type = "o")
  legend("topright", legend = c("measurements","contracted = forecast" ), col = c("black", "red"),
  pch = "o", lty = 1)
  title(main = paste0("Date of interest : ", date_bidded))
  }
  
  # Balancing market clearing
  balancing_results = balancing( contracted = contracted, 
                                 measure = measured,
                                 schedule = schedule,
                                 reg_up = reg_up,
                                 reg_down = reg_down)
  
  # Performance ratio
  pr = performance_ratio(contracted = contracted,
                         measure = measured,
                         schedule = schedule,
                         reg_up = reg_up,
                         reg_down = reg_down)
  
  # We remember the revenue generated hourly
  new_revenues = balancing_results$revenues
  
  new_surplus = balancing_results$surplus
  new_shortage = balancing_results$shortage
  new_down_regulation_costs = balancing_results$down_regulation_costs
  new_up_regulation_costs = balancing_results$up_regulation_costs
  new_balancing_quantities = c(new_surplus,new_shortage,new_down_regulation_costs,new_up_regulation_costs,
                               pr)
  
  # How many times have we helped / handicaped the syst by bidding this way
  hours_helpg_syst = hours_helpg_syst + balancing_results$hours_helpg_syst
  hours_handicp_syst = hours_handicp_syst + balancing_results$hours_handicp_syst
  
  
  if (i==1){
    revenues = rbind(new_revenues)
    balancing_quantities = rbind(new_balancing_quantities)
  }
  else {
    revenues = rbind(revenues, new_revenues)    
    balancing_quantities = rbind(balancing_quantities, new_balancing_quantities)
  }
}



total_surplus = sum(balancing_quantities[,1], na.rm = TRUE)
total_shortage = sum(balancing_quantities[,2], na.rm = TRUE)
total_down_regulation_costs = sum(balancing_quantities[,3], na.rm = TRUE)
total_up_regulation_costs = sum(balancing_quantities[,4], na.rm = TRUE)

av_down_regulation_unit_costs = total_down_regulation_costs/total_surplus
av_up_regulation_unit_costs = total_up_regulation_costs/total_shortage

###################################
###################################
# Sort the results into one table
balancing_quantities = data.frame(surplus = balancing_quantities[,1],
                                  shortage = balancing_quantities[,2],
                                  down_regulation_costs = balancing_quantities[,3],
                                  up_regulation_costs = balancing_quantities[,4],
                                  performance_ratio = balancing_quantities[,5],
                                  daily_revenue = rowSums(revenues))


###################################
###################################
# Print some plots

par(mar = c(6, 4.5,2, 2)) 
if (sum(balancing_quantities$surplus, na.rm = TRUE) > sum(balancing_quantities$shortage, na.Rm = TRUE)){
  plot(balancing_quantities$surplus, type = 'h', lwd = 4, ylim = c(0, max(balancing_quantities$surplus,balancing_quantities$shortage)+50),
       xlab = "Days in 2017", ylab = "Energy [MWh]")
  lines(balancing_quantities$shortage, col = "grey", type ="h", lwd = 4)
  legend("topleft", legend = c("Surplus","Shortage"), col = c("black", "grey"), lty = 1, lwd = 4)
  title(main = "Bid the forecast !")
} else{
  plot(balancing_quantities$shortage, col = "black", type = 'h', lwd = 4, ylim = c(0, max(balancing_quantities$surplus,balancing_quantities$shortage)+50),
       xlab = "Days in 2017", ylab = "Energy [MWh]")
  lines(balancing_quantities$surplus, col = "grey", type ="h", lwd = 4)
  legend("topleft", legend = c("Surplus","Shortage"), col = c("grey","black"), lty = 1, lwd = 4)
  title(main = "Bid the forecast !")
  
} 

# Figures
helping = round(hours_helpg_syst/((length(dati_to_consider)-1)*24)*100, digits = 2)
handicp = round(hours_handicp_syst/((length(dati_to_consider)-1)*24)*100, digits = 2)
slices = c(helping, handicp)
lbls = c(paste0("surplus : ", helping, " %"), paste0("shortage : ", handicp, " %"))
par(mar = c(5, 5,4, 5))
pie(slices, lbls, main = "2017 : scenario 1", col=c("blue", "grey"))

# Revenues
plot(1:24,colMeans(revenues, na.rm = TRUE), type = "h", lwd = 5, 
     ylim = c(0, min(colMeans(revenues), na.rm = TRUE)),
     xlab = "Hour of the day [h]", ylab = "Average revenues [DKK]")
title(main = "Hourly average of revenue generation in 2017")

