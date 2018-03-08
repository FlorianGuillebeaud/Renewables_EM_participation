## Assignment 2 ## 
## Renewables in electricity market ## 
## Author : Florian Guillebeaud ## 
###################################
###################################

# Scenario 1
# Bid the forecast at 
# and see the resulting revenues


###################################
###################################

setwd("~/Documents/DTU/B_Semester-2/31761_Renew_ElectricityMarkets/Assignments/Assignment2")

###################################
###################################

source("C_Code/read_wp.R") # ! quantities in kW
source("C_Code/read_elspot.R") # ! price is given in DKK/MWh
source("C_Code/read_regulations.R")
source("C_Code/balancing_revenues.R")

###################################
###################################

# 2016-12-31 AT 12:00 we bid for the next day 2017-01-01
# Energy as block unit to be scheduled from 00:00 to 00:00 

revenues = matrix(0,nrow = 1, ncol = 24)
hours_helpg_syst = hours_handicp_syst = 0 

# since we bid what is schedule, we get the last closest forecast date of the 
# 24 energy bid to be deliver 
dati_temp = matrix(noquote(unique(data_wp$dati)), nrow = length(unique(data_wp$dati)), ncol =1 )
dati_to_consider <- dati_temp[seq(1, length(dati_temp), 2)]

# 
plot_results = FALSE
for (i in 1:(length(dati_to_consider)-1)){
  
  # Find the index from where extract the wind data for each forecasted day to be considered
  temp = data_wp[data_wp$dati==dati_to_consider[i] & data_wp$hors==13,]
  index_next_day = as.numeric(matrix(noquote((row(temp, TRUE)[1])),1,1))
  
  # assign the wind predictions for next day
  wp_next_day = data_wp[(index_next_day-1):(index_next_day+22),]
  
  # remember the date bidded (the following)
  date_bidded = as.numeric(matrix(noquote(wp_next_day$date_daily[1])),1,1)
  
  # since we bid what forecasted
  contracted = as.numeric(wp_next_day$fore)/10^3 # MWh
  # in case of NA values
  contracted[is.na(contracted)] <- 0
  
  # what we actually produced
  measured = as.numeric(wp_next_day$meas)/10^3 # MWh
  # in case of NA values
  measured[is.na(measured)] <- 0
  
  # what is the clearing ? 
  elspot = elspot_price_2017[elspot_price_2017$date_daily==date_bidded,]$DK1
  # what is the regulation ? 
  reg_down = regulating_prices_2017[regulating_prices_2017$date_daily==date_bidded,]$DK1_DOWN
  reg_up = regulating_prices_2017[regulating_prices_2017$date_daily==date_bidded,]$DK1_UP
  
  ## plots
  if(plot_results == TRUE){
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
  balancing = balancing_revenues(contracted = contracted, 
                                          measure = measured,
                                          spot_price = elspot,
                                          reg_up = reg_up,
                                          reg_down = reg_down)
  new_revenues = balancing$revenues
  hours_helpg_syst = hours_helpg_syst + balancing$hours_helpg_syst
  hours_handicp_syst = hours_handicp_syst + balancing$hours_handicp_syst
  
  
  if (i==1) revenues = rbind(new_revenues)
  else revenues = rbind(revenues, new_revenues)
}

# Figures
helping = round(hours_helpg_syst/((length(dati_to_consider)-1)*24)*100, digits = 2)
handicp = round(hours_handicp_syst/((length(dati_to_consider)-1)*24)*100, digits = 2)
slices = c(helping, handicp)
lbls = c(paste0("positive imbalance: ", helping, " %"), paste0("negative imbalance : ", handicp, " %"))
par(mar = c(5, 5,4, 5))
pie(slices, lbls, main = "2017 : scenario 1", col=c("blue", "grey"))

# Revenues
plot(1:24,colMeans(revenues, na.rm = TRUE), type = "h", lwd = 5, ylim = c(0, min(colMeans(revenues), na.rm = TRUE)),
     xlab = "Hour of the day [h]", ylab = "Average revenues [DKK]")
title(main = "Hourly average of revenue generation in 2017")
