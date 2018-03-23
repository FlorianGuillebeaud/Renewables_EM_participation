## Assignment 2 ## 
## Renewables in electricity market ## 
## Author : Florian Guillebeaud ## 
###################################
###################################

# Compute the alpha for the quantile 

setwd("~/Documents/DTU/B_Semester-2/31761_Renew_ElectricityMarkets/Assignments/Assignment2")


source("C_Code/read_regulations.R")
source("C_Code/read_elspot.R") # ! price is given in DKK/MWh
source("C_Code/read_prod.R")


###################################
###################################
# 2016 
pi_minus16 = regulating_prices_2016$DK1_UP - elspot_price_2016$DK1
pi_minus16[is.na(pi_minus16)] <- 0 
pi_plus16 = elspot_price_2016$DK1 - regulating_prices_2016$DK1_DOWN
pi_plus16[is.na(pi_plus16)] <- 0 
alpha16 = pi_plus16/(pi_plus16+pi_minus16)
alpha16[is.na(alpha16)] <- 0 
alpha16[which(alpha16>1)] <- 1 
alpha16[which(alpha16==Inf)] <- 0 
alpha16[which(alpha16==-Inf)] <- 0 

hour = vector()
for (i in 1:length(alpha16)){
  hour[i] = i%%24
}

alpha_2016 = data.frame(date_monthly = regulating_prices_2016$date_monthly,
                        date_daily = regulating_prices_2016$date_daily,
                        date_hourly = regulating_prices_2016$date_hourly,
                        pi_minus = pi_minus16, 
                        hours = hour,
                        pi_plus = pi_plus16,
                        alpha = alpha16)

View(alpha_2016)
## Hourly average over the year
alpha_h_av = vector()
for (i in 1:24){
  alpha_h_av[i%%24] = sum(alpha_2016$alpha[alpha_2016$hours==(i%%24)])/length(alpha_2016$alpha[alpha_2016$hours==(i%%24)])
}

plot(alpha_av, type = "o", xlab = "PTU", ylab = "Average quantile")
title(main = "Hourly average of the quantile in 2016")

## Hourly average over each month 
month = c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
alpha_m_av = vector()
for(i in 1:12){
  date = paste0("2016", month[i])
  data_temp =  alpha_2016$alpha[alpha_2016$date_monthly==date]
  data_temp_1 = vector()
  for (j in 1:length(data_temp)){
    k = (j%%24)
    cat(paste0(k), "\n")
    temp = data_temp[j%%24]
    cat(paste0(temp), "\n")
    data_temp_1[k] = data_temp_1[k] + temp 
  }
  alpha_m_av[i,] = alpha_2016$alpha[alpha_2016$date_monthly==date]
}
