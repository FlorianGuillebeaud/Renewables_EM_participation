## Assignment 2 ## 
## Renewables in electricity market ## 
## Author : Florian Guillebeaud ## 
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

source("C_Code/scenario_output.R")

###################################
## Quantity Bid ## 
###################################

## Scenario 1 : We bid what forecasted ##
scenario1 = scenario_output(1, data_wp, elspot_price_2017, regulating_prices_2017, plot_results = TRUE)

## Scenario 2 : Perfect forecast ## 
scenario2 = scenario_output(2, data_wp, elspot_price_2017, regulating_prices_2017, plot_results = TRUE)

## Scenario 3 : Persistence forecast (using the last measured power value at 11h)
scenario3 = scenario_output(3, data_wp, elspot_price_2017, regulating_prices_2017, plot_results = FALSE)

## Scenario 4 : Random bid between 0 and 20 MW
scenario4 = scenario_output(4, data_wp, elspot_price_2017, regulating_prices_2017, plot_results = FALSE)

## Scenario 5 : We bid a constant amount based on an estimated CF ## 
scenario5 = scenario_output(5, data_wp, elspot_price_2017, regulating_prices_2017, plot_results = TRUE)



##################################
## Post traitement Scenarios ## 
###################################

scenario2$




###################################
###################################

# SPOT PRICES
N = min(length(elspot_price_2016$DK1), length(elspot_price_2017$DK1))
plot(1:N, elspot_price_2016$DK1[1:N], type = "l", xlab = "Time [h]", ylab = "[DKK/MWh]")
lines(1:N, elspot_price_2017$DK1[1:N], col = "blue")
legend("bottomleft", legend = c(paste0("2016 / mean price : ", round(mean(elspot_price_2016$DK1[1:N], na.rm = TRUE), digits = 2), " DKK/MWh"),
                                paste0("2017 / mean price : ", round(mean(elspot_price_2017$DK1[1:N], na.rm = TRUE), digits = 2), " DKK/MWh")), col = c("black", "blue"), lty = 1)
title(main = "Electricity Spot Price in DK1")


###################################
###################################
## compute key figures for regulation prices DKK/MWh

# average up and down regulations costs
down_reg_av_2016 = mean(regulating_prices_2016$DK1_DOWN, na.rm=TRUE)
up_reg_av_2016 = mean(regulating_prices_2016$DK1_UP, na.rm=TRUE)

down_reg_av_2017 = mean(regulating_prices_2017$DK1_DOWN, na.rm=TRUE)
up_reg_av_2017 = mean(regulating_prices_2017$DK1_UP, na.rm=TRUE)

###################################
###################################

# What date do you want to plot ? 
# 28th of March 2016 : YYYYMMDD
date = 20170328

year = noquote(sub("^(\\d{4}).*$", "\\1", date))
reg_date_up = eval(parse(text = paste0("regulating_prices_",year,"[regulating_prices_",year,"$date_daily==date,]$DK1_UP")))
reg_date_down = eval(parse(text = paste0("regulating_prices_",year,"[regulating_prices_",year,"$date_daily==date,]$DK1_DOWN")))
price_date = eval(parse(text = paste0("elspot_price_",year,"[elspot_price_",year,"$date_daily==date,]$DK1")))
wind_fore_date = eval(data_wp[data_wp$date_daily==date,]$fore)
wind_meas_date = eval(data_wp[data_wp$date_daily==date,]$meas)

# Balancing Price market for this date
plot(price_date,type = "o", ylim = c(min(reg_date_up,reg_date_down, price_date, na.rm = TRUE), max(reg_date_up, reg_date_down, price_date, na.rm = TRUE)),
     xlab = "Hour of the day [h]", ylab = "DKK/MWh", lty = 1, lwd = 2)
points(reg_date_up, col = "blue", type = "o", lty = 2 )
points(reg_date_down, col ="red", type ="o", lty = 2)
legend("topleft", legend = c("Spot price", "Up-reg. price", "Down-reg. price"), 
       col = c("black", "blue", "red"), lty = c(1,2,2), cex = 0.75, lwd = c(2,1,1))
title(main = paste0("Prices the : ", date))


# in March 2017 : YYYYMM
date = 201703

year = noquote(sub("^(\\d{4}).*$", "\\1", date))
reg_date_up = eval(parse(text = paste0("regulating_prices_",year,"[regulating_prices_",year,"$date_monthly==date,]$DK1_UP")))
reg_date_down = eval(parse(text = paste0("regulating_prices_",year,"[regulating_prices_",year,"$date_monthly==date,]$DK1_DOWN")))
price_date = eval(parse(text = paste0("elspot_price_",year,"[elspot_price_",year,"$date_monthly==date,]$DK1")))
wind_fore_date = eval(parse(text = paste0("wind_power_",year,"[wind_power_",year,"$date_monthly==date,]$fore")))
wind_meas_date = eval(parse(text = paste0("wind_power_",year,"[wind_power_",year,"$date_monthly==date,]$meas")))


plot(price_date,type = "l", ylim = c(min(reg_date_up,reg_date_down, price_date, na.rm = TRUE), max(reg_date_up, reg_date_down, price_date, na.rm = TRUE)),
     xlab = paste0("Month studied : ", date, " [h]"), ylab = "DKK/MWh", lty = 1)
points(reg_date_up, col = "blue", type = "l", lty = 2 )
points(reg_date_down, col ="red", type ="l")
par(xpd=TRUE)
legend(list(x = 0,y = 2), legend = c("Spot price", "Up-reg. price", "Down-reg. price"), 
       col = c("black", "blue", "red"), lty = c(1,2,2), cex = 0.75)

#
plot(wind_fore_date, type = "l", xlab = "Time [h]", ylab = "[kW]")
lines(wind_meas_date, type = "l", lty = 2, col = "blue")
title(main = paste0("Wind power production in : ", date))
legend(list(x = 0,y = 2), legend = c("forecasted", "measured"), col = c("black", "blue"), lty = c(1,2), cex = 0.75, lwd = 2)
title(main = paste0("Wind power production : ", date))


####

