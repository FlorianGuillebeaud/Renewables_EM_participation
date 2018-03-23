## Assignment 2 ## 
## Renewables in electricity market ## 
## Author : Florian Guillebeaud ## 
###################################
###################################

setwd("~/Documents/DTU/B_Semester-2/31761_Renew_ElectricityMarkets/Assignments/Assignment2")


###################################
###################################
library("forecast")

###################################
###################################

source("C_Code/read_wp.R") # ! quantities in kW
source("C_Code/read_elspot.R") # ! price is given in DKK/MWh
source("C_Code/read_regulations.R")

source("C_Code/get_schedule.R")
source("C_Code/balancing_new.R")
source("C_Code/performance_ratio.R")
source("C_Code/get_best_quantile.R")
source("C_Code/quantile_distribution.R")
source("C_Code/scenario_output.R")
source("C_Code/plot_quantile.R")

###################################
## Quantity Bid ## 
###################################

## Scenario 1 : We bid what forecasted ##
scenario1 = scenario_output(1, data_wp, elspot_price_2017, regulating_prices_2017, plot_results = TRUE)

## Scenario 2 : Perfect forecast ## 
scenario2 = scenario_output(2, data_wp, elspot_price_2017, regulating_prices_2017, plot_results = TRUE)
ideal_revenue = sum(scenario2$revenues_hourly, na.rm = TRUE)

## Scenario 3 : Persistence forecast (using the last power measurement value at 11h)
scenario3 = scenario_output(3, data_wp, elspot_price_2017, regulating_prices_2017, plot_results = TRUE)

## Scenario 4 : Random bid between 0 and MaxProd MW
actual_revenue = pf = vector()
for (j in 1:100){
  cat(paste0("round : ", j ), "\n")
  scenario4 = scenario_output(4, data_wp, elspot_price_2017, regulating_prices_2017, plot_results = FALSE)
  actual_revenue[j] = sum(scenario4$revenues_hourly, na.rm = TRUE)
  pf[j] = actual_revenue[j]/ideal_revenue
}
## Scenario 5 : We bid a constant amount based on an estimated CF ## 
scenario5 = scenario_output(5, data_wp, elspot_price_2017, regulating_prices_2017, plot_results = TRUE)

## Scenario 6 : We bid the median (0.5 quantile) ## 
scenario6 = scenario_output(6, data_wp, elspot_price_2017, regulating_prices_2017, plot_results = TRUE)

## Scenario 7 : using the otpimal quantile
scenario7 = scenario_output(7, data_wp, elspot_price_2017, regulating_prices_2017, plot_results = TRUE)


##################################
## Post traitement Scenarios ## 
###################################

# Maximum we can get 
plot((rowMeans(scenario2$revenues_hourly)/10^3)[1:31], ylim = c(0,40),
     type = 'h', lwd = 20, xlab = "Time [Days]", ylab = "Revenue [k€]")
title(main="Revenue in January 2017")
# Compare to our scenarios
lines((rowMeans(scenario1$revenues_hourly)/10^3)[1:31], type = 'h', lwd = 17, col = "grey")
lines((rowMeans(scenario3$revenues_hourly)/10^3)[1:31], type = 'h', lwd = 14, col = "yellow")
lines((rowMeans(scenario4$revenues_hourly)/10^3)[1:31], type = 'h', lwd = 11, col = "green")
lines((rowMeans(scenario5$revenues_hourly)/10^3)[1:31], type = 'h', lwd = 8, col = "blue")
# lines((rowMeans(scenario6$revenues_hourly)/10^3)[1:31], type = 'h', lwd = 8, col = "blue")
# lines((rowMeans(scenario7$revenues_hourly)/10^3)[1:31], type = 'h', lwd = 8, col = "blue")
legend("topright", legend = c("Perfect forecast", "Believe in forecast", "Persistence forecast", "Random bid using CF=0.5", "Constant bid using CF=0.5"),
       col = c("black", "grey", "yellow", "green", "blue"), lty = 1, lwd = c(20,17,14,11,8), cex = 0.75)

###################################
###################################
# Cumulative revenue plot 
scenario1$revenues_hourly[is.na(scenario1$revenues_hourly)] <- 0
scenario2$revenues_hourly[is.na(scenario2$revenues_hourly)] <- 0
scenario3$revenues_hourly[is.na(scenario3$revenues_hourly)] <- 0
scenario4$revenues_hourly[is.na(scenario4$revenues_hourly)] <- 0
scenario5$revenues_hourly[is.na(scenario5$revenues_hourly)] <- 0
scenario6$revenues_hourly[is.na(scenario6$revenues_hourly)] <- 0
scenario7$revenues_hourly[is.na(scenario7$revenues_hourly)] <- 0

# plot the optimal first
plot((cumsum(scenario2$revenues_hourly)/10^7)[0:200], type = 'l')
lines((cumsum(scenario1$revenues_hourly)/10^7)[0:200], type = 'l', lty = 2, col = "red")
lines(cumsum(scenario3$revenues_hourly)/10^7, type = 'l', lty = 2, col = "green")
lines(cumsum(scenario4$revenues_hourly)/10^7, type = 'l', col = "blue")
lines(cumsum(scenario5$revenues_hourly)/10^7, type = 'l', col = "orange")
lines(cumsum(scenario6$revenues_hourly)/10^7, type = 'l', lty = 2, col = "yellow")
lines(cumsum(scenario7$revenues_hourly)/10^7, type = 'l', col = "purple")

###################################
###################################
# Cumulative balancing / day ahead revenue plot 
scenario1$da_revenue_hourly[is.na(scenario1$da_revenue_hourly)] <- 0
scenario1$ba_revenue_hourly[is.na(scenario1$ba_revenue_hourly)] <- 0
scenario2$da_revenue_hourly[is.na(scenario2$da_revenue_hourly)] <- 0
scenario2$ba_revenue_hourly[is.na(scenario2$ba_revenue_hourly)] <- 0
scenario7$da_revenue_hourly[is.na(scenario7$da_revenue_hourly)] <- 0
scenario7$ba_revenue_hourly[is.na(scenario7$ba_revenue_hourly)] <- 0

plot(cumsum(scenario1$da_revenue_hourly)/10^7, ylim = c(-2,15), type = 'l')
abline(h=0)
lines(cumsum(scenario1$ba_revenue_hourly)/10^7, type = 'l')
lines(cumsum(scenario2$da_revenue_hourly)/10^7, type = 'l', col = "green")
lines(cumsum(scenario2$ba_revenue_hourly)/10^7, type = 'l', col = "green")
lines(cumsum(scenario7$da_revenue_hourly)/10^7, type = 'l', col = "red")
lines(cumsum(scenario7$ba_revenue_hourly)/10^7, type = 'l', col = "red")

###################################
###################################
# SPOT PRICES
N = min(length(elspot_price_2016$DK1), length(elspot_price_2017$DK1))
plot(1:N, elspot_price_2016$DK1[1:N], type = "l", xlab = "Time [h]", ylab = "[DKK/MWh]")
lines(1:N, elspot_price_2017$DK1[1:N], col = "blue")
legend("bottomleft", legend = c(paste0("2016 / mean price : ", round(mean(elspot_price_2016$DK1[1:N], na.rm = TRUE), digits = 2), " DKK/MWh"),
                                paste0("2017 / mean price : ", round(mean(elspot_price_2017$DK1[1:N], na.rm = TRUE), digits = 2), " DKK/MWh")), col = c("black", "blue"), lty = 1)
title(main = "Electricity Spot Price in DK1")

# Hourly tendancy
temp_elspot = vector() # matrix(0,ncol = 24, nrow = length(seq(1,length(elspot_price_2016$DK1), 24)))
temp_up = temp_dw = vector() # matrix(0,ncol = 24, nrow = length(seq(1,length(regulating_prices_2016), 24)))

for (i in seq(1,length(elspot_price_2016$DK1), 24)){
  temp_elspot = rbind(temp_elspot,elspot_price_2016$DK1[i:(i+23)])
  temp_up = rbind(temp_up, regulating_prices_2016$DK1_UP[i:(i+23)])
  temp_dw = rbind(temp_dw, regulating_prices_2016$DK1_DOWN[i:(i+23)])
}
temp_elspot[is.na(temp_elspot)] <- 0
temp_up[is.na(temp_up)] <- 0
temp_dw[is.na(temp_dw)] <- 0

hourly_av_spot_2016 = colMeans(temp_elspot[,1:24])
hourly_av_up = colMeans(temp_up[,1:24])
hourly_av_dw = colMeans(temp_dw[,1:24])
plot(1:24, hourly_av_spot_2016, type = "o", 
     ylim = c(min(hourly_av_spot_2016, hourly_av_up, hourly_av_dw), max(hourly_av_spot_2016, hourly_av_up, hourly_av_dw)),
     col = "blue", xlab = "Time [h]", ylab = "Price [DKK/MWh]", lwd = 2)
points(1:24, hourly_av_up, col = "red", pch = 3, lwd = 2)
lines(1:24, hourly_av_up, col = "red", lwd = 2)
points(1:24, hourly_av_dw, col = "darkorange", pch = 3, lwd = 2)
lines(1:24, hourly_av_dw,  col = "darkorange", lwd = 2)

plot(1:24, apply(temp_elspot, 2 , sd), type = "o",
     ylim = c(min(apply(temp_elspot,2,sd), apply(temp_up,2,sd),apply(temp_dw,2,sd)), max(apply(temp_elspot,2,sd), apply(temp_up,2,sd),apply(temp_dw,2,sd))),
     col = "blue", xlab = "Time [h]", ylab = "Standard deviation [DKK/MWh]", lwd = 2)
points(1:24, apply(temp_up, 2 , sd), col = "red", pch = 3, lwd = 2)
lines(1:24, apply(temp_up, 2 , sd), col = "red", lwd = 2)
points(1:24, apply(temp_dw, 2 , sd),  col = "darkorange", pch = 3, lwd = 2)
lines(1:24,  apply(temp_dw, 2 , sd),  col = "darkorange", lwd = 2)

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
     xlab = "Program Time Unit [h]", ylab = "DKK/MWh", lty = 1, lwd = 2)
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

