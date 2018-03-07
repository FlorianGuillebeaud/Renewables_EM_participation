# Main 

setwd("~/Documents/DTU/B_Semester-2/31761_Renew_ElectricityMarkets/Assignments/Assignment2")

###################################
###################################

source("C_Code/calendar.R")
source("C_Code/read_wp.R")
source("C_Code/read_elspot.R")

###################################
###################################

# SPOT PRICES
DK1_price_16 = elspot_price_16$DK1
DK1_price_17 = elspot_price_17$DK1

N = min(length(DK1_price_16), length(DK1_price_17))
plot(1:N, DK1_price_16[1:N], type = "l", xlab = "Time [h]", ylab = "[DKK/MWh]")
lines(1:N, DK1_price_17[1:N], col = "blue")
legend("bottomleft", legend = c(paste0("2016 / mean price : ", round(mean(DK1_price_16[1:N], na.rm = TRUE), digits = 2), " DKK/MWh"),
                                paste0("2017 / mean price : ", round(mean(DK1_price_17[1:N], na.rm = TRUE), digits = 2), " DKK/MWh")), col = c("black", "blue"), lty = 1)
title(main = "Electricity Spot Price in DK1")

###################################
###################################

# What date do you want to plot ? 
# 28th of March 2016 : YYYYMMDD
date = 20160328

date_up = regulating_prices_2016[regulating_prices_2016$date_daily == date,]$DK1_UP
date_down = regulating_prices_2016[regulating_prices_2016$date_daily == date,]$DK1_DOWN
plot(date_up, type = "o", ylim = c(min(date_up,date_down), max(date_up, date_down)))
points(regulating_prices_2016[regulating_prices_2016$date_daily == date,]$DK1_DOWN, col ="red", type ="o")

# in March 2016 : YYYYMM
date = 201603

plot(regulating_prices_2016[regulating_prices_2016$date_monthly == date,]$DK1_UP, type = "o")
points(regulating_prices_2016[regulating_prices_2016$date_monthly == date,]$DK1_DOWN, col ="red", type ="o")

