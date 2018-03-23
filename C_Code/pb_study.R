## Assignment 2 ## 
## Renewables in electricity market ## 
## Author : Florian Guillebeaud ## 
###################################
###################################

setwd("~/Documents/DTU/B_Semester-2/31761_Renew_ElectricityMarkets/Assignments/Assignment2")


source("C_Code/read_regulations.R")
source("C_Code/read_elspot.R") # ! price is given in DKK/MWh
source("C_Code/read_prod.R")

prices_16 = data.frame(prod_down = reg_prod_2016$dw_DK1,
                       reg_dw = regulating_prices_2016$DK1_DOWN,
                       elspot = elspot_price_2016$DK1, 
                       reg_up = regulating_prices_2016$DK1_UP,
                       prod_up = reg_prod_2016$up_DK1)
View(prices_16)

prices_17 = data.frame(prod_down = reg_prod_2017$dw_DK1,
                       reg_dw = regulating_prices_2017$DK1_DOWN,
                       elspot = elspot_price_2017$DK1, 
                       reg_up = regulating_prices_2017$DK1_UP,
                       prod_up = reg_prod_2017$up_DK1)
View(prices_17)

plot(prices_17$elspot[1:5], type = 'o')
points(prices_17$reg_up[1:5], pch = 3, col = "red")
lines(prices_17$reg_up[1:5], lty = 2, col = "red")
points(prices_17$reg_dw[1:5], pch = 3, col = "blue")
lines(prices_17$reg_dw[1:5], lty = 1, col = "blue")
