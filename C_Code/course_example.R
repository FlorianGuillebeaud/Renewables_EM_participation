## Course Example in R ## 

# Electricity spot prices 2016 DK1
source("C_Code/read_elspot.R")
elspot_price_example = elspot_price_16$DK1[2089:2112] # 28th of March

year = 2016
month = 03
day = 28 
look_date = paste0(year,month,day)
elspot_price_16$DK1[paste0(look_date,1):paste0(look_date,24)]


a = noquote(paste0("2016-03-28", sep = ""))
regulating_prices_2016$`Data was last updated 11-01-2018`[a:a]
regulating_prices_2016$`Data was last updated 11-01-2018`[noquote(rep(paste0("2016-03-28", sep = ","),24))]

# Regulating prices DK1
source("C_Code/read_regulations.R")
regulating_prices_up_example = DK1_regulating_prices_2016$UP[2089:2112] # 28th of March
regulating_prices_down_example = DK1_regulating_prices_2016$DOWN[2089:2112] #28th of March
  
plot(hours, elspot_price_example, type = "o", ylab = "spot price [€/MWh]",xlab = "Time of the day [h]", ylim = c(-200,700))
points(hours, regulating_prices_up_example, type = "o", col = "red", lty = 4)
points(hours, regulating_prices_down_example, type = "o", col = "blue", lty = 2)
legend("topleft", legend = c("spot price", "up-regulation price", "down-regulation price"), 
       col = c("black", "red", "blue"), lty = c(1,4,2), pch = "o")

# 
hours = seq(1,24,1)
forecast = c(129, 110, 96, 117, 132, 136,
             138, 137, 155, 180, 198, 187,
             159, 127, 112, 111, 116, 124,
             122, 108, 94, 81, 67, 68)
measurements = c(100, 78, 60, 67, 100, 132,
                 160, 165, 180, 187, 200, 188, 
                 170, 152, 140, 105, 98, 87,
                 70, 65,63,62,57, 55)

plot(hours, forecast, col = "red", type = "o", ylim = c(0,250), ylab = "power [MW]", xlab = "Time of the day [h]")
points(hours, measurements, col = "black", type = "o")


data = data.frame(hours = hours, offers = offers)
View(forecast_wp)
