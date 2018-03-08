## Assignment 2 ## 
## Renewables in electricity market ## 
## Author : Florian Guillebeaud ## 
###################################
###################################

## Course Example in R ## 

###################################
###################################

source("C_Code/read_elspot.R") # ! price is given in DKK/MWh
source("C_Code/read_regulations.R")
source("C_Code/balancing_revenues.R")
source("C_Code/performance_ratio.R")

###################################
###################################

# Constant 24h forecast/measurements (the one from the slide 14 lecture 5)
daily_study = function(date){
  
  year = noquote(sub("^(\\d{4}).*$", "\\1", date))
  if (year == 2018) return("Only 2016 and 2017 can be studied")
  
  elspot_price_example = eval(parse(text = paste0("elspot_price_", year,"[elspot_price_", year,"$date_daily==date,]$DK1")))
  
  
  # Regulating prices DK1
  regulating_prices_up_example = eval(parse(text = paste0("regulating_prices_", year,"[regulating_prices_", year,"$date_daily==date,]$DK1_UP"))) 
  regulating_prices_down_example = eval(parse(text = paste0("regulating_prices_",year,"[regulating_prices_",year,"$date_daily==date,]$DK1_DOWN")))
  
  plot(elspot_price_example, type = "o", ylab = "spot price [DKK/MWh]",xlab = "Time of the day [h]", 
       ylim = c(min(regulating_prices_down_example, na.rm = TRUE), max(regulating_prices_up_example, na.rm = TRUE)))
  points(regulating_prices_up_example, type = "o", col = "red", lty = 4)
  points(regulating_prices_down_example, type = "o", col = "blue", lty = 2)
  legend("topleft", legend = c("spot price", "up-regulation price", "down-regulation price"), 
         col = c("black", "red", "blue"), lty = c(1,4,2), pch = "o", cex = 0.75)
  title(main = paste0("Date of interest : ", date))
  
  # Wind power production
  hours = seq(1,24,1)
  forecast = c(129, 110, 96, 117, 132, 136,
               138, 137, 155, 180, 198, 187,
               159, 127, 112, 111, 116, 124,
               122, 108, 94, 81, 67, 68) # MW
  measurements = c(100, 78, 60, 67, 100, 132,
                   160, 179, 185, 192, 200, 188, 
                   170, 152, 140, 105, 98, 87,
                   70, 65,63,62,57, 55) # MW
  
  plot(hours, forecast, col = "red", type = "o", ylim = c(0,250), ylab = "power [MW]", xlab = "Time of the day [h]")
  points(hours, measurements, col = "black", type = "o")
  legend("topright", legend = c("measurements","forecasts" ), col = c("black", "red"),
         pch = "o", lty = 1)
  title(main = paste0("Date of interest : ", date))
  
  # Revenues if I bid what forecasted
  revenue_example = balancing_revenues(contracted = forecast, measure = measurements,
                                       spot_price = elspot_price_example, 
                                       reg_down = regulating_prices_down_example,
                                       reg_up = regulating_prices_up_example)
  
  
  plot(revenue_example, type = "h", lwd = 10, xlab = "Time of the day [h]", ylab = "Reevenues [DKK]")
  title(main = paste0("Date of interest : ", date))
  
  gamma_perf = performance_ratio(contracted = forecast,
                                 measure = measurements,
                                 spot_price = elspot_price_example,
                                 reg_down = regulating_prices_down_example,
                                 reg_up = regulating_prices_up_example)
  results = list(hourly_revenue = revenue_example,
                 gamma_perf = gamma_perf)
  return(results)
}

###################################
###################################

# Date of study : 28th of March 2016 / 20160328
daily_study(20160328)
daily_study(20170328)
