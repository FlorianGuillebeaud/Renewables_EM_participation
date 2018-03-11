## Assignment 2 ## 
## Renewables in electricity market ## 
## Author : Florian Guillebeaud ## 

# read elspot prices in DK1 in 2016 and 2017
library(readxl)

elspot_price_2016 = read_excel("D_Data/elspot_price_16.xlsx", skip = 2)
# change the date "2016-MM-DD" into "2016MMDDi" where i is the hour
a = noquote(paste0(elspot_price_2016$X__1[1:(length(elspot_price_2016$X__1))]))
a_daily = gsub(a,pattern = "-", replacement = "")
a_daily = a_daily[-7297]
hour = vector()
for (i in 1:length(a_daily)){
  hour[i] = i%%24
}
a_hourly = paste0(a_daily,hour)
a_monthly = sub("^(\\d{6}).*$", "\\1", a_daily)

new_elspot_price_2016 = elspot_price_2016$DK1[-7297]
# return it in a data frame
elspot_price_2016 = data.frame(date_monthly = a_monthly, date_daily = a_daily, date_hourly = a_hourly, DK1 = elspot_price_2016$DK1[1:(length(elspot_price_2016$X__1)-1)])






elspot_price_2017 = read_excel("D_Data/elspot_price_17.xlsx", skip = 2)
# change the date "2016-MM-DD" into "2016MMDDi" where i is the hour
b = noquote(paste0(elspot_price_2017$X__1[1:(length(elspot_price_2017$X__1))]))
b_daily = gsub(b,pattern = "-", replacement = "")
b_daily = b_daily[-7249]
hour = vector()
for (i in 1:length(b_daily)){
  hour[i] = i%%24
}
b_hourly = paste0(b_daily, hour)
b_monthly = sub("^(\\d{6}).*$", "\\1", b_daily)

new_elspot_price_2017 = elspot_price_2017$DK1[-7249]
# return it in a data frame
elspot_price_2017 = data.frame(date_monthly = b_monthly, date_daily = b_daily, date_hourly = b_hourly, DK1 = new_elspot_price_2017)
