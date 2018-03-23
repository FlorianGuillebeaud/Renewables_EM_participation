## Assignment 2 ## 
## Renewables in electricity market ## 
## Author : Florian Guillebeaud ## 
###################################
###################################

# read elspot prices in DK1 in 2016 and 2017
library(readxl)

###################################
###################################


elspot_price_2013 = read_excel("D_Data/prediction/elspot_price_13.xlsx", skip = 2)
# change the date "2013-MM-DD" into "2013MMDDi" where i is the hour
a = noquote(paste0(elspot_price_2013$X__1[1:(length(elspot_price_2013$X__1))]))
a_daily = gsub(a,pattern = "-", replacement = "")
a_daily = a_daily[-7297]
hour = vector()
for (i in 1:length(a_daily)){
  hour[i] = i%%24
}
a_hourly = paste0(a_daily,hour)
a_monthly = sub("^(\\d{6}).*$", "\\1", a_daily)

# return it in a data frame
elspot_price_2013 = data.frame(date_monthly = a_monthly, date_daily = a_daily, date_hourly = a_hourly, DK1 = elspot_price_2013$DK1[1:(length(elspot_price_2013$X__1)-1)])


###################################
###################################


elspot_price_2014 = read_excel("D_Data/prediction/elspot_price_14.xlsx", skip = 2)
# change the date "2013-MM-DD" into "2013MMDDi" where i is the hour
a = noquote(paste0(elspot_price_2014$X__1[1:(length(elspot_price_2014$X__1))]))
a_daily = gsub(a,pattern = "-", replacement = "")
a_daily = a_daily[-7297]
hour = vector()
for (i in 1:length(a_daily)){
  hour[i] = i%%24
}
a_hourly = paste0(a_daily,hour)
a_monthly = sub("^(\\d{6}).*$", "\\1", a_daily)

# return it in a data frame
elspot_price_2014 = data.frame(date_monthly = a_monthly, date_daily = a_daily, date_hourly = a_hourly, DK1 = elspot_price_2014$DK1[1:(length(elspot_price_2014$X__1)-1)])


###################################
###################################

elspot_price_2015 = read_excel("D_Data/prediction/elspot_price_15.xlsx", skip = 2)
# change the date "2013-MM-DD" into "2013MMDDi" where i is the hour
a = noquote(paste0(elspot_price_2015$X__1[1:(length(elspot_price_2015$X__1))]))
a_daily = gsub(a,pattern = "-", replacement = "")
a_daily = a_daily[-7297]
hour = vector()
for (i in 1:length(a_daily)){
  hour[i] = i%%24
}
a_hourly = paste0(a_daily,hour)
a_monthly = sub("^(\\d{6}).*$", "\\1", a_daily)

# return it in a data frame
elspot_price_2015 = data.frame(date_monthly = a_monthly, date_daily = a_daily, date_hourly = a_hourly, DK1 = elspot_price_2015$DK1[1:(length(elspot_price_2015$X__1)-1)])

