## Assignment 2 ## 
## Renewables in electricity market ## 
## Author : Florian Guillebeaud ## 
###################################
###################################

# Read regulating prices
library(readxl)

###################################
###################################

regulating_prices_2013 <- read_excel("D_Data/prediction/regulating_prices_2013.xlsx", skip = 3)
# change the date "2016-MM-DD" into "2016MMDDi" where i is the hour
a = noquote(paste0(regulating_prices_2013$X__1[1:(length(regulating_prices_2013$X__1)-1)]))
a_daily = gsub(a,pattern = "-", replacement = "")
hour = vector()
for (i in 1:length(a_daily)){
  hour[i] = i%%24
}
a_hourly = paste0(a_daily,hour)
a_monthly = sub("^(\\d{6}).*$", "\\1", a_daily)
# return it in a data frame
regulating_prices_2013 = data.frame(date_monthly = a_monthly, date_daily = a_daily, date_hourly = a_hourly, DK1_UP = regulating_prices_2013$Up__10[1:(length(regulating_prices_2013$Up__10)-1)],
                                    DK1_DOWN = regulating_prices_2013$Down__10[1:(length(regulating_prices_2013$Down__10)-1)])


for ( i in 1:length(regulating_prices_2013$DK1_UP)){
  if (is.na(regulating_prices_2013$DK1_UP[i])==TRUE) regulating_prices_2013$DK1_UP[i] = regulating_prices_2013$DK1_UP[i-1]
  if (is.na(regulating_prices_2013$DK1_DOWN[i])==TRUE) regulating_prices_2013$DK1_DOWN[i] = regulating_prices_2013$DK1_DOWN[i-1]
}
###################################
###################################

regulating_prices_2014 <- read_excel("D_Data/prediction/regulating_prices_2014.xlsx", skip = 3)
# change the date "2016-MM-DD" into "2016MMDDi" where i is the hour
a = noquote(paste0(regulating_prices_2014$X__1[1:(length(regulating_prices_2014$X__1)-1)]))
a_daily = gsub(a,pattern = "-", replacement = "")
hour = vector()
for (i in 1:length(a_daily)){
  hour[i] = i%%24
}
a_hourly = paste0(a_daily,hour)
a_monthly = sub("^(\\d{6}).*$", "\\1", a_daily)
# return it in a data frame
regulating_prices_2014 = data.frame(date_monthly = a_monthly, date_daily = a_daily, date_hourly = a_hourly, DK1_UP = regulating_prices_2014$Up__10[1:(length(regulating_prices_2014$Up__10)-1)],
                                    DK1_DOWN = regulating_prices_2014$Down__10[1:(length(regulating_prices_2014$Down__10)-1)])


for ( i in 1:length(regulating_prices_2014$DK1_UP)){
  if (is.na(regulating_prices_2014$DK1_UP[i])==TRUE) regulating_prices_2014$DK1_UP[i] = regulating_prices_2014$DK1_UP[i-1]
  if (is.na(regulating_prices_2014$DK1_DOWN[i])==TRUE) regulating_prices_2014$DK1_DOWN[i] = regulating_prices_2014$DK1_DOWN[i-1]
}
###################################
###################################


regulating_prices_2015 <- read_excel("D_Data/prediction/regulating_prices_2015.xlsx", skip = 3)
# change the date "2016-MM-DD" into "2016MMDDi" where i is the hour
a = noquote(paste0(regulating_prices_2015$X__1[1:(length(regulating_prices_2015$X__1)-1)]))
a_daily = gsub(a,pattern = "-", replacement = "")
hour = vector()
for (i in 1:length(a_daily)){
  hour[i] = i%%24
}
a_hourly = paste0(a_daily,hour)
a_monthly = sub("^(\\d{6}).*$", "\\1", a_daily)
# return it in a data frame
regulating_prices_2015 = data.frame(date_monthly = a_monthly, date_daily = a_daily, date_hourly = a_hourly, DK1_UP = regulating_prices_2015$Up__10[1:(length(regulating_prices_2015$Up__10)-1)],
                                    DK1_DOWN = regulating_prices_2015$Down__10[1:(length(regulating_prices_2015$Down__10)-1)])


for ( i in 1:length(regulating_prices_2015$DK1_UP)){
  if (is.na(regulating_prices_2015$DK1_UP[i])==TRUE) regulating_prices_2015$DK1_UP[i] = regulating_prices_2015$DK1_UP[i-1]
  if (is.na(regulating_prices_2015$DK1_DOWN[i])==TRUE) regulating_prices_2015$DK1_DOWN[i] = regulating_prices_2015$DK1_DOWN[i-1]
}