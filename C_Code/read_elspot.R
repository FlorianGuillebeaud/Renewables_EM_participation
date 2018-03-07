# read elspot prices in DK1 in 2016 and 2017
library(readxl)

source("C_Code/calendar.R")

elspot_price_16 = read_excel("D_Data/elspot_price_16.xlsx", skip = 2)
# change the date "2016-MM-DD" into "2016MMDDi" where i is the hour
a = noquote(paste0(elspot_price_16$X__1[1:(length(elspot_price_16$X__1)-1)]))
a_daily = gsub(a,pattern = "-", replacement = "")
a_monthly = sub("^(\\d{6}).*$", "\\1", a_daily)
a_hourly = paste0(a_daily,year_2016)
# return it in a data frame
elspot_price_16 = data.frame(date_monthly = a_monthly, date_daiy = a_daily, date_hourly = a_hourly, DK1 = elspot_price_16$DK1[1:(length(elspot_price_16$X__1)-1)])

elspot_price_17 = read_excel("D_Data/elspot_price_17.xlsx", skip = 2)
# change the date "2016-MM-DD" into "2016MMDDi" where i is the hour
b = noquote(paste0(elspot_price_17$X__1[1:(length(elspot_price_17$X__1)-1)]))
b_daily = gsub(b,pattern = "-", replacement = "")
b_monthly = sub("^(\\d{6}).*$", "\\1", b_daily)
b_hourly = paste0(b_daily,year_2017)
# return it in a data frame
elspot_price_17 = data.frame(date_monthly = b_monthly, date_daiy = b_daily, date_hourly = b_hourly, DK1 = elspot_price_17$DK1[1:(length(elspot_price_17$X__1)-1)])
