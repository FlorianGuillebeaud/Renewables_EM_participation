# Read regulating prices
source("C_Code/calendar.R")
library(readxl)

regulating_prices_2016 <- read_excel("D_Data/regulating_prices_2016.xlsx", skip = 3)
# change the date "2016-MM-DD" into "2016MMDDi" where i is the hour
a = noquote(paste0(regulating_prices_2016$X__1[1:(length(regulating_prices_2016$X__1)-1)]))
a_daily = gsub(a,pattern = "-", replacement = "")
a_monthly = sub("^(\\d{6}).*$", "\\1", a_daily)
a_hourly = paste0(a_daily,year_2016)
# return it in a data frame
regulating_prices_2016 = data.frame(date_monthly = a_monthly, date_daily = a_daily, date_hourly = a_hourly, DK1_UP = regulating_prices_2016$Up__10[1:(length(regulating_prices_2016$Up__10)-1)],
                                              DK1_DOWN = regulating_prices_2016$Down__10[1:(length(regulating_prices_2016$Down__10)-1)])


regulating_prices_2017 <- read_excel("D_Data/regulating_prices_2017.xlsx", skip = 3)
# change the date "2016-MM-DD" into "2016MMDDi" where i is the hour
b = noquote(paste0(regulating_prices_2017$X__1[1:(length(regulating_prices_2017$X__1)-1)]))
b_daily = gsub(b,pattern = "-", replacement = "")
b_monthly = sub("^(\\d{6}).*$", "\\1", b_daily)
b_hourly = paste0(b_daily,year_2017)
# return it in a data frame
regulating_prices_2017 = data.frame(date_monthly = b_monthly, date_daily = b, date_hourly = b_hourly, DK1_UP = regulating_prices_2017$Up__10[1:(length(regulating_prices_2017$Up__10)-1)],
                                    DK1_DOWN = regulating_prices_2017$Down__10[1:(length(regulating_prices_2017$Down__10)-1)])

