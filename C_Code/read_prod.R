## Assignment 2 ## 
## Renewables in electricity market ## 
## Author : Florian Guillebeaud ## 

###################################
###################################

# read elspot prices in DK1 in 2016 and 2017 from NordPool historical data

###################################
###################################



library(readxl)

#### 2016 ####
dataset <- read_excel("D_Data/prod_DK_2016.xlsx", skip = 2)
prod_DK1_16 = dataset[,-(4:5)]

# change the date "2016-MM-DD" into "2016MMDDi" where i is the hour
a = noquote(paste0(prod_DK1_16$Hours[1:(length(prod_DK1_16$Hours)-1)]))
a = gsub(a,pattern = "-", replacement = "")
a = gsub(a,pattern = " ", replacement = "")
a = sub("^(\\d{2}).*$", "\\1", a)

b = noquote(paste0(prod_DK1_16$X__1[1:(length(prod_DK1_16$X__1)-1)]))
b_daily = gsub(b,pattern = "-", replacement = "")

a_hourly = as.numeric(paste0(b_daily[1:length(b_daily)],a[1:length(a)]))
a_daily = sub("^(\\d{8}).*$", "\\1", a_hourly)
a_monthly = sub("^(\\d{6}).*$", "\\1", a_daily)

# return it in a data frame
prod_2016 = data.frame(date_monthly = a_monthly, date_daily = a_daily, 
                         date_hourly = a_hourly, DK1 = prod_DK1_16$DK1[1:(length(prod_DK1_16$X__1)-1)])

#### 2017 #### 
dataset <- read_excel("D_Data/prod_DK_2017.xlsx", skip = 2)
prod_DK1_17 = dataset[,-(4:5)]

# change the date "2016-MM-DD" into "2016MMDDi" where i is the hour
a = noquote(paste0(prod_DK1_17$Hours[1:(length(prod_DK1_17$Hours)-1)]))
a = gsub(a,pattern = "-", replacement = "")
a = gsub(a,pattern = " ", replacement = "")
a = sub("^(\\d{2}).*$", "\\1", a)

b = noquote(paste0(prod_DK1_17$X__1[1:(length(prod_DK1_17$X__1)-1)]))
b_daily = gsub(b,pattern = "-", replacement = "")

a_hourly = as.numeric(paste0(b_daily[1:length(b_daily)],a[1:length(a)]))
a_daily = sub("^(\\d{8}).*$", "\\1", a_hourly)
a_monthly = sub("^(\\d{6}).*$", "\\1", a_daily)

# return it in a data frame
prod_2017 = data.frame(date_monthly = a_monthly, date_daily = a_daily, 
                       date_hourly = a_hourly, DK1 = prod_DK1_17$DK1[1:(length(prod_DK1_17$X__1)-1)])

