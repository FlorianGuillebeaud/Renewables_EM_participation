## Assignment 2 ## 
## Renewables in electricity market ## 
## Author : Florian Guillebeaud ## 
###################################
###################################

# forecast the regulating price # 

###################################
###################################

source("C_Code/read_regulations.R")
source("C_Code/read_elspot.R") # ! price is given in DKK/MWh

###################################
###################################

## TRY ON 2015 ## 
year = c(13,14,15,16)
URC_dw = URC_up = matrix(0, nrow = length(elspot_price_2015$DK1), ncol = length(year))
epsilon = 0.5

# Get rid of the NA - values (might alter the data set)
elspot_price_2013$DK1[is.na(elspot_price_2013$DK1)] <- 0
regulating_prices_2013$DK1_UP[is.na(regulating_prices_2013$DK1_UP)] <- 0
regulating_prices_2013$DK1_DOWN[is.na(regulating_prices_2013$DK1_DOWN)] <- 0

elspot_price_2014$DK1[is.na(elspot_price_2014$DK1)] <- 0
regulating_prices_2014$DK1_UP[is.na(regulating_prices_2014$DK1_UP)] <- 0
regulating_prices_2014$DK1_DOWN[is.na(regulating_prices_2014$DK1_DOWN)] <- 0

elspot_price_2015$DK1[is.na(elspot_price_2015$DK1)] <- 0
regulating_prices_2015$DK1_UP[is.na(regulating_prices_2015$DK1_UP)] <- 0
regulating_prices_2015$DK1_DOWN[is.na(regulating_prices_2015$DK1_DOWN)] <- 0

elspot_price_2016$DK1[is.na(elspot_price_2016$DK1)] <- 0
regulating_prices_2016$DK1_UP[is.na(regulating_prices_2016$DK1_UP)] <- 0
regulating_prices_2016$DK1_DOWN[is.na(regulating_prices_2016$DK1_DOWN)] <- 0


for(k in 1:length(year)){
  for (i in 1:length(eval(parse(text = paste0("elspot_price_20",year[k],"$DK1"))))){
    if(abs(eval(parse(text = paste0("regulating_prices_20",year[k],"$DK1_UP[i]"))) - eval(parse(text = paste0("elspot_price_20",year[k],"$DK1[i]")))) <= epsilon)
    {
      # down regulation 
      # Unit Regulation costs
      URC_dw[i,k] = eval(parse(text = paste0("regulating_prices_20",year[k],"$DK1_DOWN[i]"))) -   eval(parse(text = paste0("elspot_price_20",year[k],"$DK1[i]"))) # should be negative
    }else if(abs(eval(parse(text = paste0("regulating_prices_20",year[k],"$DK1_DOWN[i]"))) - eval(parse(text = paste0("elspot_price_20",year[k],"$DK1[i]")))))
    {
      # up regulation
      # Unit Regulation costs
      URC_up[i,k] = eval(parse(text = paste0("regulating_prices_20",year[k],"$DK1_UP[i]"))) -   eval(parse(text = paste0("elspot_price_20",year[k],"$DK1[i]"))) # should be positive
    }else if((abs(regulating_prices_2015$DK1_DOWN[i] - elspot_price_2015$DK1[i]) <= epsilon) &&
             (abs(regulating_prices_2015$DK1_UP[i] - elspot_price_2015$DK1[i]) <= epsilon))
    {
      # no regulation
      URC_dw[i,k] = 1000 
      URC_up[i,k] = 1000
    }else{
      # up and down regulation 
      URC_dw[i,k] = eval(parse(text = paste0("regulating_prices_20",year[k],"$DK1_DOWN[i]"))) -   eval(parse(text = paste0("elspot_price_20",year[k],"$DK1[i]"))) # should be negative
      URC_up[i,k] = eval(parse(text = paste0("regulating_prices_20",year[k],"$DK1_UP[i]"))) -   eval(parse(text = paste0("elspot_price_20",year[k],"$DK1[i]"))) # should be positive
    }
  }
}

results = cbind(URC_up, URC_dw)





###################################
###################################
hour_dw = hour_up = vector()
for (i in 1:length(URC_dw_temp)){
  hour_dw[i] = i%%24
}

for (i in 1:length(URC_up_temp)){
  hour_up[i] = i%%24
}

URC_dw_temp = rowSums(URC_dw)
URC_dw_temp = cbind(URC_dw_temp, hour_dw)
URC_dw_temp[URC_dw_temp[,2]==0,2] <- 24
URC_up_temp = rowSums(URC_up)
URC_up_temp = cbind(URC_up_temp, hour_up)
URC_up_temp[URC_up_temp[,2]==0,2] <- 24


URC_dw_av = URC_up_av = vector(length = 24)
for (i in 1:24){
  URC_dw_av[i] = sum(URC_dw_temp[URC_dw_temp[,2]==i])/(length(year)*length(URC_dw_temp[,1])/24)
  URC_up_av[i] = sum(URC_up_temp[URC_up_temp[,2]==i])/(length(year)*length(URC_up_temp[,1])/24)
}

plot(URC_dw_av, type = "h", ylim = c(-50,50), lwd = 15, xlab = "Trading period [h]", ylab = "Average penalty", col = "grey")
lines(URC_up_av, type = "h", lwd = 15)
