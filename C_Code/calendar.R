# Create a data frame for the calendar 2016 and 2017 

day = seq(1:24)

jan = rep(day,31)
feb_16 = rep(day,29)
feb_17 = rep(day,28)
mar = rep(day,31)
apr = rep(day,30)
may = rep(day,31)
jun = rep(day,30)
jul = rep(day,31)
aug = rep(day,31)
sep = rep(day,30)
oct = rep(day,31)
nov = rep(day,30)
dec = rep(day,31)

year_2016 = c(jan,feb_16,mar,apr,may,jun,jul,aug,sep,oct,nov,dec)
year_2017 = c(jan,feb_17,mar,apr,may,jun,jul,aug,sep,oct,nov,dec)
