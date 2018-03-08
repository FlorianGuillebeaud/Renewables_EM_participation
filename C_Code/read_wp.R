# Read wind power forecast file
data_wp = read.table("D_Data/windpowerforecasts.dat", skip = 0, sep = ";")

# Rename the column with the 10th quantile / use the first line as colnames / delete first line
data_wp[1,7] = "q10" 
temp_data_wp <- data_wp
temp_data_wp[] <- lapply(data_wp, as.character)
colnames(data_wp) <- temp_data_wp[1, ]
data_wp <- data_wp[-1 ,]
temp_data_wp <- NULL

# Change the format of dato : from YYYYMMDDHH00 to YYYYMMDDHH
data_wp$dato = sub("^(\\d{10}).*$", "\\1", data_wp$dato)
# Change the format of dato : from YYYYMMDDHH00 to YYYYMMDDHH
data_wp$dati = sub("^(\\d{10}).*$", "\\1", data_wp$dati)

date_hourly = data_wp$dato
date_daily = sub("^(\\d{8}).*$", "\\1", date_hourly)
date_monthly = sub("^(\\d{6}).*$", "\\1", date_hourly)
data_wp = data.frame(dato =data_wp$dato,
                     dati = data_wp$dati,
                     date_hourly = date_hourly,
                     date_daily = date_daily,
                     date_monthly = date_monthly,
                     hors = data_wp$hors,
                     meas = data_wp$meas,
                     fore = data_wp$fore,
                     quantile = data_wp[,6:24])

stop("Second classification")
## 
# Group same dato hours together
data_new_wp = data_wp[order(data_wp$dato),]

# get rid of the dato 2016
data_new_wp = data_new_wp[-(1:23),]

# Average of forecasted features
data_ave_wp = matrix(0, nrow = 2, ncol = 24)
# initialisation 
date_ini = "2017010100"
for (i in 1:length(data_new_wp$dato)){
  date_temp = data_new_wp$dato[i]
  
  if (i == 1){
    mat =  matrix(as.numeric(unlist(t(data_new_wp[data_new_wp$dato==date_temp,]))), 
                  ncol  = dim(data_new_wp[data_new_wp$dato==date_temp,])[1],
                  nrow = dim(data_new_wp[data_new_wp$dato==date_temp,])[2] )
    
    # Compute the average of the figures obtained for the number of observations
    ave_mat = t(rowMeans(mat))
    data_ave_wp = rbind(ave_mat)
  }else if (date_temp != date_ini){
    mat = matrix(as.numeric(unlist(t(data_new_wp[data_new_wp$dato==date_temp,]))), 
                 ncol  = dim(data_new_wp[data_new_wp$dato==date_temp,])[1],
                 nrow = dim(data_new_wp[data_new_wp$dato==date_temp,])[2] )
    # Compute the average of the figures obtained for the observations
    ave_mat = t(rowMeans(mat))
    data_ave_wp = rbind(data_ave_wp, ave_mat)
    date_ini = date_temp
    mat <- NULL
    ave_mat <- NULL
  }
}

data_ave_wp = data.frame(data_ave_wp)
colnames(data_ave_wp) <- colnames(data_wp)

date_hourly = data_ave_wp$dato
date_daily = sub("^(\\d{8}).*$", "\\1", date_hourly)
date_monthly = sub("^(\\d{6}).*$", "\\1", date_hourly)
# 
wind_power_2017 = data.frame(date_hourly = date_hourly,
                             date_daily = date_daily,
                             date_monthly = date_monthly,
                             meas = data_ave_wp$meas,
                             fore = data_ave_wp$fore,
                             quantile = data_ave_wp[,6:24]
)

