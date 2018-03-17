# plot quantile 

dati_temp = matrix(noquote(unique(data_wp$dati)), nrow = length(unique(data_wp$dati)), ncol =1 )
dati_to_consider <- dati_temp[seq(1, length(dati_temp), 2)] # when the forecast is issued 

###################################
###################################
# choose a day in 1:366
i = 1

  # Find the index from where extract the wind data for each forecasted day to be considered
  temp_dati = data_wp[data_wp$dati==dati_to_consider[i] & data_wp$hors==13,]
  index_next_day = as.numeric(matrix(noquote((row(temp_dati, TRUE)[1])),1,1))
  
  # assign the wind predictions for next day
  wp_next_day = data_wp[(index_next_day-1):(index_next_day+22),]
  
  # remember the date bidded (the following)
  date_bidded = as.numeric(matrix(noquote(wp_next_day$date_daily[1])),1,1)
  
  plot(as.numeric(matrix(wp_next_day$quantile.q95,1,24))/10^3, type = "l", lwd = 2, col = "red",
     xlab = "Program Time Unit [h]", ylab = "Power production [MW]", ylim = c(10,190))
  lines(as.numeric(matrix(wp_next_day$quantile.q5,1,24))/10^3, type = "l", lwd = 2, col = "red")
  polygon(c(1:24,rev(1:24)), c(as.numeric(matrix(wp_next_day$quantile.q5,1,24))/10^3, 
                               rev(as.numeric(matrix(wp_next_day$quantile.q95,1,24))/10^3)), 
          col = "grey")
  lines(as.numeric(matrix(wp_next_day$meas,1,24))/10^3, type = "o", lwd = 2, col = "black")
  lines(as.numeric(matrix(wp_next_day$fore,1,24))/10^3, type = "l", lwd = 2, col = "blue")
  legend("topright", legend = c("Forecasts","Measures","Range of quantiles"), lty = 1, 
         lwd = c(2,2,20), col = c("blue", "black", "grey"))  
  title(main = paste0("Selected day : ", date_bidded))
  