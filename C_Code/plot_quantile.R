## Assignment 2 ## 
## Renewables in electricity market ## 
## Author : Florian Guillebeaud ## 
###################################
###################################

# plot quantile 

plot_quantile = function(wp_next_day, date_bidded, contracted, i){
  
  quantile_95 = as.numeric(matrix(wp_next_day$quantile.q95,1,24))/10^3
  quantile_5 = as.numeric(matrix(wp_next_day$quantile.q5,1,24))/10^3
  meas_temp = as.numeric(matrix(wp_next_day$meas,1,24))/10^3
  fore_temp = as.numeric(matrix(wp_next_day$fore,1,24))/10^3
  
  y_lim = c(min(fore_temp, meas_temp, quantile_5, quantile_95, na.rm = TRUE), max(fore_temp, meas_temp, quantile_5, quantile_95, na.rm = TRUE))
  plot(quantile_95, type = "l", lwd = 2, col = "red",
       xlab = "Program Time Unit [h]", ylab = "Power production [MW]", 
       ylim = y_lim)
  lines(quantile_5, type = "l", lwd = 2, col = "red")
  polygon(c(1:24,rev(1:24)), c(quantile_5, 
                               rev(quantile_95)), 
          col = "grey")
  lines(meas_temp, type = "o", lwd = 2, col = "black")
  lines(fore_temp, type = "l", lwd = 2, col = "blue")
  lines(contracted, type = "l", lwd = 2, col = "green")
  legend("bottomright", legend = c("Forecasts","Measures", "Best Quantile contracted","Range of quantiles"), lty = 1, 
         lwd = c(2,2,2,10), col = c("blue", "black", "green","grey"))  
  title(main = paste0("Selected day : ", date_bidded))
  
}
  