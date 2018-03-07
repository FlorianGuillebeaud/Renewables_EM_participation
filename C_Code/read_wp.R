# Read wind power forecast file
data_wp = read.table("D_Data/windpowerforecasts.dat", skip = 0, sep = ";")

# Rename the column with the 10th quantile
data_wp[1,7] = "q10" 

# shift 2017 hours in first position
data_wp[2:27750,] = data_wp[14:27762,]

# Write and read only the hours in 2017
write.table(data_wp[1:22750,], file = "D_Data/renamed_wp_file.dat")
data_wp = read.table("D_Data/renamed_wp_file.dat", header = TRUE, skip = 1)
wind_power_forecast_2017 = data.frame( data_wp)

# Plots
plot(1:length(wind_power_forecast_2017[,1]), wind_power_forecast_2017$fore, type = "l", xlab = "Time [h]", ylab = "[kW]")
title(main = "Wind power forecast in 2017")

plot(1:length(wind_power_forecast_2017[,1]), wind_power_forecast_2017$meas, type = "l", xlab = "Time [h]", ylab = "[kW]")
title(main = "Wind power measurements in 2017")

plot(1:24, wind_power_forecast_2017$fore[1:24], type = "l", xlab = "First day prediction", ylab = "[kW]")
lines(1:24, wind_power_forecast_2017$meas[1:24], col = "blue")
legend("topright", legend = c("forecasted", "measured"), col = c("black", "blue"), lty = 1)
title(main = "Wind power production")
