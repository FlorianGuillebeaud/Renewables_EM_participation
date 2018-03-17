# Get distribution 

quantile_distribution = function(wp_next_day){
  q5 = q10 = q15 = q15 = q15 = q15 = q15 = vector() 
  q20 = q25 = q30 = q35 = q40 = q45 = q50 = vector() 
  q55 = q60 = q65 = q70 = q75 = q80 = q85 = q90 = q95  = vector() 
  for (i in 1:length(wp_next_day$dati)){
   q5[i] = as.numeric(matrix(wp_next_day$quantile.q5[i],1,1))
   q10[i] = as.numeric(matrix(wp_next_day$quantile.q10[i],1,1))
   q15[i] = as.numeric(matrix(wp_next_day$quantile.q15[i],1,1))
   q20[i] = as.numeric(matrix(wp_next_day$quantile.q20[i],1,1))
   q25[i] = as.numeric(matrix(wp_next_day$quantile.q25[i],1,1))
   q30[i] = as.numeric(matrix(wp_next_day$quantile.q30[i],1,1))
   q35[i] = as.numeric(matrix(wp_next_day$quantile.q35[i],1,1))
   q40[i] = as.numeric(matrix(wp_next_day$quantile.q40[i],1,1))
   q45[i] = as.numeric(matrix(wp_next_day$quantile.q45[i],1,1))
   q50[i] = as.numeric(matrix(wp_next_day$quantile.q50[i],1,1))
   q55[i] = as.numeric(matrix(wp_next_day$quantile.q55[i],1,1))
   q60[i] = as.numeric(matrix(wp_next_day$quantile.q60[i],1,1))
   q65[i] = as.numeric(matrix(wp_next_day$quantile.q65[i],1,1))
   q70[i] = as.numeric(matrix(wp_next_day$quantile.q70[i],1,1))
   q75[i] = as.numeric(matrix(wp_next_day$quantile.q75[i],1,1))
   q80[i] = as.numeric(matrix(wp_next_day$quantile.q80[i],1,1))
   q85[i] = as.numeric(matrix(wp_next_day$quantile.q85[i],1,1))
   q90[i] = as.numeric(matrix(wp_next_day$quantile.q90[i],1,1))
   q95[i] = as.numeric(matrix(wp_next_day$quantile.q95[i],1,1))
  }
  quantiles = cbind(q5,q10,q15,q20,q25,q30,q35,q40,q45,q50, 
                     q55,q60,q65,q70,q75,q80,q85,q90,q95)
  return(quantiles)
}