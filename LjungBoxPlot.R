LjungBoxPlot <- function(residual, max_time_lag, significance_level)
{
  if (significance_level > 1){
    significance_level <- significance_level/100
  }
  
  time_lags = 1:max_time_lag
  p_values = 1:max_time_lag
  
  for (i in time_lags){
    test <- Box.test (residual, lag = i, type = "Ljung")
    p_values[i] = test[["p.value"]]
  }
  
  plot(time_lags, p_values, xlab='Time Lag', ylab='p-value', ylim = c(0,1))
  abline(significance_level, 0,lty=2, col='red', lwd=2)
}