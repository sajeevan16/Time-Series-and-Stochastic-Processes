setwd("F:/Semister_8/Time Series/Project")

library(TSA)
library(tseries)
source('LjungBoxPlot.R')
source('t_stat.R')

ts <- read.ts('ts1.txt')

adf.test(ts)

plot.ts(ts, ylab='ts',xlab='Time',type='o')
abline(mean(ts), 0, col='red', lwd=2)

acf(ts)
pacf(ts)
eacf(ts)

p <- 1
d <- 0
q <- 3

model_fit <- arima(ts, c(p,d,q))
model_fit
t_stat(model_fit)

AIC(model_fit)
BIC(model_fit)

res_model_fit <- residuals(model_fit)

plot(res_model_fit, type='o')
plot(rstandard(model_fit),ylab='Standardized Residuals',type='o')

acf(res_model_fit, na.action = na.remove)
pacf(res_model_fit, na.action = na.remove)

qqnorm(res_model_fit)
qqline(res_model_fit)

shapiro.test(res_model_fit)

LjungBoxPlot(res_model_fit,50,5)

p <- 0
d <- 0
q <- 3

model_fit <- arima(ts, c(p,d,q))
model_fit
t_stat(model_fit)

AIC (model_fit)
BIC(model_fit)

res_model_fit <- residuals(model_fit)

plot(res_model_fit, type='o')
plot(rstandard(model_fit),ylab='Standardized Residuals',type='o')

acf(res_model_fit, na.action = na.remove)
pacf(res_model_fit, na.action = na.remove)

qqnorm(res_model_fit)
qqline(res_model_fit)

shapiro.test(res_model_fit)

LjungBoxPlot(res_model_fit,50,5)

p <- 0
d <- 0
q <- 2

model_fit <- arima(ts, c(p,d,q))
model_fit
t_stat(model_fit)

AIC (model_fit)
BIC(model_fit)

res_model_fit <- residuals(model_fit)

plot(res_model_fit, type='o')
plot(rstandard(model_fit),ylab='Standardized Residuals',type='o')

acf(res_model_fit, na.action = na.remove)
pacf(res_model_fit, na.action = na.remove)

qqnorm(res_model_fit)
qqline(res_model_fit)

shapiro.test(res_model_fit)

LjungBoxPlot(res_model_fit,50,5)

