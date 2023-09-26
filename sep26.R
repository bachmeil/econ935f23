library(tstools)
u <- import.fred("unrate.csv")
inf <- import.fred("inflation.csv")
dataset <- ts.combine(u, inf)
# Estimate RF VAR model
library(vars)
varfit <- VAR(dataset, lag.max=13,
              ic="SC")
causality(varfit, cause="u")
causality(varfit, cause="inf")


peso <- import.fred("peso.csv")
last(peso)
plot(peso)
actuals <- window(peso, 
                  start=c(2013,9),
                  end=c(2023,8))
plot(actuals)
rw <- window(peso, start=c(2013,8),
             end=c(2023,7))
# Fix the dates to be the date
# we're forecasting
rw.fcst <- ts(rw, start=c(2013,9),
              frequency=12)
rw.error <- actuals - rw.fcst
plot(rw.error)
rw.mse <- mean(rw.error^2)
rw.mse
# Don't do this!
# mean(rw.error)^2
mean(window(peso, end=c(2013,8)))
mean.error <- actuals - 10.04
mse.mean <- mean(mean.error^2)
mse.mean
# Let's do inference
# Loss differential series
d <- rw.error^2 - mean.error^2
mean(d)
dmfit <- lm(d ~ 1)
nw.correction(dmfit)

# Define the last estimation dates
# for the forecasts
dates <- make.dates(c(2009,12), c(2023,6),
                    12)
dates
# d is the last date for the sample used
# in estimation
varfcst <- function(d) {
  ds <- window(dataset, end=d)
  fit <- VAR(ds, lag.max=6, ic="SC")
  getVarForecast(fit, "inf", 1)
}
varfcst(c(2000,6))
tsobs(inf, c(2000,7))
fcst.var <- sapply(dates, varfcst)
fcst.var

arfcst <- function(d) {
  ds <- window(dataset, end=d)
  fit <- arima(ds[,"inf"], order=c(4,0,0))
  pred <- predict(fit, 1)
  pred$pred
}
arfcst(c(2000,6))
fcst.ar <- sapply(dates, arfcst)
fcst.ar

actuals <- window(inf, start=c(2010,1),
                  end=c(2023,7))

e.pc <- actuals - fcst.var
e.ar <- actuals - fcst.ar
num <- sum(e.ar*e.ar - e.ar*e.pc)
den <- sum(e.pc^2)
enc.new <- 163*num/den
enc.new
# Phillips curve is forecasting worse
dim(dataset)
163/704

