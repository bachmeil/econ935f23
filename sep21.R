library(tstools)
u <- import.fred("unrate.csv")
plot(u)
inf <- import.fred("inflation.csv")
plot(inf)
pctChange(u,12)
# Estimate the VAR equations
lags(u,1:2) %~% lags(inf,1:2)
fit.u <- tsreg(u, lags(u,1:2) %~% lags(inf,1:2))
fit.u
fit.inf <- tsreg(inf, lags(u,1:2) %~% lags(inf,1:2))
fit.inf
# vars package: workhorse for basic VAR models
# Create dataset (but no lags)
dataset <- ts.combine(u, inf)
library(vars)
varfit <- VAR(dataset, p=2)
varfit
# Let vars package do the selection
varfit <- VAR(dataset, lag.max=13, ic="SC")
varfit
varfit <- VAR(dataset, lag.max=13, ic="AIC")
varfit
?VARselect
VARselect(dataset, lag.max=6)
# Predict both variables one step ahead
pred <- predict(varfit, n.ahead=1)

pred
pred$inf
pred$fcst$inf
pred$fcst$inf[1]
# 1-step forecast of variable 2
getVarForecast(varfit, 2, n=1)
getVarForecast(varfit, "inf", n=1)
# 12-step forecast of inflation
getVarForecast(varfit, "inf", n=12)
# Forecasts steps 1-12
getVarForecasts(varfit, "inf", n=1:12)
# Create time series
last(inf)
# Forecasts steps 1-12 as time series
getVarForecasts(varfit, "inf", n=1:12,
                start=c(2023,8))
plot(getVarForecasts(varfit, "inf", n=1:12,
                start=c(2023,8)))
# Forecast inf(T+6)
fit.inf <- tsreg(inf, lags(u,6:7) %~%
                   lags(inf,6:7))
last(u,3)
last(inf,2)
# Make the forecast
fit.inf
0.42 - 0.17*3.5 + 0.20*3.6 + 1.77*3.3 -
  0.94*3.09
# Forecast Jan 2024 inflation is 3.48


