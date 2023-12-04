library(tstools)
u <- import.fred("unrate.csv")
inf <- import.fred("inflation.csv")
fit.linear <- tsreg(inf, lags(u,1))
fit.linear

# Dummy variable for high unemployment
# regime
dum <- lags(u,1) > 6.0
# Interact with unemployment rate
inter <- dum*lags(u,1)
plot(inter)
fit.nonlinear <- tsreg(inf,
                       lags(u,1) %~% inter)
fit.nonlinear
# High unemployment slope
# About zero
-0.07663 + 0.07941
nw.correction(fit.nonlinear)
# Can't reject linear model
# No evidence for PC
# We want to forecast
# What about the AIC?
AIC(fit.nonlinear)
AIC(fit.linear)
AIC(tsreg(inf, 1))
# According to AIC, nonlinear PC is
# best forecasting model

x <- abs(rt(10000, 3))
plot(density(x))
y <- runif(10000, 0.4, 0.6)
plot(density(y))
z <- rnorm(10000, sd=2)
plot(density(z))
f <- x^y/sin(z)
f
mean(f)


inter6 <- (lags(u,6) > 6)*lags(u, 6)
tsreg(inf, lags(u,6) %~% inter6)
