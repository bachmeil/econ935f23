library(urca)
data("Raotbl3")
lc <- ts(Raotbl3$lc, start=c(1966,4),
         frequency=4)
# Diff of natural log is % change
dlc <- diff(lc)
plot(dlc)
library(fGarch)
fit <- garchFit(~ garch(1,1), data=dlc)
summary(fit)
# Fitted values for the mean equation
# Fitted values for dlc
fit@fitted
# Volatility in-sample
fit@h.t
plot(fit@h.t, type="l")
# Predict h
predict(fit, n.ahead=4)
0.0125^2
# Get predicted variances
predict(fit, n.ahead=4)[,"standardDeviation"]^2
# ARCH(3)
fit <- garchFit(~ garch(3,0), data=dlc)
predict(fit, n.ahead=4)[,"standardDeviation"]^2
# Mean: ARMA(2,3), Volatility: GARCH(1,1)
fit <- garchFit(~ arma(2,0) + garch(1,1),
                data = dlc)
predict(fit, n.ahead=4)

