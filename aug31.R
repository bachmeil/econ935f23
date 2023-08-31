library(tstools)
rgdp <- import.fred("rgdp.csv")
drgdp <- pctChange(rgdp)
arma11 <- arima(drgdp, order=c(1,0,1))
arma11
mean(drgdp)
last(residuals(arma11))
last(drgdp)
# Predict deviation from the mean
# time T+1 using the estimate
0.4648*(0.005946259-0.007465561) -
  0.3429*(-0.001105497)
# Now calculate prediction
-0.0003270966 + 0.007465561
# T+2: Iterate on the model
0.4648*(-0.0003270966) -
  0.3429*(0)
# Deviation from the mean at T+2
# Add mean in
-0.0001520345 + 0.007465561
predict(arma11, n.ahead=2)
# Same as we calculated

inf <- import.fred("inflation.csv")
plot(inf)
arma.inf <- arima(inf, order=c(1,0,1))
inf.error <- residuals(arma.inf)
plot(inf.error)
errsq <- inf.error^2
fit <- tsreg(errsq, lags(errsq,1))
fit
summary(fit)
# Use $fitted to keep the ts properties
plot(fit$fitted)
sum(fit$fitted < 0)
fit <- tsreg(errsq, lags(errsq,1:3))
summary(fit)
900*0.1004
# Error in the lecture, I should have typed pchisq
# not dchisq to find the p-value
pchisq(90.36, 3) # Probability less than 90.36
1-pchisq(90.36, 3) # p-value
