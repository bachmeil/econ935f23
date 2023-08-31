library(tstools)
rgdp <- import.fred("rgdp.csv")
drgdp <- pctChange(rgdp)
last(rgdp)
ma1 <- arima(drgdp, order=c(0,0,1))
AIC(ma1)
ma2 <- arima(drgdp, order=c(0,0,2))
AIC(ma2)

ma.criteria <- function(q) {
  fit <- arima(drgdp, order=c(0,0,q))
  list(lags=q, aic=AIC(fit))
}
ma.criteria(1)
ma.criteria(2)
aic.values <- lapply(1:12, ma.criteria)
aic.values

minaic <- list(aic=Inf)
for(x in aic.values) {
  if (x$aic < minaic$aic) {minaic <- x}
}

ar1 <- tsreg(drgdp, lags(drgdp,1))
ar1
ar2 <- tsreg(drgdp, lags(drgdp,1:2))
ar2
AIC(ar1)
AIC(ar2)

minaic
aic.values

last(residuals(ma1))
arma32 <- arima(drgdp, order=c(3,0,2))
AIC(arma32)
AIC(ma1)
AIC(ar1)
