library(tstools)
rgdp <- import.fred("rgdp.csv")
drgdp <- pctChange(rgdp)
inter <- lags(drgdp,1) * (lags(drgdp,1) < 0)
ar1.nonlinear <- tsreg(drgdp, lags(drgdp,1) %~% inter)
ar1.nonlinear
res <- ar1$resids
res
simoutput <- replicate(100, {
  # Initial value
  # Tells us the coefficients
  drgdp.sim <- 0.01
  # Use a for loop in this example
  for (h in 1:4) {
    drgdp.sim <- 0.003192 + 0.429481*drgdp.sim -
      0.796694*drgdp.sim*(drgdp.sim < 0) +
      sample(res, size=1)
  }
  # Last item evaluted is stored by replicate
  drgdp.sim
})
# Average value is the forecast for T+4
# if current value is 0.01
mean(simoutput)



# Redo with a different initial value
# Only one line has changed
simoutput <- replicate(100, {
  # Initial value
  # Tells us the coefficients
  drgdp.sim <- -0.01
  # Use a for loop in this example
  for (h in 1:4) {
    drgdp.sim <- 0.003192 + 0.429481*drgdp.sim -
      0.796694*drgdp.sim*(drgdp.sim < 0) +
      sample(res, size=1)
  }
  # Last item evaluted is stored by replicate
  drgdp.sim
})
# Average value is the forecast for T+4
# if current value is 0.01
mean(simoutput)

