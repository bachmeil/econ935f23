library(tstools)
rgdp <- import.fred("rgdp.csv")
drgdp <- pctChange(rgdp)

# Calculate the AIC for MA(q) model
# This code is the same as before
ma.criteria <- function(q) {
  fit <- arima(drgdp, order=c(0,0,q))
  list(lags=q, aic=AIC(fit))
}
aic.values <- lapply(1:12, ma.criteria)
aic.values

minaic <- list(aic=Inf)
for(x in aic.values) {
  if (x$aic < minaic$aic) {minaic <- x}
}
minaic

# Now do the same, but for ARMA models
# Problem: Need to send two lag lengths, not just one, so a scalar doesn't work
# Create all combinations
# Each row is a (p, q) pair for the ARMA(p,q)
possible.lags <- expand.grid(0:2,0:2)
possible.lags
colnames(possible.lags) <- c("p", "q")
possible.lags

# Modify the function to work with two lag lengths
# Strategy: Send the row number as `index` rather
# than the lag lengths themselves
arma.criteria <- function(index) {
  p <- possible.lags[index,1]
  q <- possible.lags[index,2]
  fit <- arima(drgdp, order=c(p,0,q))
  list(lags=c(p=p, q=q), aic=AIC(fit))
}

# Apply this for all of the rows of possible.lags
aic.values <- lapply(1:nrow(possible.lags),
                     arma.criteria)
aic.values
minaic <- list(aic=Inf)
for(x in aic.values) {
  if (x$aic < minaic$aic) {minaic <- x}
}
minaic

# 1 million draws from std normal
x <- rnorm(1000000)
plot(density(x))
var(x)
mean(x)
?arima.sim
xsim <- arima.sim(n=500, list(ar=0.4))
plot(xsim)

# Use set.seed to guarantee replicability
# Will always get the same set of random numbers
# If we set the same seed
set.seed(200)
y <- rnorm(400)
x <- rnorm(400)
cor(y, x)

set.seed(200)
# replicate repeats the code in brackets 1000 times
# Saves the last item evaluated each time
simoutput <- replicate(1000, {
  y <- rnorm(400)
  x <- rnorm(400)
  cor(y, x)
})
simoutput
plot(density(simoutput))
mean(abs(simoutput) > 0.1)
mean(abs(simoutput) > 0.2)
max(abs(simoutput))

set.seed(200)
# Now check the correlation with extreme persistence
y <- arima.sim(list(ar=0.999), n=400)
x <- arima.sim(list(ar=0.999), n=400)
cor(y,x)

set.seed(200)
simoutput <- replicate(1000, {
  y <- arima.sim(list(ar=0.999), n=400)
  x <- arima.sim(list(ar=0.999), n=400)
  cor(y, x)
})
plot(density(simoutput))
mean(abs(simoutput) > 0.1)
mean(abs(simoutput) > 0.2)
max(abs(simoutput))
# What's going on?