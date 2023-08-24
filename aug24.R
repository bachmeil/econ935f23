library(tstools)
beta <- 0.4
compute.epsilon <- function(y, eps) {
  # Compute the next epsilon
  next.epsilon <- y[1] - beta*last(eps)
  compute.epsilon(y[-1], c(eps, next.epsilon))
}
# y[-1]: Drop first value of y
# c(eps, next.epsilon): New vector with
# existing eps plus new calculation
compute.epsilon(c(0.3, 0.1, -0.2, -0.8), 0)

compute.epsilon <- function(y, eps) {
  if (length(y) > 0) {
    next.epsilon <- y[1] - beta*last(eps)
    compute.epsilon(y[-1], c(eps, next.epsilon))
  } else {
    return(eps[-1])
  }
}
compute.epsilon(c(0.3, 0.1, -0.2, -0.8), 0)
# Evaluate the log likelihood
eps <- compute.epsilon(c(0.3, 0.1, -0.2, -0.8), 0)
sum(dnorm(eps, log=TRUE))
eps
dnorm(eps)
dnorm(eps, log=TRUE)
log(dnorm(eps))

## Objective function
# data defined outside the function
ll <- function(beta) {
  compute.epsilon <- function(y, eps) {
    if (length(y) > 0) {
      next.epsilon <- y[1] - beta*last(eps)
      compute.epsilon(y[-1], c(eps, next.epsilon))
    } else {
      return(eps[-1])
    }
  }
  eps <- compute.epsilon(data, 0)
  return(-sum(dnorm(eps, log=TRUE)))
}
rgdp <- import.fred("rgdp.csv")
plot(rgdp)
drgdp <- pctChange(rgdp)
plot(drgdp)  
ma1 <- arima(drgdp, order=c(0,0,1)) 
AIC(ma1)  
BIC(ma1)
ma1
ma2 <- arima(drgdp, order=c(0,0,2)) 
AIC(ma2)
BIC(ma2)
predict(ma2, n.ahead=4)
