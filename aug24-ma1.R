library(tstools)

# This example lets you pass the data
# and initial value of beta to the function
# and estimate an MA(1) model with no
# intercept and variance equal to 1
ma1.estimate <- function(data, beta.init) {
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
  
  optim(beta.init, ll)
}
rgdp <- import.fred("rgdp.csv")
drgdp <- pctChange(rgdp)
ma1.estimate(drgdp, 0.5)
# Estimate is 0.25

# Now add an intercept
ma1.estimate2 <- function(data, alpha.init, beta.init) {
  ll <- function(theta) {
    compute.epsilon <- function(y, eps) {
      if (length(y) > 0) {
        next.epsilon <- y[1] - theta[1] - theta[2]*last(eps)
        compute.epsilon(y[-1], c(eps, next.epsilon))
      } else {
        return(eps[-1])
      }
    }
    eps <- compute.epsilon(data, 0)
    return(-sum(dnorm(eps, log=TRUE)))
  }
  
  optim(c(alpha.init, beta.init), ll)
}
ma1.estimate2(drgdp, 0.0, 0.5)
# alpha = 0.007
# beta = 0.09

# Including an intercept and estimated sd
ma1.estimate3 <- function(data, alpha.init, beta.init, sd.init) {
  ll <- function(theta) {
    compute.epsilon <- function(y, eps) {
      if (length(y) > 0) {
        next.epsilon <- y[1] - theta[1] - theta[2]*last(eps)
        compute.epsilon(y[-1], c(eps, next.epsilon))
      } else {
        return(eps[-1])
      }
    }
    eps <- compute.epsilon(data, 0)
    return(-sum(dnorm(eps, sd=theta[3], log=TRUE)))
  }
  
  optim(c(alpha.init, beta.init, sd.init), ll)
}
ma1.estimate3(drgdp, 0.0, 0.5, 1.0)
# alpha = 0.007
# beta = 0.09
# sd = 0.011

# The last gives similar results to arima
# Note that arima is using a different algorithm
# So it is not expected to give exactly the same results
arima(drgdp, order=c(0,0,1))
