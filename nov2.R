utility <- function(x) {
  x^0.5 * (10-x)^0.5
}
utility(8)
utility(12)
curve(utility, from=-10, to=20)
# Grid search
# Full range of all feasible values
# Here: both x and y are positive
# Try all points: impossible
# Choose fine enough grid
# If you get the solution, you know it's
# close enough
x.values <- seq(0.01, 9.99, by=0.01)
x.values
u.values <- utility(x.values)
u.values
which.max(u.values)
x.values[500]
# x=5, y=5
# Advantage: easy
# Always works when it works
# Curse of dimensionality
# Doesn't work in practice
# Take educated guesses of the parameters
# Reduce dimension of values we try
# x=1, 3, 5
# Algorithms tell you the guess
# Determine when you are "close enough"
optimize(utility, c(0,10), maximum=TRUE)
# Four goods
# Price of all goods is one
# Still have 10 units to spend
# New objective function
utility <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  x3 <- x[3]
  x4 <- 10 - x1 - x2 - x3
  return(-(x1^0.5*x2^0.5*x3^0.5*x4^0.5))
}
utility(c(2,3,4))
utility(c(3,4,5))
optim(c(1,2,3), utility)
# Important: convergence=0
optim(c(1,2,3), utility, 
      control=list(maxit=10000))
# Could try several other points in that region

# Solve system of nonlinear equations
obj.fun <- function(par) {
  x <- par[1]
  y <- par[2]
  dev1 <- x+y^2-6
  dev2 <- x^2+y-4
  return(dev1^2+dev2^2)
}
obj.fun(c(1,1))
optim(c(1,1), obj.fun)
# x=1.36, y=2.15 is the solution
# Check it
1.36 + 2.15^2
1.36^2 + 2.15

# nleqslv
# Modify the objective function
obj.fun2 <- function(par) {
  x <- par[1]
  y <- par[2]
  dev1 <- x+y^2-6
  dev2 <- x^2+y-4
  return(c(dev1, dev2))
}

library(nleqslv)
nleqslv(c(1,1), obj.fun2)

# Estimate the RF VAR model
library(tstools)
u <- import.fred("unrate.csv")
inf <- import.fred("inflation.csv")
dataset <- window(ts.combine(u, inf),
                  end=c(2019,12))
fit.u <- tsreg(u, lags(dataset,1))
fit.u
fit.inf <- tsreg(inf, lags(dataset,1))
fit.inf
res1 <- fit.u$resids
res2 <- fit.inf$resids
res1
res2
s2.1 <- var(res1)
s2.2 <- var(res2)
s12 <- cov(res1,res2)
s2.1
s2.2

# b=0
objfnc.optim <- function(par) {
  b <- 0
  c <- par[1]
  su <- par[2]
  sinf <- par[3]
  dev1 <- s2.1 - su - b^2*sinf
  dev2 <- s2.2 - c^2*su - sinf
  dev3 <- s12 - c*su - b*sinf
  return(dev1^2 + dev2^2 + dev3^2)
}
objfnc.optim(c(0, 0.1, 0.1))  
optim(c(0, 0.1, 0.1), objfnc.optim)  
# c = -0.32
# Positive variances

# Nonlinear equation solver
# c=0
objfnc.nleqslv <- function(par) {
  b <- par[1]
  c <- 0
  su <- par[2]
  sinf <- par[3]
  dev1 <- s2.1 - su - b^2*sinf
  dev2 <- s2.2 - c^2*su - sinf
  dev3 <- s12 - c*su - b*sinf
  return(c(dev1, dev2, dev3))
}
nleqslv(c(0,0.1,0.1), objfnc.nleqslv)
# b = -0.07

# Constraint on the variances
objfnc.nleqslv2 <- function(par) {
  b <- par[1]
  c <- par[2]
  su <- par[3]
  sinf <- par[3]
  dev1 <- s2.1 - su - b^2*sinf
  dev2 <- s2.2 - c^2*su - sinf
  dev3 <- s12 - c*su - b*sinf
  return(c(dev1, dev2, dev3))
}
nleqslv(c(0,0.1,0.1), objfnc.nleqslv2)

# Overidentified model
# Use GMM
objfnc.oid <- function(par, data) {
  c <- par[1]
  su <- par[2]
  dev1 <- data[,"res1"]^2 - su
  dev2 <- data[,"res2"]^2 - (c^2+1)*su
  dev3 <- data[,"res1"]*data[,"res2"] - c*su
  return(cbind(dev1, dev2, dev3))
}
res <- ts.combine(res1,res2)
objfnc.oid(c(-0.1, 0.1), res)
library(gmm)
gmm(objfnc.oid, res, t0=c(-0.1,0.1))
gmmoutput <- gmm(objfnc.oid, res, t0=c(-0.1,0.1))
specTest(gmmoutput)
  