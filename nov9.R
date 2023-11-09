library(tstools)
u <- import.fred("unrate.csv")
inf <- import.fred("inflation.csv")
data <- ts.combine(u, inf)
rhs <- lags(data, 1)

plot(inf)
# Split regimes in 1994
# After that, inflation shock variance
# should be smaller
fit.u1 <- tsreg(u, rhs, end=c(1994,12))
fit.u1
fit.inf1 <- tsreg(inf, rhs, end=c(1994,12))
fit.inf1
res.u1 <- fit.u1$resids
res.inf1 <- fit.inf1$resids
cov1 <- cov(res.u1 %~% res.inf1)
cov1

fit.u2 <- tsreg(u, rhs, start=c(1995,1))
fit.u2
fit.inf2 <- tsreg(inf, rhs, start=c(1995,1))
fit.inf2
res.u2 <- fit.u2$resids
res.inf2 <- fit.inf2$resids
cov2 <- cov(res.u2 %~% res.inf2)
cov2

# Now use GMM to solve for the six unknowns
# Objective function
dev.nleq <- function(par) {
  b <- par[1]
  c <- par[2]
  su1 <- par[3]
  sinf1 <- par[4]
  su2 <- par[5]
  sinf2 <- par[6]
  dev1 <- 0.05121797 - su1 - (b^2)*sinf1
  dev2 <- 0.21238583 - (c^2)*su1 - sinf1
  dev3 <- -0.01818761 - c*su1 - b*sinf1
  dev4 <- 0.36184342 - su2 - (b^2)*sinf2
  dev5 <- 0.18059166 - (c^2)*su2 - sinf2
  dev6 <- -0.05076644 - c*su2 - b*sinf2
  return(c(dev1, dev2, dev3, dev4, dev5, dev6))
}
library(nleqslv)
nleqslv(c(0,0,0.1,0.1,0.2,0.05), dev.nleq)

# Did the relative shock variances change?
# Regime 1
0.21/0.05
# Regime 2
0.18/0.36
