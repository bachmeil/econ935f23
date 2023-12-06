# hstep function
# AR(1)
# 6-step ahead forecast
library(tstools)
u <- import.fred("unrate.csv")
h6 <- hstep(u, u, k=1, h=6)
h6$fit
predict(h6)

# AR(3)
h6 <- hstep(u, u, k=3, h=6)

# VAR(2)
inf <- import.fred("inflation.csv")
# h=4
var2 <- hstep(u, u %~% inf, k=2, h=4)
predict(var2)

# VEC(2)
du <- diff(u)
dinf <- diff(inf)
z <- tsreg(u, inf)$resids
vec2 <- hstep(du, du %~% dinf, k=2, h=4,
              ect=z)
predict(vec2)

# Threshold AR
dum <- du > 0
inter <- u * dum
inter
fit <- hstep(u, u %~% inter, k=1, h=5)
predict(fit)


f1 <- function(gamma) {
  1/(1+exp(-gamma))
}
curve(f1, from=-12, to=12)

f2 <- function(gamma) {
  1/(1+exp(-2*gamma))
}
curve(f2, from=-12, to=12)

f50 <- function(gamma) {
  1/(1+exp(-50*gamma))
}
curve(f50, from=-12, to=12)

f5000 <- function(gamma) {
  1/(1+exp(-5000*gamma))
}
curve(f5000, from=-12, to=12)

library(neuralnet)
u <- import.fred("unrate.csv")
ffr <- import.fred("ffr.csv")
inf <- import.fred("inflation.csv")
ds <- ts.combine(u, ffr, inf,
                 lags(u,1),
                 lags(ffr,1),
                 lags(inf,1))
colnames(ds) <- c("u", "ffr", "inf",
                  "u1", "ffr1", "inf1")
# Nonlinear Taylor Rule
fit.nn <- neuralnet(ffr ~ ffr1 + u1 + inf1, data=ds, hidden=2, stepmax=1000000)
fit.nn
