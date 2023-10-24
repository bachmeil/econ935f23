library(tstools)
u <- import.fred("unrate.csv")
inf <- import.fred("inflation.csv")
fit <- tsreg(u, lags(u %~% inf, 1),
             end=c(2019,12))
fit
shock.u <- fit$resids
plot(shock.u)
# Regress inflation on lags of the shocks
fit.irf <- tsreg(inf, lags(shock.u,0:12))
fit.irf
u
inf
plot(coefficients(fit.irf)[-1],
     type="l",
     main="IRF of Inflation to One Pct Pt Higher U")
irf.cumulative <- cumsum(coefficients(fit.irf)[-1])
plot(irf.cumulative,
     type="l",
     main="Cumulative IRF of Inflation to One Pct Pt Higher U")

# Change the timing restriction
# Inflation isn't affected by economic activity
# shocks contemporaneously
fit <- tsreg(u, inf %~% lags(u %~% inf, 1),
             end=c(2019,12))
fit
shock.u <- fit$resids
plot(shock.u)

fit.irf <- tsreg(inf, lags(shock.u,0:12))
fit.irf
u
inf
plot(coefficients(fit.irf)[-1],
     type="l",
     main="IRF of Inflation to One Pct Pt Higher U")
irf.cumulative <- cumsum(coefficients(fit.irf)[-1])
plot(irf.cumulative,
     type="l",
     main="Cumulative IRF of Inflation to One Pct Pt Higher U")
fit.inf <- tsreg(inf, u %~% lags(u %~% inf,1))
fit.inf
fit.u <- tsreg(u, lags(u %~% inf,1))
fit.u

D <- matrix(c(0,0,-0.18,0),ncol=2)
D
B <- matrix(c(0.99,0.01,0.165,0.97),ncol=2)
B
A <- soAlve(diag(2) - D)
A
E <- matrix(c(0,1), ncol=1)
A %*% E
A %*% B %*% A %*% E
irf <- A %*% E
for (h in 1:12) {
  irf <- A %*% B %*% irf
  cat("h:", h, irf[1], "\n")
}

# Linear algebra hack
x <- matrix(c(1,2.2,4.4,
              0,1,5.5,
              0,0,1),ncol=3)
x
y <- diag(3)
y
diag(y) <- c(1.2, 2.4, 3.5)
y

# You only observe
M <- x %*% y %*% t(y) %*% t(x)
M
# Now recover y and x using only M
P <- t(chol(M))
P
P %*% t(P)

D <- diag(3)
diag(D) <- diag(P)
D
y
P %*% solve(D)
x
