library(tstools)
u <- import.fred("unrate.csv")
inf <- import.fred("inflation.csv")
fit.u <- tsreg(u, lags(u %~% inf, 1),
               end=c(2019,12))
fit.inf <- tsreg(inf, lags(u %~% inf, 1),
               end=c(2019,12))
res.u <- fit.u$resids
res.inf <- fit.inf$resids
res <- ts.combine(res.u, res.inf)
cov(res)
t(chol(cov(res)))
tsreg(inf, u %~% lags(u %~% inf,1),
      end=c(2019,12))
# -0.3215 -> 1 pct point change in u
# causes inflation to fall -0.32
sd(res.u)
-0.3215*0.205
fit.u
fit.inf

# Three variable model
ffr <- import.fred("ffr.csv")
fit.u <- tsreg(u, lags(ffr %~% u %~% inf,1),
               end=c(2019,12))
fit.inf <- tsreg(inf, lags(ffr %~% u %~% inf,1),
               end=c(2019,12))
fit.ffr <- tsreg(ffr, lags(ffr %~% u %~% inf,1),
               end=c(2019,12))
res.u <- fit.u$resids
res.inf <- fit.inf$resids
res.ffr <- fit.ffr$resids
res <- ts.combine(res.ffr, res.u, res.inf)
t(chol(cov(res)))
res <- ts.combine(res.ffr, res.inf, res.u)
t(chol(cov(res)))
res <- ts.combine(res.inf, res.ffr, res.u)
t(chol(cov(res)))

library(vars)
dataset <- ts.combine(u, inf)
varfit <- VAR(dataset, lag.max=4, ic="AIC")
varfit
# Reduced form VAR
# Now use the Choleski to calculate the IRF
irf(varfit, impulse="u",
    boot=FALSE, n.ahead=12)
irf(varfit, impulse="inf", response="u",
    boot=FALSE, n.ahead=12)
plot(irf(varfit, impulse="u", response="inf",
    boot=TRUE, n.ahead=12))

plot(u)
dataset2 <- ts.combine(inf, u)
varfit2 <- VAR(dataset2, lag.max=4, ic="AIC")
irf(varfit2, impulse="u",
    boot=FALSE, n.ahead=12)
