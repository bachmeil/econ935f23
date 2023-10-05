library(tstools)
ngdp <- import.fred("ngdp.csv")
m2 <- import.fred("m2.csv")
lv <- log(ngdp) - log(m2)
plot(lv)
plot(exp(lv))
library(urca)
df.drift <- ur.df(y=lv,
                  type="trend",
                  selectlags="AIC")
summary(df.drift)
# Fail to reject unit root in
# velocity => this version of the
# quantity theory does not hold
# No evidence of relationship even
# in the long run between money and
# prices

gas <- import.fred("gas.csv")
wti <- import.fred("wti.csv")/42
plot(ts.combine(gas, wti),
     plot.type="single")
plot(gas-wti)
fit <- tsreg(gas, wti)
fit
fit <- tsreg(log(gas), log(wti))
fit
plot(fit$resids)
df.drift <- ur.df(y=fit$resids,
                  type="drift",
                  selectlags="AIC")
summary(df.drift)
# DF test: no unit root
# cointegrated
# But wrong critical values!
library(aTSA)
dataset <- ts.combine(gas, wti)
coint.test(dataset[,1], dataset[,2])


