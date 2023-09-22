library(urca)
library(tstools)
rgdp <- import.fred("rgdp.csv")
plot(rgdp)

df.none <- ur.df(y=rgdp, type="none",
                 selectlags="AIC")
summary(df.none)
# Don't reject H0 -> Unit root
df.drift <- ur.df(y=rgdp, type="drift",
                 selectlags="AIC")
summary(df.drift)
# Don't reject H0 -> Unit root
df.trend <- ur.df(y=rgdp, type="trend",
                  selectlags="AIC")
summary(df.trend)
# Don't reject H0 -> Unit root
# All three fail to reject
# Conclusion: Unit root!

gas <- import.fred("gas.csv")
plot(gas)
df.drift <- ur.df(y=gas, type="drift",
                  selectlags="AIC")
summary(df.drift)
df.trend <- ur.df(y=gas, type="trend",
                  selectlags="AIC")
summary(df.trend)
# Reject H0 -> Assume it's stationary
df.trend <- ur.df(y=gas, type="trend",
                  selectlags="Fixed",
                  lags=4)
summary(df.trend)
pp <- ur.pp(gas)
summary(pp)
pp <- ur.pp(gas, model="trend")
summary(pp)

plot(rgdp)
dum <- time.dummy(rgdp, c(2007,4),
                  c(2009,2))
dum

tsreg(rgdp, ts.combine(lags(rgdp,1),
                       dum))
