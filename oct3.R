library(tstools)
u <- import.fred("unrate.csv")
inf <- import.fred("inflation.csv")
phillips <- ts.combine(u, inf)
library(vars)
dates <- make.dates(c(2009,12), c(2023,6),
                    12)
varfcst <- function(d) {
  ds <- window(phillips, start=d %m-% 119,
               end=d)
  fit <- VAR(ds, lag.max=6, ic="SC")
  getVarForecast(fit, "inf", 1)
}
fcst.var <- sapply(dates, varfcst)
fcst.var

