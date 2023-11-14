fit.u <- tsreg(u, rhs)
fit.inf <- tsreg(inf, rhs)
fit.u
fit.inf
B <- matrix(c(0.97, -0.01, 0.01, 0.99),
            ncol=2)
B
impact <- matrix(c(1, -0.11), ncol=1)
impact
IRF1 <- B %*% impact
IRF1
IRF2 <- B %*% IRF1
IRF3 <- B %*% IRF2
IRF4 <- B %*% IRF3
IRF2
IRF3
IRF4

# Generate a new dataset
u.fitted <- fit.u$fitted
inf.fitted <- fit.inf$fitted
length(u.fitted)
length(inf.fitted)
fitted.values <- ts.combine(u.fitted, 
                            inf.fitted)
dim(fitted.values)
u.res <- fit.u$resids
inf.res <- fit.inf$resids
res <- ts.combine(u.res, inf.res)
dim(res)

# Original data
fitted.values + res

z <- sample(c(-1,1),
            size=nrow(res),
            replace=TRUE)
z
mean(z)
data.sim <- fitted.values + z*res

set.seed(200)
output <- replicate(100, {
  # Simulate new data
  z <- sample(c(-1,1),
              size=nrow(res),
              replace=TRUE)
  data.sim <- fitted.values + z*res
  colnames(data.sim) <- c("u", "inf")
  usim <- data.sim[, "u"]
  infsim <- data.sim[, "inf"]
  
  rhs <- lags(data.sim, 1)
  fit.u1 <- tsreg(usim, rhs,
                  end=c(1994,12))
  fit.inf1 <- tsreg(infsim, rhs,
                  end=c(1994,12))
  res.u1 <- fit.u1$resids
  res.inf1 <- fit.inf1$resids
  var11 <- var(res.u1)
  var21 <- var(res.inf1)
  cov1 <- cov(res.u1, res.inf1)
  
  # Redo for regime 2
  fit.u2 <- tsreg(usim, rhs,
                  start=c(1995,1))
  fit.inf2 <- tsreg(infsim, rhs,
                    start=c(1995,1))
  res.u2 <- fit.u2$resids
  res.inf2 <- fit.inf2$resids
  var12 <- var(res.u2)
  var22 <- var(res.inf2)
  cov2 <- cov(res.u2, res.inf2)
  
  dev.nleq <- function(par) {
    b <- par[1]
    c <- par[2]
    su1 <- par[3]
    sinf1 <- par[4]
    su2 <- par[5]
    sinf2 <- par[6]
    dev1 <- var11 - su1 - (b^2)*sinf1
    dev2 <- var21 - (c^2)*su1 - sinf1
    dev3 <- cov1 - c*su1 - b*sinf1
    dev4 <- var12 - su2 - (b^2)*sinf2
    dev5 <- var22 - (c^2)*su2 - sinf2
    dev6 <- cov2 - c*su2 - b*sinf2
    return(c(dev1, dev2, dev3, dev4, dev5, dev6))
  }
  
  soln <- nleqslv(c(0,0,0.1,0.1,0.2,0.05), dev.nleq)
  rel1 <- soln$x[4]/soln$x[3]
  rel2 <- soln$x[6]/soln$x[5]
  rel2/rel1
})
sd(output)


