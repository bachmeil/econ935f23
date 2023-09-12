y <- arima.sim(list(ar=0.2), n=400)
plot(y)

y <- arima.sim(list(ar=0.9), n=400)
plot(y)
abline(a=0,b=0)

y <- arima.sim(list(ar=0.95), n=400)
plot(y)
abline(a=0,b=0)

y <- arima.sim(list(ar=0.99), n=400)
plot(y)
abline(a=0,b=0)

y <- arima.sim(list(ar=0.9999), n=400)
plot(y)
abline(a=0,b=0)

set.seed(200)
simoutput <- replicate(1000, {
  y <- arima.sim(list(ar=0.999), n=400)
  x <- arima.sim(list(ar=0.1), n=400)
  cor(y, x)
})
plot(density(simoutput))
