library(urca)
data("Raotbl3")
vec <- ca.jo(Raotbl3[, c("li", "lc")])
vec
summary(vec)
vec <- ca.jo(Raotbl3[, c("li", "lc")],
             ecdet="trend")
summary(vec)
