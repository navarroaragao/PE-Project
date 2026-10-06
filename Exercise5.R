# ---- QUESTION 5 ----
valor_exato <- integrate(function(x) {
  sapply(x, function(xi) {
    integrate(function(y) rep(2*xi/(1-xi), length(y)), 
              lower = 1.3*xi, 
              upper = 1)$value
  })
}, lower = 0, upper = 1/1.3)$value

set.seed(1207)
K <- 100
n <- 800
proporcoes <- numeric(K)
for (i in 1:K) {
  u <- runif(n)
  x <- sqrt(u)
  z <- runif(n)
  y <- x + z * (1 - x)
  proporcoes[i] <- mean(y > 1.3 * x)
}
valor_simulado <- mean(proporcoes)

round(valor_simulado / valor_exato, 4)