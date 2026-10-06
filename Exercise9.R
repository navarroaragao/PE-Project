# ---- QUESTION 9 ----
n <- 38
m <- 1000
alfa <- 0.1

a0 <- 2.5; b0 <- 3
mu0  <- a0 / (a0 + b0)
var0 <- (a0*b0) / ((a0+b0)^2 * (a0+b0+1))
se0  <- sqrt(var0 / n)

k <- qnorm(1 - alfa, mean = mu0, sd = se0)

a1 <- 2.75; b1 <- 3
mu1  <- a1 / (a1 + b1)
var1 <- (a1*b1) / ((a1+b1)^2 * (a1+b1+1))
se1  <- sqrt(var1 / n)

beta_teo <- pnorm(k, mean = mu1, sd = se1)

set.seed(596)
medias   <- replicate(m, mean(rbeta(n, a1, b1)))
beta_hat <- mean(medias <= k)

quociente <- beta_hat / beta_teo
round(quociente, 4)