# ---- QUESTION 4 ----
library(extraDistr)
mu <- 2350
sigma <- 600

LA <- qgumbel(0.9, mu, sigma)
LE <- LA + 1000

p_superior_LE <- 1 - pgumbel(LE, mu, sigma) 
p_superior_LA <- 0.1
prob_cond_exata <- p_superior_LE / p_superior_LA

set.seed(4339)
amostra <- rgumbel(2000, mu, sigma)
acima_LA <- amostra[amostra > LA]
prop_sim <- mean(acima_LA > LE)

round(prop_sim / prob_cond_exata, 4)

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

# ---- QUESTION 6 ----
set.seed(1910)
M <- 163
n <- 237
lambda <- 3
epsilon <- 0.15

medias <- replicate(M, mean(rpois(n, lambda)))
freq_rel <- mean(abs(medias - 3) <= epsilon)

prob_tlc <- 2 * pnorm(epsilon / sqrt(lambda/n)) - 1

quociente <- freq_rel / prob_tlc
round(quociente, 4)

# ---- QUESTION 7 ----
x <- c(5.84, 4.29, 5.32, 6.66, 6.14, 5.62, 4.33, 4.99, 5.33, 5.72, 6.04)
n <- length(x)

f_alfa <- function(alfa) {
  term1 <- sum(x^alfa * log(x)) / sum(x^alfa)
  term2 <- 1 / alfa
  term3 <- mean(log(x))
  return(term1 - term2 - term3)
}

solucao_alfa <- uniroot(f_alfa, interval = c(2.8, 9.2))
alfa_hat <- solucao_alfa$root

lambda_hat <- (mean(x^alfa_hat))^(1/alfa_hat)

mediana_mle <- lambda_hat * (log(2))^(1/alfa_hat)

round(mediana_mle, 2)

# ---- QUESTION 8 ----
set.seed(3799)
m <- 1200
n <- 51
lambda_true <- 5
gamma <- 0.93
alfa <- 1 - gamma
z <- qnorm(1 - alfa/2)  
q_inf <- qchisq(alfa/2, df = 2*n)
q_sup <- qchisq(1 - alfa/2, df = 2*n)
p1 <- 0
p2 <- 0
for (i in 1:m) {
  x <- rexp(n, rate = 1/lambda_true)
  xbar <- mean(x)
  
  L1 <- (2*n*xbar) / q_sup
  U1 <- (2*n*xbar) / q_inf
  if (L1 <= lambda_true && lambda_true <= U1) p1 <- p1 + 1
  
  L2 <- xbar / (1 + z/sqrt(n))
  U2 <- xbar / (1 - z/sqrt(n))
  if (L2 <= lambda_true && lambda_true <= U2) p2 <- p2 + 1
}
p1 <- p1/m
p2 <- p2/m
ratio <- p1/p2
round(ratio, 4)

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

# ---- QUESTION 10 ----
set.seed(2342)

dados_raw <- scan("Modular_P800.txt", what = numeric())

subamostra <- sample(dados_raw, size = floor(length(dados_raw) * 0.6))

k_sturges <- nclass.Sturges(subamostra)
k_fd <- nclass.FD(subamostra)

calc_p_value <- function(dados, k) {
  
  quebras <- qnorm(seq(0, 1, length.out = k + 1))
  quebras[1] <- -Inf
  quebras[k + 1] <- Inf
  
  classes <- cut(dados, breaks = quebras, include.lowest = TRUE)
  observado <- as.numeric(table(factor(classes, levels = levels(classes))))
  
  esperado <- rep(length(dados) / k, k)
  
  chi2 <- sum((observado - esperado)^2 / esperado)
  
  p_valor <- pchisq(chi2, df = k - 1, lower.tail = FALSE)
  
  return(p_valor)
}

p_sturges <- calc_p_value(subamostra, k_sturges)
p_fd <- calc_p_value(subamostra, k_fd)

quociente <- max(p_sturges, p_fd) / min(p_sturges, p_fd)

round(quociente, 4)