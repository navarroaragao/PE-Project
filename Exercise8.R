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