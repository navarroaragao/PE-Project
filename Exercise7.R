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