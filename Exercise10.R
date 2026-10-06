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