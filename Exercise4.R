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