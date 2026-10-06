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