# ---- EXERCISE 1 ----
library(ggplot2)

data <- read.csv("hcvdat0.csv", header = TRUE)

ggplot(data = data, aes(x = Category, y = CREA)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(
    title = "Analysis of CREA Levels by Category",
    x = "Clinical Category",
    y = "Creatinine (CREA)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))