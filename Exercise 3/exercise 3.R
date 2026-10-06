# ---- QUESTION 3 ----

library(ggplot2)

jobs_data <- read.csv("Jobs_and_Growth.csv", header = TRUE, sep = ",", check.names = FALSE)

jobs_data$`Member State Name` <- trimws(jobs_data$`Member State Name`)

filtered_jobs <- subset(jobs_data, 
                        `Member State Name` %in% c("Croatia", "Finland") & 
                          `Time Period` >= 2008 & `Time Period` <= 2022 &
                          `Sub-indicator Name` == "Employment in the food industry" &
                          `Sub-indicator Parameter` == "share")

ggplot(data = filtered_jobs, aes(x = factor(`Time Period`), y = Data, fill = `Member State Name`)) +
  geom_col(position = position_dodge(width = 0.9), color = "black") +
  scale_fill_manual(values = c("Croatia" = "#1f77b4", "Finland" = "#ff7f0e")) +
  labs(
    title = "Food Industry Employment Share (2008-2022)",
    subtitle = "Comparative analysis between Croatia and Finland",
    x = "Year",
    y = "Share (% of total employment)",
    fill = "Country"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )