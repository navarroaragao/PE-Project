# ---- QUESTION 2 ----

library(ggplot2)

oil_data <- read.table("Petroleum&OtherLiquidFuels.txt", header = TRUE, 
                       sep = "", dec = ",", fill = TRUE)
oil_data$Date <- as.Date(oil_data$Date)

ggplot(oil_data, aes(x = Date)) +
  geom_line(aes(y = NYDiesel, color = "New York Diesel"), linewidth = 0.6) +
  geom_line(aes(y = LADiesel, color = "LA Diesel"), linewidth = 0.6) +
  geom_line(aes(y = MEAN, color = "Average (MEAN)"), linetype = "dotdash", 
            linewidth = 0.8) +
  labs(
    title = "Analysis of Fuel Price Fluctuations (2020-2026)",
    subtitle = "Daily YoY variation: NY Diesel vs LA Diesel vs Overall Average",
    x = "Timeline (Years)",
    y = "Relative Variation Rate",
    color = "Fuel Type"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("New York Diesel" = "darkblue", 
                                "LA Diesel" = "darkgreen", 
                                "Average (MEAN)" = "darkred"))