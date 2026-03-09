# ==========================================
# Maize Price Volatility Analysis
# ==========================================

# Load libraries
library(ggplot2)
library(dplyr)

# ------------------------------------------
# Step 1 — Load Dataset
# ------------------------------------------

setwd("G:/Projects/price volatility analysis")

data <- read.csv("maize_price_arrivals.csv")

data$date <- as.Date(data$date)

str(data)
head(data)

# ------------------------------------------
# Step 2 — Calculate Price Change
# ------------------------------------------

data <- data %>%
  arrange(date) %>%
  mutate(price_change = price - lag(price))

# Remove first NA row
data <- data %>% filter(!is.na(price_change))

# ------------------------------------------
# Step 3 — Calculate Percentage Change
# ------------------------------------------

data <- data %>%
  mutate(price_change_pct = (price_change / lag(price)) * 100)

# ------------------------------------------
# Step 4 — Monthly Price Change Plot
# ------------------------------------------

plot1 <- ggplot(data, aes(x = date, y = price_change)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(
    title = "Monthly Change in Maize Prices",
    x = "Date",
    y = "Price Change (Rs/quintal)"
  ) +
  theme_minimal(base_size = 14)

print(plot1)

# Save plot
ggsave(
  "maize_price_change.svg",
  plot = plot1,
  width = 12,
  height = 5.5
)

# ------------------------------------------
# Step 5 — Percentage Volatility Plot
# ------------------------------------------

plot2 <- ggplot(data, aes(x = date, y = price_change_pct)) +
  geom_line(color = "darkred", linewidth = 1) +
  labs(
    title = "Percentage Change in Maize Prices",
    x = "Date",
    y = "Price Change (%)"
  ) +
  theme_minimal(base_size = 14)

print(plot2)

# Save plot
ggsave(
  "maize_price_volatility.svg",
  plot = plot2,
  width = 12,
  height = 5.5
)

# ------------------------------------------
# Step 6 — Calculate Price Volatility
# ------------------------------------------

volatility <- sd(data$price_change, na.rm = TRUE)

print(paste("Price volatility:", volatility))

# ------------------------------------------
# End of Script
# ------------------------------------------