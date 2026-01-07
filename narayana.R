setwd("C:/Users/hp/Downloads/archive (14)")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(car)

# Load dataset 
cardio <- read_csv("8- cardiovascular-disease-death-rate-who-mdb.csv")


#  Boxplot comparison
ggplot(cardio, aes(x = Group, y = Cardio_Death_Rate, fill = Group)) +
  geom_boxplot(alpha = 0.7, outlier.color = "black") +
  labs(
    title = "Comparison of Cardiovascular Mortality Rates Between Groups",
    x = "Country Group (Based on Median Mortality)",
    y = "Cardiovascular Death Rate (per 100,000 population)"
  ) +
  theme_minimal()

# Histogram of mortality rate distribution
ggplot(cardio, aes(x = Cardio_Death_Rate)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.8) +
  labs(
    title = "Distribution of Cardiovascular Mortality Rates (2000–2019)",
    x = "Mortality Rate (per 100,000 population)",
    y = "Frequency"
  ) +
  theme_minimal()