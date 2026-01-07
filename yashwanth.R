setwd("C:/Users/hp/Downloads/archive (14)")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(car)

# Load dataset 
cardio <- read_csv("8- cardiovascular-disease-death-rate-who-mdb.csv")


# Descriptive summary
summary_stats <- cardio %>%
  group_by(Group) %>%
  summarise(
    Mean_Rate = mean(Cardio_Death_Rate, na.rm = TRUE),
    SD_Rate = sd(Cardio_Death_Rate, na.rm = TRUE),
    Count = n()
  )
print(summary_stats)