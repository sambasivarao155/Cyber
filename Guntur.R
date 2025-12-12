setwd("C:/Users/hp/Downloads/archive (14)")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(car)

# Load dataset 
cardio <- read_csv("8- cardiovascular-disease-death-rate-who-mdb.csv")


# Trend of mean mortality rate over years
trend_data <- cardio %>%
  group_by(Year) %>%
  summarise(Mean_Yearly_Rate = mean(Cardio_Death_Rate, na.rm = TRUE))
ggplot(trend_data, aes(x = Year, y = Mean_Yearly_Rate)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Trend of Average Cardiovascular Mortality Rate (2000–2019)",
    x = "Year",
    y = "Average Mortality Rate"
  ) +
  theme_minimal()

# Normality test 
shapiro_sample <- sample(cardio$Cardio_Death_Rate, 500, replace = FALSE)
shapiro_test <- shapiro.test(shapiro_sample)
print(shapiro_test)

# Check homogeneity of variances
levene_test <- leveneTest(Cardio_Death_Rate ~ Group, data = cardio)
print(levene_test)

# Independent samples t-test
t_test_result <- t.test(Cardio_Death_Rate ~ Group, data = cardio, var.equal = TRUE)
print(t_test_result)


cat("Mean (High Mortality Group):", round(summary_stats$Mean_Rate[summary_stats$Group == 'High_mortality'], 2), "\n")
cat("Mean (Low Mortality Group):", round(summary_stats$Mean_Rate[summary_stats$Group == 'Low_mortality'], 2), "\n")
cat("T-test p-value:", round(t_test_result$p.value, 5), "\n")

if (t_test_result$p.value < 0.05) {
  cat("Conclusion: Reject H0. There is a significant difference between the two groups.\n")
} else {
  cat("Conclusion: Fail to reject H0. No significant difference detected.\n")
}




