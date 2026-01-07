setwd("C:/Users/hp/Downloads/archive (14)")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(car)

# Load dataset 
cardio <- read_csv("8- cardiovascular-disease-death-rate-who-mdb.csv")

str(cardio)

colnames(cardio) <- make.names(colnames(cardio))
head(cardio)

# Rename relevant columns 
colnames(cardio) <- c("Country", "Code", "Year", "Cardio_Death_Rate")

# Remove missing and non-numeric entries
cardio <- cardio %>%
  filter(!is.na(Cardio_Death_Rate)) %>%
  mutate(Cardio_Death_Rate = as.numeric(Cardio_Death_Rate))

# Create grouping variable based on median mortality rate
median_rate <- median(cardio$Cardio_Death_Rate, na.rm = TRUE)
cardio <- cardio %>%
  mutate(Group = ifelse(Cardio_Death_Rate >= median_rate, "High_mortality", "Low_mortality"))

# Descriptive summary
summary_stats <- cardio %>%
  group_by(Group) %>%
  summarise(
    Mean_Rate = mean(Cardio_Death_Rate, na.rm = TRUE),
    SD_Rate = sd(Cardio_Death_Rate, na.rm = TRUE),
    Count = n()
  )
print(summary_stats)

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

ggsave("boxplot_cardiovascular_groups.png", width = 8, height = 6)
ggsave("histogram_cardiovascular_distribution.png", width = 8, height = 6)
ggsave("trend_cardiovascular_yearly_mean.png", width = 8, height = 6)
write_csv(cardio, "cleaned_cardiovascular_data.csv")

sink("Rscript.log")
cat("Descriptive Statistics:\n")
print(summary_stats)
cat("\nNormality Test:\n")
print(shapiro_test)
cat("\nLevene’s Test for Equality of Variances:\n")
print(levene_test)
cat("\nIndependent Samples T-Test Result:\n")
print(t_test_result)
sink()

