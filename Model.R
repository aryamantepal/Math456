# Title: Insurance Forecast using Simple Linear Regression
# Authors: Aryaman, Jun, Arjun, Lawrence, Greg, Jerry, Raj
# Date: 02/18/2026

library(tidyverse) 
library(ggpubr)   

# Ensure insurance.csv is in your working directory or provide the full path
insurance <- read_csv("~/Downloads/insurance.csv")

# Feature Engineering: Convert categorical 'smoker' to numeric binary
# This allows us to calculate correlation and run the regression model.
insurance$smoker_num <- ifelse(insurance$smoker == "yes", 1, 0)

# Correlation Check
cor_value <- cor(insurance$smoker_num, insurance$charges)
print(paste("Correlation between Smoker and Charges:", round(cor_value, 4)))


set.seed(456)

# train / test split
sample_index <- sample(1:nrow(insurance), 0.8 * nrow(insurance))
train_data <- insurance[sample_index, ]
test_data  <- insurance[-sample_index, ]

model_smoker <- lm(charges ~ smoker_num, data = train_data)

ggplot(train_data, aes(x = smoker_num, y = charges)) +
  geom_jitter(alpha = 0.2, width = 0.1) + 
  stat_smooth(method = "lm", color = "red") +
  scale_x_continuous(breaks = c(0, 1), labels = c("Non-Smoker (0)", "Smoker (1)")) +
  labs(title = "Linear Regression: Smoking Status vs Medical Charges",
       subtitle = paste("R-Squared:", round(summary(model_smoker)$r.squared, 3)),
       x = "Smoker Status", y = "Charges ($)")

# see model params
summary(model_smoker)

# get results
predictions <- predict(model_smoker, newdata = test_data)

# Calculate Accuracy Metrics
eval_results <- data.frame(
  Actual = test_data$charges,
  Predicted = predictions
)

mae <- mean(abs(eval_results$Actual - eval_results$Predicted))
rmse <- sqrt(mean((eval_results$Actual - eval_results$Predicted)^2))

# will prob be off by thousands, looking at the plots
print(paste("Mean Absolute Error (MAE):", round(mae, 2)))
print(paste("Root Mean Squared Error (RMSE):", round(rmse, 2)))

par(mfrow = c(2, 2))
plot(model_smoker)