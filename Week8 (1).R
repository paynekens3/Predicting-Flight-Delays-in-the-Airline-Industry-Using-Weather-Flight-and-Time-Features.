library(tidyverse)
library(caret)
library(randomForest)
library(Metrics)
library(ggplot2)
# Load the dataset
file_path <- "C:/Users/mcken/DelayedFlights.csv"
df <- read.csv(file_path)
# Display the first few rows of the dataset
head(df)
# Get information about the dataset (e.g., number of non-null entries, data types)
str(df)
# Summary statistics to understand the distribution of numerical features
summary(df)
# Check for missing values
colSums(is.na(df))
# Impute missing values in delay columns with the mean
df$ArrDelay[is.na(df$ArrDelay)] <- mean(df$ArrDelay, na.rm = TRUE)
df$DepDelay[is.na(df$DepDelay)] <- mean(df$DepDelay, na.rm = TRUE)
# Check if there are still missing values
colSums(is.na(df))
# Label encoding categorical columns
df$Month <- as.numeric(factor(df$Month))
df$DayOfWeek <- as.numeric(factor(df$DayOfWeek))
# Display the first few rows after encoding
head(df)
# Feature engineering: Create 'DelayCategory' based on ArrDelay
df$DelayCategory <- cut(df$ArrDelay, breaks = c(-Inf, 0, 30, Inf), labels = c("No Delay", "Short Delay", "Long Delay"))
# Create a ratio of Departure Delay to Arrival Delay
df$DelayRatio <- df$DepDelay / ifelse(df$ArrDelay == 0, NA, df$ArrDelay)
# Check the newly created features
head(df[, c("ArrDelay", "DelayCategory", "DelayRatio")])
colnames(df)
# Select features and target variable
X <- df[, c("Month", "DayOfWeek", "DepTime", "CRSDepTime", "FlightNum")]
y <- df$ArrDelay  # Target variable is the arrival delay
# Set a seed for reproducibility
set.seed(42)
# Sample 10% of the data
sample_size <- floor(0.1 * nrow(X))  # 10% of your data
sample_index <- sample(seq_len(nrow(X)), size = sample_size)
# Create smaller dataset
X_small <- X[sample_index, ]
y_small <- y[sample_index]
# Split into training and testing sets (80% train, 20% test)
trainIndex <- createDataPartition(y_small, p = 0.8, list = FALSE)
X_train <- X_small[trainIndex, ]
y_train <- y_small[trainIndex]
X_test <- X_small[-trainIndex, ]
y_test <- y_small[-trainIndex]
# Check the shape of training and test sets
dim(X_train)
dim(X_test)
# Train the Linear Regression model
lr_model <- lm(ArrDelay ~ Month + DayOfWeek + DepTime + CRSDepTime + FlightNum, data = df[trainIndex,])
# Extract the test set from the dataset (X_test and y_test)
X_test <- df[-trainIndex, c("Month", "DayOfWeek", "DepTime", "CRSDepTime", "FlightNum")]
y_test <- df[-trainIndex, "ArrDelay"]
# Make predictions
y_pred_lr <- predict(lr_model, X_test)
# Evaluate the model (using base R for MAE and RMSE)
mae_lr <- mean(abs(y_test - y_pred_lr))  # Mean Absolute Error
rmse_lr <- sqrt(mean((y_test - y_pred_lr)^2))  # Root Mean Squared Error
r2_lr <- cor(y_test, y_pred_lr)^2  # R-squared
cat("Linear Regression - MAE:", mae_lr, "RMSE:", rmse_lr, "R2:", r2_lr, "\n")
# Train the Random Forest model
rf_model <- randomForest(ArrDelay ~ Month + DayOfWeek + DepTime + CRSDepTime + FlightNum, data = df[trainIndex,])
# Make predictions
y_pred_rf <- predict(rf_model, X_test)
# Evaluate the model (using base R for MAE and RMSE)
mae_rf <- mean(abs(y_test - y_pred_rf))  # Mean Absolute Error
rmse_rf <- sqrt(mean((y_test - y_pred_rf)^2))  # Root Mean Squared Error
r2_rf <- cor(y_test, y_pred_rf)^2  # R-squared
cat("Random Forest - MAE:", mae_rf, "RMSE:", rmse_rf, "R2:", r2_rf, "\n")
# Create a DataFrame to compare both models' performance
results <- data.frame(
  Model = c("Linear Regression", "Random Forest"),
  MAE = c(mae_lr, mae_rf),
  RMSE = c(rmse_lr, rmse_rf),
  R2 = c(r2_lr, r2_rf)
)
print(results)
# Visualize predictions vs actual values for Random Forest
library(ggplot2)
ggplot(data.frame(Actual = y_test, Predicted = y_pred_rf), aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5, color = 'pink') +
  labs(title = "Random Forest Predictions vs Actual", x = "Actual Arrival Delay", y = "Predicted Arrival Delay") +
  theme_minimal()

# Get the importance of the features
rf_importance <- importance(rf_model)

# Convert the importance into a data frame
importance_df <- data.frame(Feature = rownames(rf_importance), Importance = rf_importance[, 1])

# Plot the feature importance
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip the chart to make it horizontal
  labs(title = "Feature Importance - Random Forest", x = "Feature", y = "Importance") +
  theme_minimal()

