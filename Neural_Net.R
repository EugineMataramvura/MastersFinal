# Load necessary libraries
library(nnet)
library(dplyr)
library(caret)
library(ggplot2)
library(gganimate)
library(tidyr)

# Assume tor_df is your tornado dataset loaded into R
# Data preparation and cleaning
tor_df <- tor_df %>%
  mutate(across(c(BEGIN_YEARMONTH, BEGIN_DAY, BEGIN_TIME, END_YEARMONTH, END_DAY, END_TIME, EVENT_ID), as.factor)) %>%
  mutate(across(where(is.numeric), ~ifelse(is.infinite(.), NA, .))) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) %>%
  mutate(DAMAGE_PROPERTY = as.numeric(DAMAGE_PROPERTY))

# Transform DAMAGE_PROPERTY to a log scale
tor_df$log_DAMAGE_PROPERTY <- log1p(tor_df$DAMAGE_PROPERTY)

# Convert STATE to dummy variables
dummies <- dummyVars(~ STATE, data = tor_df)
state_dummies <- predict(dummies, newdata = tor_df)

# Combine dummy variables with the original data frame
tor_df <- cbind(tor_df, state_dummies)

# Select features for the model
features <- c("YEAR", "INJURIES_DIRECT", "DEATHS_DIRECT", "TOR_LENGTH", "TOR_WIDTH", colnames(state_dummies))

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(tor_df$log_DAMAGE_PROPERTY, p = 0.8, list = FALSE)
train_data <- tor_df[trainIndex, ]
test_data <- tor_df[-trainIndex, ]

# Normalize the numeric features
normalize <- function(x) {
  if (!is.numeric(x)) return(x)  # Skip non-numeric columns
  (x - min(x)) / (max(x) - min(x))
}

train_data[features] <- lapply(train_data[features], normalize)
test_data[features] <- lapply(test_data[features], normalize)

# Ensure log_DAMAGE_PROPERTY is part of the training data
train_data$log_DAMAGE_PROPERTY <- tor_df$log_DAMAGE_PROPERTY[trainIndex]
test_data$log_DAMAGE_PROPERTY <- tor_df$log_DAMAGE_PROPERTY[-trainIndex]

# Train the neural network model
set.seed(123)
nn_model <- nnet(
  log_DAMAGE_PROPERTY ~ .,
  data = train_data[, c(features, "log_DAMAGE_PROPERTY")],
  size = 5,  # Number of neurons in the hidden layer
  linout = TRUE,
  maxit = 200
)

# Make predictions on the test set
test_predictions <- predict(nn_model, test_data[, features], type = "raw")
predicted_damage <- expm1(test_predictions)  # Convert back from log scale

# Evaluate the model performance
actual_damage <- expm1(test_data$log_DAMAGE_PROPERTY)
results_df <- data.frame(Actual = actual_damage, Predicted = predicted_damage)
rmse <- sqrt(mean((results_df$Actual - results_df$Predicted)^2))
print(paste("RMSE:", rmse))

# View results
print(head(results_df))
