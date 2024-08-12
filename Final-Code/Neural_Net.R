# Load necessary libraries
library(nnet)
library(dplyr)
library(caret)
library(ggplot2)
library(gganimate)
library(tidyr)

reserve <- tor_df

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

# Load necessary libraries
library(nnet)
library(dplyr)
library(caret)

# Define the states and years for future projection
states <- unique(tor_df$STATE)  # Assuming tor_df is your original data frame
years <- seq(2024, 2033)  # Next 10 years

# Generate future data
future_data <- expand.grid(
  BEGIN_YEARMONTH = as.factor(paste0(rep(years, each = 12), sprintf("%02d", 1:12))),  # Generate YEAR and MONTH combinations
  STATE = states
)

# Extract YEAR from BEGIN_YEARMONTH and convert to numeric
future_data$YEAR <- as.numeric(substr(as.character(future_data$BEGIN_YEARMONTH), 1, 4))

# Ensure the dummyVars object is correctly created from all potential states
if (!exists("dummies")) {
  # Recreate the dummy variable model using all potential states from the training data
  dummies <- dummyVars(~ STATE, data = tor_df)
}

# Convert STATE to dummy variables in the future data
future_data_dummies <- predict(dummies, newdata = future_data)

# Replace NA in dummy variables with 0 (assuming NA means the state is not present)
future_data_dummies[is.na(future_data_dummies)] <- 0

# Combine dummy variables with the future data frame
future_data <- cbind(future_data, future_data_dummies)

# Initialize other features; you might need to adjust based on domain knowledge
future_data$INJURIES_DIRECT <- 0
future_data$DEATHS_DIRECT <- 0

# Initialize TOR_LENGTH and TOR_WIDTH with reasonable values, such as the mean from the historical data
mean_tor_length <- mean(tor_df$TOR_LENGTH, na.rm = TRUE)  # Calculate mean of historical data
mean_tor_width <- mean(tor_df$TOR_WIDTH, na.rm = TRUE)    # Calculate mean of historical data
future_data$TOR_LENGTH <- mean_tor_length
future_data$TOR_WIDTH <- mean_tor_width

# Select and normalize features for the model, matching the training process
features <- c("YEAR", "INJURIES_DIRECT", "DEATHS_DIRECT", "TOR_LENGTH", "TOR_WIDTH", colnames(future_data_dummies))

# Enhanced normalization function to avoid division by zero
normalize <- function(x) {
  if (!is.numeric(x)) return(x)  # Skip non-numeric columns
  rng <- max(x) - min(x)
  if (rng == 0) return(rep(0, length(x)))  # If no variance, return zeros
  (x - min(x)) / rng
}

# Apply normalization
future_data[features] <- lapply(future_data[features], normalize)

# Check for NA values after normalization
na_check_after_norm <- sapply(future_data[features], function(x) sum(is.na(x)))
if (any(na_check_after_norm > 0)) {
  print("NA values detected in the following features after normalization:")
  print(na_check_after_norm[na_check_after_norm > 0])
  stop("Resolve NA values after normalization.")
}

# Predict future DAMAGE_PROPERTY
future_predictions <- predict(nn_model, future_data[, features], type = "raw")

# Check for NA predictions
if (any(is.na(future_predictions))) {
  stop("NA values detected in predictions. Check model and input data consistency.")
}

# Convert predictions back from log scale to original scale
future_data$Predicted_DAMAGE_PROPERTY <- expm1(future_predictions)

# Save the future predictions to a CSV file
write.csv(future_data, "future_damage_property_predictions.csv", row.names = FALSE)

print("Future projections for DAMAGE_PROPERTY completed and saved to 'future_damage_property_predictions.csv'")

