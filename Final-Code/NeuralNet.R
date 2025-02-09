library(nnet)
library(dplyr)
library(caret)
library(ggplot2)
library(gganimate)
library(tidyr)

reserve <- tor_df

tor_df <- tor_df %>%
  mutate(across(c(BEGIN_YEARMONTH, BEGIN_DAY, BEGIN_TIME, END_YEARMONTH, END_DAY, END_TIME, EVENT_ID), as.factor)) %>%
  mutate(across(where(is.numeric), ~ifelse(is.infinite(.), NA, .))) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) %>%
  mutate(DAMAGE_PROPERTY = as.numeric(DAMAGE_PROPERTY))

tor_df$log_DAMAGE_PROPERTY <- log1p(tor_df$DAMAGE_PROPERTY)

dummies <- dummyVars(~ STATE, data = tor_df)
state_dummies <- predict(dummies, newdata = tor_df)

tor_df <- cbind(tor_df, state_dummies)

features <- c("YEAR", "INJURIES_DIRECT", "DEATHS_DIRECT", "TOR_LENGTH", "TOR_WIDTH", colnames(state_dummies))

set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(tor_df$log_DAMAGE_PROPERTY, p = 0.8, list = FALSE)
train_data <- tor_df[trainIndex, ]
test_data <- tor_df[-trainIndex, ]

normalize <- function(x) {
  if (!is.numeric(x)) return(x)  
  (x - min(x)) / (max(x) - min(x))
}

train_data[features] <- lapply(train_data[features], normalize)
test_data[features] <- lapply(test_data[features], normalize)

train_data$log_DAMAGE_PROPERTY <- tor_df$log_DAMAGE_PROPERTY[trainIndex]
test_data$log_DAMAGE_PROPERTY <- tor_df$log_DAMAGE_PROPERTY[-trainIndex]

set.seed(123)
nn_model <- nnet(
  log_DAMAGE_PROPERTY ~ .,
  data = train_data[, c(features, "log_DAMAGE_PROPERTY")],
  size = 5,  
  linout = TRUE,
  maxit = 200
)

test_predictions <- predict(nn_model, test_data[, features], type = "raw")
predicted_damage <- expm1(test_predictions) 

actual_damage <- expm1(test_data$log_DAMAGE_PROPERTY)
results_df <- data.frame(Actual = actual_damage, Predicted = predicted_damage)
rmse <- sqrt(mean((results_df$Actual - results_df$Predicted)^2))
print(paste("RMSE:", rmse))

print(head(results_df))

library(nnet)
library(dplyr)
library(caret)

states <- unique(tor_df$STATE)  
years <- seq(2024, 2033)

future_data <- expand.grid(
  BEGIN_YEARMONTH = as.factor(paste0(rep(years, each = 12), sprintf("%02d", 1:12))),  
  STATE = states
)

future_data$YEAR <- as.numeric(substr(as.character(future_data$BEGIN_YEARMONTH), 1, 4))

if (!exists("dummies")) {
  dummies <- dummyVars(~ STATE, data = tor_df)
}

future_data_dummies <- predict(dummies, newdata = future_data)

future_data_dummies[is.na(future_data_dummies)] <- 0

future_data <- cbind(future_data, future_data_dummies)

future_data$INJURIES_DIRECT <- 0
future_data$DEATHS_DIRECT <- 0

mean_tor_length <- mean(tor_df$TOR_LENGTH, na.rm = TRUE) 
mean_tor_width <- mean(tor_df$TOR_WIDTH, na.rm = TRUE)    
future_data$TOR_LENGTH <- mean_tor_length
future_data$TOR_WIDTH <- mean_tor_width

features <- c("YEAR", "INJURIES_DIRECT", "DEATHS_DIRECT", "TOR_LENGTH", "TOR_WIDTH", colnames(future_data_dummies))

normalize <- function(x) {
  if (!is.numeric(x)) return(x)  
  rng <- max(x) - min(x)
  if (rng == 0) return(rep(0, length(x)))  
  (x - min(x)) / rng
}

future_data[features] <- lapply(future_data[features], normalize)

na_check_after_norm <- sapply(future_data[features], function(x) sum(is.na(x)))
if (any(na_check_after_norm > 0)) {
  print("NA values detected in the following features after normalization:")
  print(na_check_after_norm[na_check_after_norm > 0])
  stop("Resolve NA values after normalization.")
}

future_predictions <- predict(nn_model, future_data[, features], type = "raw")

if (any(is.na(future_predictions))) {
  stop("NA values detected in predictions. Check model and input data consistency.")
}

future_data$Predicted_DAMAGE_PROPERTY <- expm1(future_predictions)

write.csv(future_data, "future_damage_property_predictions.csv", row.names = FALSE)

print("Future projections for DAMAGE_PROPERTY completed and saved to 'future_damage_property_predictions.csv'")