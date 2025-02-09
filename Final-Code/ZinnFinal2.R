library(dplyr)
library(ggplot2)
library(tensorflow)
library(keras)
library(caret)

set.seed(123)
tor_df$DAMAGE_PROPERTY_NUM <- as.numeric(as.character(tor_df$DAMAGE_PROPERTY))
tor_df$DAMAGE_PROPERTY_NUM[is.na(tor_df$DAMAGE_PROPERTY_NUM)] <- 0
tor_df$ZERO_INDICATOR <- ifelse(tor_df$DAMAGE_PROPERTY_NUM == 0, 1, 0)
vars <- c("BEGIN_LAT", "BEGIN_LON", "END_LAT", "END_LON", "MONTH_NAME", "TOR_F_SCALE",
          "INJURIES_DIRECT", "INJURIES_INDIRECT", "DEATHS_DIRECT", "DEATHS_INDIRECT",
          "DAMAGE_CROPS", "TOR_LENGTH", "TOR_WIDTH", "ZERO_INDICATOR", "DAMAGE_PROPERTY_NUM")
data_use <- tor_df[, vars]
data_use <- data_use[complete.cases(data_use), ]
data_use$MONTH_NAME <- as.factor(data_use$MONTH_NAME)
data_use$TOR_F_SCALE <- as.factor(data_use$TOR_F_SCALE)
data_use$ZERO_INDICATOR <- as.numeric(data_use$ZERO_INDICATOR)
data_use$DAMAGE_PROPERTY_NUM <- as.numeric(data_use$DAMAGE_PROPERTY_NUM)
predictors <- model.matrix(~ BEGIN_LAT + BEGIN_LON + END_LAT + END_LON +
                             MONTH_NAME + TOR_F_SCALE + INJURIES_DIRECT +
                             INJURIES_INDIRECT + DEATHS_DIRECT + DEATHS_INDIRECT +
                             DAMAGE_CROPS + TOR_LENGTH + TOR_WIDTH - 1, data = data_use)
response_zero <- data_use$ZERO_INDICATOR
response_severity <- data_use$DAMAGE_PROPERTY_NUM
split_index <- createDataPartition(response_zero, p = 0.8, list = FALSE)
x_train <- predictors[split_index, ]
x_test  <- predictors[-split_index, ]
y_zero_train <- response_zero[split_index]
y_zero_test  <- response_zero[-split_index]
y_severity_train <- response_severity[split_index]
y_severity_test  <- response_severity[-split_index]
x_train_scaled <- scale(x_train)
x_test_scaled <- scale(x_test,
                       center = attr(x_train_scaled, "scaled:center"),
                       scale  = attr(x_train_scaled, "scaled:scale"))
y_zero_train <- matrix(y_zero_train, ncol = 1)
y_zero_test  <- matrix(y_zero_test, ncol = 1)
y_severity_train <- matrix(y_severity_train, ncol = 1)
y_severity_test  <- matrix(y_severity_test, ncol = 1)
y_train <- cbind(y_zero_train, y_severity_train)
y_test  <- cbind(y_zero_test, y_severity_test)

build_zinn_model <- function(input_dim) {
  inputs <- layer_input(shape = input_dim)
  shared <- inputs %>% 
    layer_dense(units = 64, activation = "relu") %>% 
    layer_dense(units = 32, activation = "relu")
  zero_logits <- shared %>% 
    layer_dense(units = 1, activation = "linear", name = "zero_logits")
  severity_pred <- shared %>% 
    layer_dense(units = 1, activation = "relu", name = "severity_pred")
  outputs <- layer_concatenate(list(zero_logits, severity_pred))
  keras_model(inputs = inputs, outputs = outputs)
}

model1 <- build_zinn_model(ncol(x_train_scaled))
custom_loss <- function(y_true, y_pred) {
  y_true_zero <- y_true[, 1, drop = FALSE]
  y_true_severity <- y_true[, 2, drop = FALSE]
  logits_zero <- y_pred[, 1, drop = FALSE]
  pred_severity <- y_pred[, 2, drop = FALSE]
  bce <- tf$reduce_mean(tf$nn$sigmoid_cross_entropy_with_logits(labels = y_true_zero, logits = logits_zero))
  mse <- tf$reduce_mean(tf$square(y_true_severity - pred_severity) * (1 - y_true_zero))
  bce + mse
}

model1 %>% compile(
  optimizer = tf$keras$optimizers$legacy$Adam(learning_rate = 1e-3),
  loss = custom_loss
)

history1 <- model1 %>% fit(
  x_train_scaled,
  y_train,
  epochs = 10,
  batch_size = 32,
  validation_split = 0.2,
  verbose = 1
)

preds <- model1 %>% predict(x_test_scaled)
p_zero <- 1 / (1 + exp(-preds[, 1]))
expected_loss <- (1 - p_zero) * preds[, 2]
results <- data.frame(
  Actual_Severity = y_severity_test,
  Predicted_Severity = preds[, 2],
  p_NoLoss = p_zero,
  Expected_Loss = expected_loss
)

loss_df <- data.frame(
  epoch = 1:length(history1$metrics$loss),
  loss = history1$metrics$loss,
  val_loss = history1$metrics$val_loss
)
p_loss <- ggplot(loss_df, aes(x = epoch)) +
  geom_line(aes(y = loss, color = "Training Loss"), linewidth = 1) +
  geom_line(aes(y = val_loss, color = "Validation Loss"), linewidth = 1) +
  ggtitle("Training and Validation Loss") +
  ylab("Loss") +
  theme_minimal()
ggsave("loss_plot_cbp.png", p_loss, width = 8, height = 6, dpi = 300)

p_expected <- ggplot(results, aes(x = Actual_Severity, y = Expected_Loss)) +
  geom_point(alpha = 0.5) +
  ggtitle("Actual vs. Expected Loss (Catastrophe Bond Pricing)") +
  xlab("Actual Severity") +
  ylab("Expected Loss") +
  theme_minimal()
ggsave("actual_vs_expected_loss.png", p_expected, width = 8, height = 6, dpi = 300)

p_severity <- ggplot(results, aes(x = Actual_Severity, y = Predicted_Severity)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  ggtitle("Actual vs. Predicted Severity") +
  xlab("Actual Severity") +
  ylab("Predicted Severity") +
  theme_minimal()
ggsave("actual_vs_predicted_severity.png", p_severity, width = 8, height = 6, dpi = 300)

p_hist_severity <- ggplot(data.frame(Predicted_Severity = preds[, 2]), aes(x = Predicted_Severity)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  ggtitle("Histogram of Predicted Severity") +
  xlab("Predicted Severity") +
  theme_minimal()
ggsave("hist_predicted_severity.png", p_hist_severity, width = 8, height = 6, dpi = 300)

resid_data <- data.frame(
  Actual_Severity = y_severity_test,
  Predicted_Severity = preds[, 2],
  Zero = y_test[, 1]
)
resid_data <- resid_data[resid_data$Zero == 0, ]
resid_data$Residual <- resid_data$Predicted_Severity - resid_data$Actual_Severity
p_resid <- ggplot(resid_data, aes(x = Actual_Severity, y = Residual)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Residual Plot for Severity (Loss Occurred)") +
  xlab("Actual Severity") +
  ylab("Residual (Predicted - Actual)") +
  theme_minimal()
ggsave("residual_severity.png", p_resid, width = 8, height = 6, dpi = 300)

calib_data <- data.frame(
  p_zero = p_zero,
  Actual_NoLoss = y_test[, 1]
)
p_calib <- ggplot(calib_data, aes(x = p_zero, y = Actual_NoLoss)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "blue") +
  ggtitle("Calibration Plot for No Loss Prediction") +
  xlab("Predicted Probability of No Loss") +
  ylab("Actual No Loss Indicator") +
  theme_minimal()
ggsave("calibration_noloss.png", p_calib, width = 8, height = 6, dpi = 300)
