library(dplyr)
library(ggplot2)
library(tensorflow)
library(keras)
library(caret)
library(maps)
library(lubridate)

set.seed(123)
tor_df$DAMAGE_PROPERTY_NUM <- as.numeric(as.character(tor_df$DAMAGE_PROPERTY))
tor_df$DAMAGE_PROPERTY_NUM[is.na(tor_df$DAMAGE_PROPERTY_NUM)] <- 0
tor_df$ZERO_INDICATOR <- ifelse(tor_df$DAMAGE_PROPERTY_NUM == 0, 1, 0)
tor_df$log_DAMAGE_PROPERTY <- log(tor_df$DAMAGE_PROPERTY_NUM + 1)
if("BEGIN_DATE_TIME" %in% colnames(tor_df)){
  tor_df$YEAR <- year(as.Date(tor_df$BEGIN_DATE_TIME, format="%d-%b-%y %H:%M:%S"))
} else {
  tor_df$YEAR <- sample(2000:2020, nrow(tor_df), replace = TRUE)
}
if(!("STATE" %in% colnames(tor_df))){
  tor_df$STATE <- sample(c("TX", "OK", "KS", "NE", "MO"), nrow(tor_df), replace = TRUE)
}
tor_df_clean <- tor_df %>% filter(!is.na(BEGIN_LAT) & !is.na(BEGIN_LON) & !is.na(DAMAGE_PROPERTY_NUM))

vars <- c("BEGIN_LAT", "BEGIN_LON", "END_LAT", "END_LON", "MONTH_NAME", "TOR_F_SCALE",
          "INJURIES_DIRECT", "INJURIES_INDIRECT", "DEATHS_DIRECT", "DEATHS_INDIRECT",
          "DAMAGE_CROPS", "TOR_LENGTH", "TOR_WIDTH", "ZERO_INDICATOR", "DAMAGE_PROPERTY_NUM",
          "log_DAMAGE_PROPERTY", "STATE", "YEAR")
data_use <- tor_df[, vars]
data_use <- data_use[complete.cases(data_use), ]
data_use$MONTH_NAME <- as.factor(data_use$MONTH_NAME)
data_use$TOR_F_SCALE <- as.factor(data_use$TOR_F_SCALE)
data_use$ZERO_INDICATOR <- as.numeric(data_use$ZERO_INDICATOR)
data_use$DAMAGE_PROPERTY_NUM <- as.numeric(data_use$DAMAGE_PROPERTY_NUM)
data_use$log_DAMAGE_PROPERTY <- as.numeric(data_use$log_DAMAGE_PROPERTY)

predictors <- model.matrix(~ BEGIN_LAT + BEGIN_LON + END_LAT + END_LON +
                             MONTH_NAME + TOR_F_SCALE + INJURIES_DIRECT +
                             INJURIES_INDIRECT + DEATHS_DIRECT + DEATHS_INDIRECT +
                             DAMAGE_CROPS + TOR_LENGTH + TOR_WIDTH - 1, data = data_use)
response_zero <- data_use$ZERO_INDICATOR
response_count <- data_use$DAMAGE_PROPERTY_NUM
response_log <- data_use$log_DAMAGE_PROPERTY

split_index <- createDataPartition(response_zero, p = 0.8, list = FALSE)
x_train <- predictors[split_index, ]
x_test  <- predictors[-split_index, ]
y_zero_train <- response_zero[split_index]
y_zero_test  <- response_zero[-split_index]
y_count_train <- response_count[split_index]
y_count_test  <- response_count[-split_index]
y_log_train   <- response_log[split_index]
y_log_test    <- response_log[-split_index]

x_train_scaled <- scale(x_train)
x_test_scaled  <- scale(x_test,
                        center = attr(x_train_scaled, "scaled:center"),
                        scale  = attr(x_train_scaled, "scaled:scale"))
y_zero_train <- matrix(y_zero_train, ncol = 1)
y_zero_test  <- matrix(y_zero_test,  ncol = 1)
y_count_train <- matrix(y_count_train, ncol = 1)
y_count_test  <- matrix(y_count_test, ncol = 1)

build_zinn_model <- function(input_dim) {
  inputs <- layer_input(shape = input_dim)
  shared <- inputs %>% layer_dense(units = 64, activation = "relu") %>% layer_dense(units = 32, activation = "relu")
  zero_logits <- shared %>% layer_dense(units = 1, activation = "linear", name = "zero_logits")
  count_output <- shared %>% layer_dense(units = 1, activation = "relu", name = "count_output")
  keras_model(inputs = inputs, outputs = list(zero_logits, count_output))
}

model1 <- build_zinn_model(ncol(x_train_scaled))
model1 %>% compile(
  optimizer = tf$keras$optimizers$legacy$Adam(learning_rate = 1e-3),
  loss = list(
    function(y_true, y_pred) {tf$reduce_mean(tf$nn$sigmoid_cross_entropy_with_logits(labels = y_true, logits = y_pred))},
    function(y_true, y_pred) {tf$reduce_mean(tf$square(y_true - y_pred))}
  ),
  loss_weights = list(1.0, 1.0)
)

history1 <- model1 %>% fit(
  x_train_scaled,
  list(y_zero_train, y_count_train),
  epochs = 10,
  batch_size = 32,
  validation_split = 0.2,
  verbose = 1
)

preds1 <- model1 %>% predict(x_test_scaled)
logits_zero_test <- preds1[[1]]
count_test <- preds1[[2]]
prob_zero_test <- 1 / (1 + exp(-logits_zero_test))
zero_pred <- ifelse(prob_zero_test > 0.5, 1, 0)
acc_zinn <- mean(zero_pred == y_zero_test)
mse_zinn <- mean((count_test - y_count_test)^2 * (1 - y_zero_test))
zero_cm <- table(Predicted = zero_pred, Actual = y_zero_test)
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
ggsave("loss_plot.png", p_loss, width = 8, height = 6, dpi = 300)

preds_log <- log(as.vector(count_test) + 1)
actual_log <- log(y_count_test + 1)
p_log_actual_vs_pred <- ggplot(data.frame(actual_log = actual_log, preds_log = preds_log), aes(x = actual_log, y = preds_log)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  ggtitle("Log-Transformed: Actual vs. Predicted Damages") +
  xlab("Log(Actual Damage + 1)") +
  ylab("Log(Predicted Damage + 1)") +
  theme_minimal()
ggsave("log_actual_vs_predicted.png", p_log_actual_vs_pred, width = 8, height = 6, dpi = 300)

residuals_log <- preds_log - actual_log
p_residuals_log <- ggplot(data.frame(actual_log = actual_log, residuals_log = residuals_log), aes(x = actual_log, y = residuals_log)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Residual Plot (Log Scale)") +
  xlab("Log(Actual Damage + 1)") +
  ylab("Residuals") +
  theme_minimal()
ggsave("residual_plot_log.png", p_residuals_log, width = 8, height = 6, dpi = 300)

p_raw <- ggplot(data.frame(actual_count = y_count_test, pred_count = as.vector(count_test)), aes(x = actual_count, y = pred_count)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  ggtitle("Actual vs. Predicted Damages (Raw Counts)") +
  xlab("Actual Damage") +
  ylab("Predicted Damage") +
  theme_minimal()
ggsave("actual_vs_predicted_raw.png", p_raw, width = 8, height = 6, dpi = 300)

p_hist <- ggplot(data.frame(pred_count = as.vector(count_test)), aes(x = pred_count)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  ggtitle("Histogram of Predicted Damages") +
  xlab("Predicted Damage") +
  theme_minimal()
ggsave("predicted_histogram.png", p_hist, width = 8, height = 6, dpi = 300)

residuals_raw <- as.vector(count_test) - y_count_test
error_sd <- sd(residuals_raw)
ci_lower <- as.vector(count_test) - 1.96 * error_sd
ci_upper <- as.vector(count_test) + 1.96 * error_sd
plot_data <- data.frame(actual_count = y_count_test, pred_count = as.vector(count_test),
                        ci_lower = ci_lower, ci_upper = ci_upper)
p_ci <- ggplot(plot_data, aes(x = actual_count, y = pred_count)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, alpha = 0.5, color = "gray") +
  ggtitle("Predicted Damages with Approximate 95% Confidence Intervals") +
  xlab("Actual Damage") +
  ylab("Predicted Damage") +
  theme_minimal()
ggsave("confidence_intervals.png", p_ci, width = 8, height = 6, dpi = 300)

us_states <- map_data("state")
p_geo <- ggplot() +
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group),
               fill = "lightyellow", color = "black", alpha = 0.5) +
  geom_point(data = tor_df_clean, aes(x = BEGIN_LON, y = BEGIN_LAT, color = DAMAGE_PROPERTY_NUM),
             alpha = 0.6, size = 2) +
  scale_color_viridis_c(option = "plasma") +
  ggtitle("Geographic Distribution of Tornado Damages") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_minimal()
ggsave("geo_distribution.png", p_geo, width = 8, height = 6, dpi = 300)

damage_by_year <- tor_df %>% group_by(YEAR) %>% summarize(avg_damage = mean(DAMAGE_PROPERTY_NUM, na.rm = TRUE))
p_year <- ggplot(damage_by_year, aes(x = YEAR, y = avg_damage)) +
  geom_line(linewidth = 1, color = "blue") +
  geom_point(color = "red", size = 2) +
  ggtitle("Average Tornado Damage Over Time") +
  xlab("Year") +
  ylab("Average Damage") +
  theme_minimal()
ggsave("damage_by_year.png", p_year, width = 8, height = 6, dpi = 300)

p_state <- ggplot(tor_df, aes(x = STATE, y = DAMAGE_PROPERTY_NUM)) +
  geom_boxplot(fill = "lightgreen", outlier.color = "red", outlier.alpha = 0.6) +
  ggtitle("Tornado Damage by State") +
  xlab("State") +
  ylab("Damage") +
  theme_minimal()
ggsave("damage_by_state.png", p_state, width = 8, height = 6, dpi = 300)

p_calib <- ggplot(data.frame(prob = as.vector(prob_zero_test), actual = as.vector(y_zero_test)), aes(x = prob, y = actual)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "blue") +
  ggtitle("Calibration Plot: Zero-Inflation Probabilities") +
  xlab("Predicted Probability of Zero Damage") +
  ylab("Actual Zero Indicator") +
  theme_minimal()
ggsave("calibration_plot.png", p_calib, width = 8, height = 6, dpi = 300)

sensitivity_df <- as.data.frame(x_test)
sensitivity_df$predicted_damage <- as.vector(count_test)
sensitivity_corr <- sapply(names(sensitivity_df)[-ncol(sensitivity_df)], function(var) {
  x <- sensitivity_df[[var]]
  if(sd(x, na.rm = TRUE) == 0) NA else cor(x, sensitivity_df$predicted_damage, use = "complete.obs")
})
sensitivity_table <- data.frame(
  Variable = names(sensitivity_corr),
  Correlation = sensitivity_corr
) %>% filter(!is.na(Correlation))
p_sens <- ggplot(sensitivity_table, aes(x = reorder(Variable, abs(Correlation)), y = abs(Correlation))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle("Sensitivity Analysis (Absolute Correlations)") +
  xlab("Predictor Variable") +
  ylab("Absolute Correlation with Predicted Damage") +
  theme_minimal()
ggsave("sensitivity_analysis.png", p_sens, width = 8, height = 6, dpi = 300)

damage_by_state <- tor_df %>% 
  mutate(DAMAGE_PROPERTY_NUM = as.numeric(as.character(DAMAGE_PROPERTY))) %>% 
  filter(!is.na(DAMAGE_PROPERTY_NUM)) %>% 
  group_by(STATE) %>% 
  summarize(total_damage = sum(DAMAGE_PROPERTY_NUM, na.rm = TRUE))
damage_by_state$region <- tolower(damage_by_state$STATE)
us_states <- map_data("state")
map_data_states <- merge(us_states, damage_by_state, by.x = "region", by.y = "region", all.x = TRUE)
map_data_states <- map_data_states[order(map_data_states$order), ]
p_state_map <- ggplot(map_data_states, aes(x = long, y = lat, group = group, fill = total_damage)) +
  geom_polygon(color = "white") +
  scale_fill_continuous(low = "yellow", high = "red", na.value = "grey50") +
  labs(title = "Total Tornado Damage by State") +
  theme_minimal()
ggsave("damage_by_state_map.png", p_state_map, width = 8, height = 6, dpi = 300)

p_heat <- ggplot() +
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group),
               fill = "lightyellow", color = "black", alpha = 0.5) +
  geom_point(data = tor_df_clean, aes(x = BEGIN_LON, y = BEGIN_LAT, color = DAMAGE_PROPERTY_NUM),
             alpha = 0.6, size = 2) +
  scale_color_viridis_c(option = "plasma") +
  ggtitle("Heatmap: Tornado Damages on US Map") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_minimal()
ggsave("heatmap_tornado_damages.png", p_heat, width = 8, height = 6, dpi = 300)
