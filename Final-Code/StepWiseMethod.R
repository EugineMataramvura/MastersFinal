library(ggplot2)
library(dplyr)
library(gridExtra)
library(reshape2)
set.seed(123)

# Estimate empirical parameters from tor_df
nonzero_losses <- tor_df$DAMAGE_PROPERTY_NUM[tor_df$DAMAGE_PROPERTY_NUM > 0]
loss_mean_empirical <- mean(nonzero_losses)
loss_sd_empirical <- sd(nonzero_losses)
obs_years <- 10
lambda_empirical <- nrow(tor_df) / obs_years

# Create individual plots from tor_df
p_tor_hist <- ggplot(tor_df, aes(x = DAMAGE_PROPERTY_NUM)) + 
  geom_histogram(bins = 50, fill = "blue", alpha = 0.7) + 
  theme_minimal() + 
  ggtitle("Histogram of DAMAGE_PROPERTY_NUM (tor_df)")
p_tor_density <- ggplot(tor_df, aes(x = DAMAGE_PROPERTY_NUM)) + 
  geom_density(fill = "green", alpha = 0.7) + 
  theme_minimal() + 
  ggtitle("Density of DAMAGE_PROPERTY_NUM (tor_df)")
p_tor_qq <- ggplot(tor_df, aes(sample = DAMAGE_PROPERTY_NUM)) + 
  stat_qq() + stat_qq_line() + theme_minimal() + 
  ggtitle("QQ Plot of DAMAGE_PROPERTY_NUM (tor_df)")

# Save grid arrangement of tor_df plots as PNG
g1 <- arrangeGrob(p_tor_hist, p_tor_density, p_tor_qq, ncol = 2)
ggsave("tor_df_plots.png", g1, width = 12, height = 8, dpi = 300)

# Simulation parameters
nSim <- 10000
T <- 1
dt <- 1/252
nSteps <- as.integer(T/dt)
a <- 0.1
b <- 0.05
sigma_r <- 0.01
r0 <- 0.03
lambda <- lambda_empirical
loss_mean <- loss_mean_empirical
K <- 200
w <- 0.3
F_u <- 1000

discounted_payoffs <- numeric(nSim)
discounted_payoffs_linear <- numeric(nSim)
simulated_r_T <- numeric(nSim)
simulated_avg_r <- numeric(nSim)
simulated_loss <- numeric(nSim)
nEvents_all <- numeric(nSim)

for(sim in 1:nSim){
  r_path <- numeric(nSteps)
  r_path[1] <- r0
  for(i in 2:nSteps){
    dr <- a*(b - r_path[i-1])*dt + sigma_r*sqrt(dt)*rnorm(1)
    r_path[i] <- r_path[i-1] + dr
  }
  discount_factor <- exp(-sum(r_path)*dt)
  simulated_r_T[sim] <- r_path[nSteps]
  simulated_avg_r[sim] <- mean(r_path)
  nEvents <- rpois(1, lambda*T)
  nEvents_all[sim] <- nEvents
  if(nEvents > 0){
    losses <- rexp(nEvents, rate = 1/loss_mean)
    S_T <- sum(losses)
  } else {
    S_T <- 0
  }
  simulated_loss[sim] <- S_T
  if(S_T < K){
    payoff <- F_u
  } else {
    payoff <- F_u*(1 - w)
  }
  discounted_payoffs[sim] <- discount_factor * payoff
  K1 <- 150; K2 <- 250
  if(S_T <= K1){
    payoff_linear <- F_u
  } else if(S_T >= K2){
    payoff_linear <- F_u*(1 - w)
  } else {
    payoff_linear <- F_u - (F_u*w)*((S_T - K1)/(K2 - K1))
  }
  discounted_payoffs_linear[sim] <- discount_factor * payoff_linear
}

bond_price <- mean(discounted_payoffs)
price_std_err <- sd(discounted_payoffs)/sqrt(nSim)
bond_price_linear <- mean(discounted_payoffs_linear)
price_linear_std_err <- sd(discounted_payoffs_linear)/sqrt(nSim)

# Interest rate plots
df_interest <- data.frame(TerminalRate = simulated_r_T, AvgRate = simulated_avg_r)
df_interest_melt <- melt(df_interest)
p1 <- ggplot(df_interest_melt, aes(x = value, fill = variable)) +
  geom_histogram(alpha = 0.6, bins = 50) +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  ggtitle("Histogram of Terminal and Average Interest Rates")
p2 <- ggplot(df_interest, aes(x = TerminalRate)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  theme_minimal() +
  ggtitle("Density of Terminal Interest Rate")
p3 <- ggplot(df_interest, aes(sample = TerminalRate)) +
  stat_qq() + stat_qq_line() +
  theme_minimal() +
  ggtitle("QQ Plot of Terminal Interest Rate")

g2 <- arrangeGrob(p1, p2, p3, ncol = 2)
ggsave("interest_rate_plots.png", g2, width = 12, height = 8, dpi = 300)

# Catastrophic event and loss plots
df_events <- data.frame(Events = nEvents_all)
p4 <- ggplot(df_events, aes(x = Events)) +
  geom_bar(fill = "tomato", alpha = 0.7) +
  theme_minimal() +
  ggtitle("Number of Catastrophic Events (Simulated)")
df_losses <- data.frame(Loss = simulated_loss)
p5 <- ggplot(df_losses, aes(x = Loss)) +
  geom_histogram(fill = "forestgreen", bins = 50, alpha = 0.7) +
  theme_minimal() +
  ggtitle("Histogram of Cumulative Losses (Simulated)")
df_payoffs <- data.frame(Stepwise = discounted_payoffs, Piecewise = discounted_payoffs_linear)
p6 <- ggplot(df_payoffs, aes(x = Stepwise)) +
  geom_histogram(fill = "purple", bins = 50, alpha = 0.7) +
  theme_minimal() +
  ggtitle("Histogram of Discounted Payoffs (Stepwise)")
p7 <- ggplot(df_payoffs, aes(x = Piecewise)) +
  geom_histogram(fill = "orange", bins = 50, alpha = 0.7) +
  theme_minimal() +
  ggtitle("Histogram of Discounted Payoffs (Piecewise-Linear)")
p8 <- ggplot(df_payoffs, aes(x = Stepwise, y = Piecewise)) +
  geom_point(alpha = 0.3) +
  theme_minimal() +
  ggtitle("Scatter Plot: Stepwise vs. Piecewise-Linear")
p9 <- ggplot(df_payoffs, aes(sample = Stepwise)) +
  stat_qq() + stat_qq_line() +
  theme_minimal() +
  ggtitle("QQ Plot of Stepwise Discounted Payoffs")
p10 <- ggplot(df_payoffs, aes(sample = Piecewise)) +
  stat_qq() + stat_qq_line() +
  theme_minimal() +
  ggtitle("QQ Plot of Piecewise-Linear Discounted Payoffs")
df_discount <- data.frame(DiscountFactor = discounted_payoffs / F_u)
p11 <- ggplot(df_discount, aes(x = DiscountFactor)) +
  geom_histogram(fill = "steelblue", bins = 50, alpha = 0.7) +
  theme_minimal() +
  ggtitle("Histogram of Discount Factors")

g3 <- arrangeGrob(p4, p5, ncol = 2)
ggsave("events_losses.png", g3, width = 12, height = 6, dpi = 300)
g4 <- arrangeGrob(p6, p7, p8, ncol = 3)
ggsave("discounted_payoffs_hist_scatter.png", g4, width = 12, height = 6, dpi = 300)
g5 <- arrangeGrob(p9, p10, ncol = 2)
ggsave("qq_plots_discounted_payoffs.png", g5, width = 12, height = 6, dpi = 300)
g6 <- arrangeGrob(p11, ncol = 1)
ggsave("discount_factors.png", g6, width = 8, height = 6, dpi = 300)

# Interest rate time series and sample path plots
sample_path_indices <- sample(1:nSim, 20)
df_paths <- data.frame(Time = rep(seq(0, T, length.out = nSteps), 20),
                       Rate = unlist(lapply(sample_path_indices, function(idx) {
                         r_path <- numeric(nSteps)
                         r_path[1] <- r0
                         for(i in 2:nSteps){ 
                           r_path[i] <- r_path[i-1] + a*(b - r_path[i-1])*dt + sigma_r*sqrt(dt)*rnorm(1)
                         }
                         r_path
                       })),
                       Path = rep(paste("Path", 1:20), each = nSteps))
p12 <- ggplot(df_paths, aes(x = Time, y = Rate, color = Path)) +
  geom_line(alpha = 0.8) +
  theme_minimal() +
  ggtitle("Sample Paths of the Vasicek Interest Rate")
p13 <- ggplot(data.frame(Index = 1:nSim, Stepwise = discounted_payoffs),
              aes(x = Index, y = Stepwise)) +
  geom_line(alpha = 0.5, color = "darkred") +
  theme_minimal() +
  ggtitle("Time Series of Discounted Payoffs (Stepwise)")
p14 <- ggplot(data.frame(Index = 1:nSim, TerminalRate = simulated_r_T),
              aes(x = Index, y = TerminalRate)) +
  geom_line(alpha = 0.5, color = "darkblue") +
  theme_minimal() +
  ggtitle("Time Series of Terminal Interest Rates")
g7 <- arrangeGrob(p12, p13, p14, ncol = 2)
ggsave("interest_rate_time_series_plots.png", g7, width = 12, height = 8, dpi = 300)

# Save remaining individual plots
ggsave("hist_predicted_severity.png", p_tor_hist, width = 8, height = 6, dpi = 300)  # Example for an individual plot

# Print simulation results and summaries
print(paste("Bond Price (Stepwise):", round(bond_price, 2)))
print(paste("Standard Error (Stepwise):", round(price_std_err, 2)))
print(paste("Bond Price (Piecewise):", round(bond_price_linear, 2)))
print(paste("Standard Error (Piecewise):", round(price_linear_std_err, 2)))
print(summary(simulated_r_T))
print(summary(simulated_avg_r))
print(summary(nEvents_all))
print(summary(simulated_loss))
print(summary(discounted_payoffs))
print(summary(discounted_payoffs_linear))
