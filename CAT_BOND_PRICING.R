# Load necessary libraries
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(purrr)

# Load the data
file_path <- "/Users/test/Documents/GitHub/future_damage_property_predictions.csv"
tornado_data <- read_csv(file_path)

# Define parameters
risk_free_rate <- 0.03  # Example annual risk-free rate
face_value <- 100

# Function to simulate interest rates using the Vasicek model
simulate_vasicek <- function(a, b, sigma, r0, T, dt) {
  n <- T / dt
  rates <- numeric(n)
  rates[1] <- r0
  for (i in 2:n) {
    dr <- a * (b - rates[i-1]) * dt + sigma * sqrt(dt) * rnorm(1)
    rates[i] <- rates[i-1] + dr
  }
  return(rates)
}

# Function to calculate the discount factor
calculate_discount_factors <- function(rates, dt) {
  exp(-cumsum(rates) * dt)
}

# Function to calculate bond payoff with a piecewise linear function
calculate_payoff <- function(predicted_damage, face_value = 100, thresholds = c(1, 2, 3), reductions = c(0.1, 0.3, 0.5)) {
  reduction_percentage <- sum(sapply(1:length(thresholds), function(i) {
    ifelse(predicted_damage > thresholds[i], reductions[i], 0)
  }))
  payoff <- face_value * (1 - reduction_percentage)
  return(payoff)
}

# Function to simulate bond prices using Monte Carlo simulation
simulate_bond_prices <- function(data, num_simulations = 1000, a = 0.1, b = 0.03, sigma = 0.02, r0 = 0.03, T = 1, dt = 1/12) {
  # Initialize a list to store simulation results
  simulation_results <- vector("list", num_simulations)
  
  # Loop through each simulation
  for (sim in 1:num_simulations) {
    # Simulate interest rates
    rates <- simulate_vasicek(a, b, sigma, r0, T, dt)
    discount_factors <- calculate_discount_factors(rates, dt)
    
    # Calculate payoffs and discounted bond prices
    simulation_results[[sim]] <- data %>%
      mutate(Payoff = map_dbl(Predicted_DAMAGE_PROPERTY, calculate_payoff, face_value),
             Bond_Price = Payoff * discount_factors[length(discount_factors)])
  }
  
  # Combine results into a single dataframe
  simulation_df <- bind_rows(simulation_results, .id = "Simulation")
  return(simulation_df)
}

# Run the bond price simulation
simulated_data <- simulate_bond_prices(tornado_data, num_simulations = 1000, a = 0.1, b = 0.03, sigma = 0.02, r0 = 0.03, T = 1, dt = 1/12)

# Ensure that BEGIN_YEARMONTH is a character and then convert to Date
simulated_data <- simulated_data %>%
  mutate(BEGIN_YEARMONTH = as.Date(paste0(BEGIN_YEARMONTH, "01"), "%Y%m%d"))

# Calculate average bond prices for each month
average_bond_prices <- simulated_data %>%
  group_by(BEGIN_YEARMONTH) %>%
  summarize(Average_Bond_Price = mean(Bond_Price))

# Visualize the average bond prices over time
ggplot(average_bond_prices, aes(x = BEGIN_YEARMONTH, y = Average_Bond_Price)) +
  geom_line() +
  labs(title = "Average Bond Prices Over Time", x = "Date", y = "Average Bond Price") +
  theme_minimal()

# Calculate the distribution of bond prices
bond_price_distribution <- simulated_data %>%
  group_by(Simulation) %>%
  summarize(Average_Bond_Price = mean(Bond_Price))

# Function for VaR and CVaR analysis
calculate_var_cvar <- function(prices, confidence_level = 0.95) {
  sorted_prices <- sort(prices)
  index <- floor((1 - confidence_level) * length(sorted_prices))
  var <- sorted_prices[index]
  cvar <- mean(sorted_prices[1:index])
  return(list(VaR = var, CVaR = cvar))
}

# Perform VaR and CVaR analysis
var_cvar_results <- calculate_var_cvar(bond_price_distribution$Average_Bond_Price)

# Print VaR and CVaR results
print(paste("Value-at-Risk (VaR):", var_cvar_results$VaR))
print(paste("Conditional Value-at-Risk (CVaR):", var_cvar_results$CVaR))

# Simulate Vasicek interest rates
simulate_vasicek <- function(T, dt, a, b, sigma, r0) {
  n <- round(T/dt)
  rates <- numeric(n)
  rates[1] <- r0
  for (i in 2:n) {
    rates[i] <- rates[i-1] + a * (b - rates[i-1]) * dt + sigma * sqrt(dt) * rnorm(1)
  }
  return(rates)
}

# Calculate discount factors from Vasicek rates
calculate_discount_factors <- function(rates, dt) {
  disc_factors <- exp(-cumsum(rates) * dt)
  return(disc_factors)
}

# Adjust bond price simulation to include Vasicek model discounting
simulate_bond_prices_vasicek <- function(data, num_simulations = 1000, T = 1, dt = 1/12, 
                                         a = 0.1, b = 0.03, sigma = 0.02, r0 = 0.03) {
  simulation_results <- vector("list", num_simulations)
  
  for (sim in 1:num_simulations) {
    rates <- simulate_vasicek(T, dt, a, b, sigma, r0)
    discount_factors <- calculate_discount_factors(rates, dt)
    
    simulation_results[[sim]] <- data %>%
      mutate(Payoff = map_dbl(Predicted_DAMAGE_PROPERTY, calculate_payoff, face_value),
             Discounted_Payoff = Payoff * discount_factors[length(discount_factors)])
  }
  
  simulation_df <- bind_rows(simulation_results, .id = "Simulation")
  return(simulation_df)
}

# Calculate average bond prices using the updated simulation function
simulated_data_vasicek <- simulate_bond_prices_vasicek(tornado_data)

# Calculate the average bond prices and visualize them
average_bond_prices_vasicek <- simulated_data_vasicek %>%
  group_by(BEGIN_YEARMONTH) %>%
  summarize(Average_Bond_Price = mean(Discounted_Payoff))

ggplot(average_bond_prices_vasicek, aes(x = BEGIN_YEARMONTH, y = Average_Bond_Price)) +
  geom_line() +
  labs(title = "Average Bond Prices Over Time (Vasicek Model)", x = "Date", y = "Average Bond Price") +
  theme_minimal()

