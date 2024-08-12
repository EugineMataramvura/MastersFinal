# Catastrophe Bond Pricing Project

## Project Overview

This project aims to predict and analyze the impact of tornado occurrences on the pricing of catastrophe bonds. Using historical tornado data and neural networks, we simulate the future impact on bond prices, incorporating stochastic interest rates via the Vasicek model.

## Directory Structure

-   `Tornado_Data_Download.R`: Downloads tornado event data from NOAA.
-   `Tornado_Data_Processing.R`: Processes the downloaded data for modeling.
-   `Neural_Net.R`: Trains a neural network on the processed tornado data.
-   `CAT_BOND_PRICING.R`: Simulates bond prices using the neural network predictions.
-   `US_MAP.R`: Generates a GIF visualization of tornado occurrences over time.

## Installation and Setup

Before running the scripts, ensure R and RStudio are installed. Additionally, install the necessary R packages using the following command: \`\`\`r install.packages(c("rvest", "dplyr", "readr", "nnet", "caret", "ggplot2", "gganimate", "tidyr", "lubridate", "purrr"))

## Usage Instructions

### 1. Data Download

Run `Tornado_Data_Download.R` to fetch the latest tornado data from the NOAA website. This script will save compressed CSV files in the `data/raw` directory.

### 2. Data Processing

Execute `Tornado_Data_Processing.R` to prepare the data for analysis. This includes cleaning, type conversions, and preliminary analysis.

### 3. Neural Network Modeling

Use `Neural_Net.R` to train a neural network model. This script processes the data, applies transformations, trains the model, and evaluates its performance.

### 4. Bond Pricing Simulation

Run `CAT_BOND_PRICING.R` to simulate catastrophe bond prices based on the outputs from the neural network. This script also incorporates discounting bond payouts using the Vasicek model to simulate future interest rates.

### 5. Visualization of Tornado Data

`US_MAP.R` generates a GIF showing the progression of tornado occurrences over time, visualizing the data geographically across the United States.

## Data Sources

Data is sourced from the National Oceanic and Atmospheric Administration (NOAA) available [here](https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/).

## Contributing

We welcome contributions from the community. Please fork the repository and submit a pull request with your suggested changes.

## License

This project is open-sourced under the MIT License. See the LICENSE file for more details.

## Contact Information

For support or collaboration, please contact [euginematy\@gmail.com](mailto:euginematy@gmail.com).

## Acknowledgments

Thanks to NOAA for providing access to the storm data, and to all contributors who have worked on developing and testing this project.
