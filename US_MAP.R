# Load necessary libraries
library(ggplot2)
library(dplyr)
library(maps)
library(gganimate)
library(readr)
library(transformr)
library(viridis)

# Load your data
data <- read_csv("/Users/test/Documents/GitHub/future_damage_property_predictions.csv")

# Data preparation
# Convert BEGIN_YEARMONTH to a Date format for plotting
data <- data %>%
  mutate(Date = as.Date(paste0(BEGIN_YEARMONTH, "01"), format = "%Y%m%d"),
         STATE = tolower(STATE)) # Ensure states are lowercase for matching

# Load the US state map
us_map <- map_data("state")

# Prepare the data by merging map data with your dataset
data_plot <- us_map %>%
  left_join(data, by = c("region" = "STATE"))

# Plotting and animation with a more noticeable color scale
p <- ggplot(data = data_plot, aes(x = long, y = lat, group = group, fill = Predicted_DAMAGE_PROPERTY)) +
  geom_polygon(color = "black", size = 0.1) +
  scale_fill_viridis(option = "C", na.value = "grey90", 
                     breaks = seq(min(data$Predicted_DAMAGE_PROPERTY, na.rm = TRUE), 
                                  max(data$Predicted_DAMAGE_PROPERTY, na.rm = TRUE), by = 0.5),
                     labels = scales::label_number(accuracy = 0.1)) +
  coord_fixed(1.3) +
  labs(title = "Predicted Tornado Damages by State: {frame_time}",
       fill = "Damage\n(log scale)") +
  theme_minimal() +
  transition_time(Date) +
  ease_aes('linear')

# Save the animation
anim_save("tornado_damage_evolution_colored.gif", animate(p, nframes = 100, fps = 10, width = 800, height = 600))
