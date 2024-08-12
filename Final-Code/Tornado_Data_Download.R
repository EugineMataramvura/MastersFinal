# Load required libraries
library(rvest)
library(dplyr)

# Set the Storm Events URL
url <- "https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/"

# Read the webpage content
webpage <- read_html(url)

# Extract all file names ending with .csv.gz
filenames <- webpage %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  grep("StormEvents_details-ftp_v1.0_.*\\.csv\\.gz$", ., value = TRUE)

# Generate full URL of the files
full_paths <- paste0(url, filenames)

# Where it's going in your directory
local_prefix <- file.path("data", "raw")

# Create the directory if it does not exist
if (!dir.exists(local_prefix)) {
  dir.create(local_prefix, recursive = TRUE)
}

# Maintaining their original names in your directory
local_paths <- file.path(local_prefix, filenames)

# Download all of the data files if they don't already exist
for (i in seq_along(full_paths)) {
  if (!file.exists(local_paths[i])) {
    tryCatch(
      {
        download.file(full_paths[i], local_paths[i], mode = "wb")
        message(paste("Successfully downloaded:", filenames[i]))
      },
      error = function(e) {
        message(paste("Failed to download:", full_paths[i]))
      }
    )
  }
}
