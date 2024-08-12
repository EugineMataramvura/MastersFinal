# Load necessary libraries
library(dplyr)
library(readr)

# Directory where the data files are stored
data_dir <- "/Users/test/Documents/GitHub/data/raw"

# List all .csv.gz files
compressed_files <- list.files(path = data_dir, pattern = ".csv.gz", full.names = TRUE)

# Function to read and standardize column types
read_and_standardize <- function(file) {
  cat("Processing file:", file, "\n")
  
  df <- read_csv(file, col_types = cols(
    BEGIN_YEARMONTH = col_double(),
    BEGIN_DAY = col_double(),
    BEGIN_TIME = col_double(),
    END_YEARMONTH = col_double(),
    END_DAY = col_double(),
    END_TIME = col_double(),
    EPISODE_ID = col_double(),
    EVENT_ID = col_double(),
    STATE = col_character(),
    STATE_FIPS = col_double(),
    YEAR = col_double(),
    MONTH_NAME = col_character(),
    EVENT_TYPE = col_character(),
    CZ_TYPE = col_character(),
    CZ_FIPS = col_double(),
    CZ_NAME = col_character(),
    WFO = col_character(),
    BEGIN_DATE_TIME = col_character(),
    CZ_TIMEZONE = col_character(),
    END_DATE_TIME = col_character(),
    INJURIES_DIRECT = col_double(),
    INJURIES_INDIRECT = col_double(),
    DEATHS_DIRECT = col_double(),
    DEATHS_INDIRECT = col_double(),
    DAMAGE_PROPERTY = col_character(),  # Keep as character for now
    DAMAGE_CROPS = col_character(),     # Keep as character for now
    SOURCE = col_character(),
    MAGNITUDE = col_double(),
    MAGNITUDE_TYPE = col_character(),
    FLOOD_CAUSE = col_character(),
    CATEGORY = col_character(),
    TOR_F_SCALE = col_character(),
    TOR_LENGTH = col_double(),
    TOR_WIDTH = col_double(),
    TOR_OTHER_WFO = col_character(),
    TOR_OTHER_CZ_STATE = col_character(),
    TOR_OTHER_CZ_FIPS = col_character(),
    TOR_OTHER_CZ_NAME = col_character(),
    BEGIN_RANGE = col_double(),
    BEGIN_AZIMUTH = col_character(),
    BEGIN_LOCATION = col_character(),
    END_RANGE = col_double(),
    END_AZIMUTH = col_character(),
    END_LOCATION = col_character(),
    BEGIN_LAT = col_double(),
    BEGIN_LON = col_double(),
    END_LAT = col_double(),
    END_LON = col_double(),
    EPISODE_NARRATIVE = col_character(),
    EVENT_NARRATIVE = col_character(),
    DATA_SOURCE = col_character()
  ))
  
  # Convert DAMAGE_PROPERTY and DAMAGE_CROPS to numeric after removing letters
  df$DAMAGE_PROPERTY <- as.numeric(gsub("[^0-9.]", "", df$DAMAGE_PROPERTY))
  df$DAMAGE_CROPS <- as.numeric(gsub("[^0-9.]", "", df$DAMAGE_CROPS))
  
  return(df)
}

# Read and combine all files
tor_df <- lapply(compressed_files, read_and_standardize) %>%
  bind_rows()

# Check the structure of the combined data
str(tor_df)
