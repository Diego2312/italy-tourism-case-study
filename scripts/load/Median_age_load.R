# Data souce: https://population.un.org/dataportal/home?df=45a1cf71-2b19-45a6-9852-5eddcc5cfaed
# Median Age Data


# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(scales)
library(ggthemes)

# Define data path
data_path <- "data/raw/Median_age_full.csv"

# Read data
median_age_data_raw <- read.csv(data_path)

# View data
View(median_age_data_raw)

# Clean data

# View data
View(median_age_data)

# Select relevant columns
median_age_data <- median_age_data_raw %>%
  select(Location, Time, Value)

# Pivot to wider format
median_age_data <- median_age_data %>%
  pivot_wider(names_from = Location, values_from = Value)

# Rename first column to Year
colnames(median_age_data)[1] <- "Year"

# Rename country columns

# Russian Federation to Russia
colnames(median_age_data) <- gsub("Russian Federation", "Russia", colnames(median_age_data))

# Türkiye to Turkey
colnames(median_age_data) <- gsub("Türkiye", "Turkey", colnames(median_age_data))

# Convert Year and median ages to numeric
median_age_data <- median_age_data %>%
  mutate(Year = as.numeric(Year),
         across(-Year, ~as.numeric(.)))

# View cleaned data
View(median_age_data)

# Save cleaned data
cleaned_data_path <- "data/processed/processed_median_age.xlsx"
write.xlsx(median_age_data, cleaned_data_path, rowNames = FALSE)
