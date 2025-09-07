# Data source: https://databank.worldbank.org/source/jobs/Series/SP.POP.GROW#

# Population growth (annual %)


# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(scales)
library(ggthemes)



# Define data path
data_path <- "data/raw/Population_change_full.csv"

# Read data
pop_data_raw <- read.csv(data_path)

# View raw data
View(pop_data_raw)

# Clean data

# View data
View(pop_data)

# Select relevant columns
pop_data <- pop_data_raw %>%
  select(Location, Time, Value)

# Pivot to wider format
pop_data <- pop_data %>%
  pivot_wider(names_from = Location, values_from = Value)

# Rename first column to Year
colnames(pop_data)[1] <- "Year"

# Rename country columns

# Russian Federation to Russia
colnames(pop_data) <- gsub("Russian Federation", "Russia", colnames(pop_data))

# Türkiye to Turkey
colnames(pop_data) <- gsub("Türkiye", "Turkey", colnames(pop_data))

# Convert Year and median ages to numeric
pop_data <- pop_data %>%
  mutate(Year = as.numeric(Year),
         across(-Year, ~as.numeric(.)))

# View cleaned data
View(pop_data)

# Save cleaned data
cleaned_data_path <- "data/processed/processed_population_change.xlsx"
write.xlsx(pop_data, cleaned_data_path, rowNames = FALSE)
