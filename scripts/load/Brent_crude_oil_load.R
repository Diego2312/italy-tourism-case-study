# Data source: https://www.worldbank.org/en/research/commodity-markets
# Brent Crude Oil Prices (Nominal, USD per barrel)

# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(scales)
library(ggthemes)

# Define data path
data_path <- "data/raw/Brent_crude_oil_full.xlsx"

# Read data
oil_data_raw <- read_excel(data_path, sheet = "Annual Prices (Real)", skip = 6)

# View data
View(oil_data_raw)

# Clean data 

# Filter for only Crude oil, Brent (keep first and third column)
oil_data <- oil_data_raw %>%
  select(1, "Crude oil, Brent")

# Rename columns
colnames(oil_data) <- c("Year", "Brent_Crude_Oil_USD_per_barrel")

# Filter for years 1990-2024
oil_data <- oil_data %>%
  filter(Year >= 1990 & Year <= 2024)

#Convert Year and Brent_Crude_Oil to numeric
oil_data <- oil_data %>%
  mutate(Year = as.numeric(Year),
         Brent_Crude_Oil_USD_per_barrel = as.numeric(Brent_Crude_Oil_USD_per_barrel))

#View cleaned data
View(oil_data)

#Save cleaned data
write.xlsx(oil_data, "data/processed/processed_brent_crude_oil.xlsx", rowNames = FALSE, overwrite = TRUE)
