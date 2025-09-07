#Data source: https://data.ecb.europa.eu/data/data-categories/ecbeurosystem-policy-and-exchange-rates/exchange-rates/reference-rates?searchTerm=&resetAllFilters=true
# Exchange Rates (Reference rates)

# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(scales)
library(ggthemes)

# Define data path
data_path <- "data/raw/Exchange_rate_full.xlsx"

# Read data
exchange_data_raw <- read_excel(data_path, sheet = "DATA", skip = 14)

# View data
View(exchange_data_raw)


# Clean data

#View data
View(exchange_data)

# Drop first column
exchange_data <- exchange_data_raw %>%
  select(-1)

# Rename country columns
old_names <- names(exchange_data)
new_names <- sapply(strsplit(old_names, "/"), "[", 1) # Like splitting in python then taking index 0
names(exchange_data) <- new_names

# Rename first column to Year
colnames(exchange_data)[1] <- "Year"

#Create new columns for Germany, France and Austria 
exchange_data <- exchange_data %>%
  mutate(Germany = 1,
         France = 1,
         Austria = 1)

# Convert Year and exchange rates to numeric
exchange_data <- exchange_data %>%
  mutate(Year = as.numeric(Year),
         across(-Year, ~as.numeric(.)))

# Set first three rows of france to NA
exchange_data$France[1:3] <- NA

# Drop Korean won column
exchange_data <- exchange_data %>%
  select(-'Korean won (Republic)') 

# Save cleaned data
write.xlsx(exchange_data, file = "data/processed/processed_exchange_rates.xlsx", sheetName = "Exchange Rate Data", overwrite = TRUE)

