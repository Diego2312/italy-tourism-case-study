# Data source: https://data.worldbank.org/indicator/SL.UEM.TOTL.ZS

# Unemployment, total (% of total labor force) (modeled ILO estimate)

# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(tidyr)

# Define data path
data_path <- "data/raw/Unemployement_full.xls"

# Read data
un_data_raw <- read_excel(data_path, sheet = "Data", skip = 3)

# Inspect data
View(un_data_raw)

#Clean data

# Drop unnecessary columns
#Select only relevant columns: Country Name and years 1990-2024
un_data <- un_data_raw %>%
  select(`Country Name`, `1990`:`2024`)

# Countries to study
final_countries = c('Germany', 'United Kingdom', 'France', 'Austria', 'United States', 'Japan', 'China', 'Brazil', 'Canada', 'Australia', 'Turkiye', 'South Africa', 'Russian Federation')
final_countries

# Filter for final countries
un_data <- un_data %>%
  filter(`Country Name` %in% final_countries)

# Rename Russian Federation to Russia and Turkiye to Turkey
un_data$`Country Name`[un_data$`Country Name` == 'Russian Federation'] <- 'Russia'
un_data$`Country Name`[un_data$`Country Name` == 'Turkiye'] <- 'Turkey'


#Pivot data so that years are in a single column
un_data_pivoted <- un_data %>%
  pivot_longer(
    cols = `1990`:`2024`,
    names_to = "Year",
    values_to = "Rate"
  ) %>%
  pivot_wider(
    names_from = `Country Name`,
    values_from = Rate
  )

View(un_data)
View(un_data_pivoted)

#Save data
write.xlsx(un_data_pivoted, file = "data/processed/processed_unemployement.xlsx", sheetName = "Unemployement Data", overwrite = TRUE)

