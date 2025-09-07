# Data source: https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD?name_desc=false&locations=AU-BR-AT-CA-CN-FR-DE-JP-IT-ZA-GB-US
# GDP per capita, PPP (current international $)



# Define data path
data_path <- "data/raw/GDP_full.xls"

# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(tidyr)


# Read data
gdp_data_raw <- read_excel(data_path, sheet = "Data", skip = 3)

# Inspect data
#View(gdp_data_raw)

#Clean data

# Drop unnecessary columns
#Select only relevant columns: Country Name and years 1990-2024
gdp_data <- gdp_data_raw %>%
  select(`Country Name`, `1990`:`2024`)

# Countries to study
final_countries = c('Germany', 'United Kingdom', 'France', 'Austria', 'United States', 'Japan', 'China', 'Brazil', 'Canada', 'Australia', 'Turkiye', 'South Africa', 'Russian Federation')
final_countries

# Filter for final countries
gdp_data <- gdp_data %>%
  filter(`Country Name` %in% final_countries)

# Rename Russian Federation to Russia and Turkiye to Turkey
gdp_data$`Country Name`[gdp_data$`Country Name` == 'Russian Federation'] <- 'Russia'
gdp_data$`Country Name`[gdp_data$`Country Name` == 'Turkiye'] <- 'Turkey'


#Pivot data so that years are in a single column
gdp_data_pivoted <- gdp_data %>%
  pivot_longer(
    cols = `1990`:`2024`,
    names_to = "Year",
    values_to = "GDP"
  ) %>%
  pivot_wider(
    names_from = `Country Name`,
    values_from = GDP
  )

View(gdp_data)
View(gdp_data_pivoted)

#Save data
write.xlsx(gdp_data_pivoted, file = "data/processed/processed_gdp.xlsx", sheetName = "GDP Data", overwrite = TRUE)

#Create a new GDP increase/decrease data frame

#Create a new column for each country showing the increase/decrease in GDP from 1990 to 2024 in percentage
gdp_change <- gdp_data_pivoted %>%
  mutate(across(-`Year`, ~ (. - first(.)) / first(.) * 100, .names = "pct_change_{col}"))

View(gdp_change)

#Keep only the percentage change columns and the Year column
gdp_change <- gdp_change %>%
  select(`Year`, starts_with("pct_change_"))

#Rename the percentage change columns to remove the "pct_change_" prefix
colnames(gdp_change) <- gsub("pct_change_", "", colnames(gdp_change))
