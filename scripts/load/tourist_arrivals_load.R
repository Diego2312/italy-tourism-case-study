#Load libary to inspect exel data
library(readxl)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(scales)
library(ggthemes)

install.packages("ggthemes")

#Data source
# https://ec.europa.eu/eurostat/web/tourism/database

#Define data path
data_filtered_path = "data/raw/tourist_arrivals_yearly_filtered.xlsx"

#Read data
tourism_data_filtered_yearly <- read_excel(data_filtered_path, sheet = "Sheet 1", skip = 7)


#View(tourism_data_filtered_yearly)

#Data info
head(tourism_data_filtered_yearly)
colnames(tourism_data_filtered_yearly)

#Data cleaning ===============================================

#Drop second column
tourism_data_filtered_yearly <- tourism_data_filtered_yearly[, -2]

#Drop first row
tourism_data_filtered_yearly <- tourism_data_filtered_yearly[-1, ]

#Drop last 3 rows
num_rows = nrow(tourism_data_filtered_yearly)
tourism_data_filtered_yearly <- tourism_data_filtered_yearly[1:(num_rows - 3), ]


#Rename first column to Year
colnames(tourism_data_filtered_yearly)[1] <- "Year"

#Column values to numeric
tourism_data_filtered_yearly[] <- lapply(tourism_data_filtered_yearly, function(x) as.numeric(as.character(x)))


# =============================================================



#Study tourist arrivals since 1990, 2000, 2010 =====================

#Sum columns to find countries with highest tourist arrivals since 1990
column_sums_90s = colSums(tourism_data_filtered_yearly, na.rm = TRUE)
column_sums_df_90s = as.data.frame(column_sums_90s)
column_sums_ordered_90s <- column_sums_df_90s %>%
  arrange(desc(column_sums_90s))
top_10_90s <- head(column_sums_ordered_90s, 13)
top_10_90s = tail(top_10_90s, 10)

#Sum columns to find countries with highest tourist arrivals since 2000
column_sums_00s = colSums(tourism_data_filtered_yearly[tourism_data_filtered_yearly$Year >= 2000, ], na.rm = TRUE)
column_sums_df_00s = as.data.frame(column_sums_00s)
column_sums_ordered_00s <- column_sums_df_00s %>%
  arrange(desc(column_sums_00s))
top_10_00s <- head(column_sums_ordered_00s, 13)
top_10_00s = tail(top_10_00s, 10)



#Sum columns to find countries with highest tourist arrivals since 2010
column_sums_10s = colSums(tourism_data_filtered_yearly[tourism_data_filtered_yearly$Year >= 2010, ], na.rm = TRUE)
column_sums_df_10s = as.data.frame(column_sums_10s)
column_sums_ordered_10s <- column_sums_df_10s %>%
  arrange(desc(column_sums_10s))
top_10_10s <- head(column_sums_ordered_10s, 13)
top_10_10s = tail(top_10_10s, 10)


#Bar plot of top 10 countries since 1990
top_10_90s <- top_10_90s %>%
  mutate(Country = rownames(.))
top_10_90s_plot = ggplot(top_10_90s, aes(x = reorder(Country, column_sums_90s), y = column_sums_90s)) +
  geom_col(fill = "steelblue") +
  coord_flip() + # Flip the coordinates to make the bars horizontal
  labs(
    title = "Top 10 Tourist Arrivals (Since 1990)",
    x = "Country",
    y = "Total Tourist Arrivals"
  ) +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(8,8,8,8), "pt"))
top_10_90s_plot

#Bar plot of top 10 countries since 2000
top_10_00s <- top_10_00s %>%
  mutate(Country = rownames(.))
top_10_00s_plot = ggplot(top_10_00s, aes(x = reorder(Country, column_sums_00s), y = column_sums_00s)) +
  geom_col(fill = "steelblue") +
  coord_flip() + # Flip the coordinates to make the bars horizontal
  labs(
    title = "Top 10 Tourist Arrivals (Since 2000)",
    x = "Country",
    y = "Total Tourist Arrivals"
  ) +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(8,8,8,8), "pt"))
top_10_00s_plot

#Bar plot of top 10 countries since 2010
top_10_10s <- top_10_10s %>%
  mutate(Country = rownames(.))

# Rename Other Asian countries (aggregate changing according to the context) Other Asian countries (aggregate changing according to the context) to "Other Asian countries"
colnames(top_10_10s)[colnames(top_10_10s) == "Other Asian countries (aggregate changing according to the context)"] <- "Other Asian countries"
top_10_10s

top_10_10s_plot = ggplot(top_10_10s, aes(x = reorder(Country, column_sums_10s), y = column_sums_10s)) +
  geom_col(fill = "steelblue") +
  coord_flip() + # Flip the coordinates to make the bars horizontal
  labs(
    title = "Top 10 Tourist Arrivals (Since 2010)",
    x = "Country",
    y = "Total Tourist Arrivals"
  ) +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(8,8,8,8), "pt"))
top_10_10s_plot


# Save plots
# Define output directory
output_dir <- "plots/study_tourism"

# Save the plots
ggsave(filename = file.path(output_dir, "top_10_tourist_arrivals_90s.png"), plot = top_10_90s_plot, width = 8, height = 6)
ggsave(filename = file.path(output_dir, "top_10_tourist_arrivals_00s.png"), plot = top_10_00s_plot, width = 8, height = 6)
ggsave(filename = file.path(output_dir, "top_10_tourist_arrivals_10s.png"), plot = top_10_10s_plot, width = 8, height = 6)


# Manually select the countries from the top 10 lists that appear in all three decades
top_markets_90s = c('Germany', 'United Kingdom', 'France', 'Austria', 'United States', 'Spain', 'Netherlands', 'Switzerland and Licheqnstein')
top_markets_00s = c('Germany', 'United Kingdom', 'France', 'Austria', 'Netherlands', 'United States', 'Spain', 'Switzerland and Licheqnstein')
top_markets_10s = c('Germany', 'United Kingdom', 'France', 'United States', 'Austria', 'Switzerland and LicheqnsteinSwitzerland and Licheqnstein','Central and South America')

#Final list of countries to study including geographically diverse countries
final_countries = c('Germany', 'United Kingdom', 'France', 'Austria', 'United States', 'Japan', 'China', 'Brazil', 'Canada', 'Australia', 'Türkiye', 'South Africa', 'Russia')
final_countries

#Filter data to include only final countries
tourism_data_final <- tourism_data_filtered_yearly %>%
  select(Year, all_of(final_countries))

#Rename Türkiye column to Turkey
colnames(tourism_data_final)[colnames(tourism_data_final) == "Türkiye"] <- "Turkey"

#View final data
View(tourism_data_final)

#Find countries with complete data since 1990

#Select only countries with complete data since 1990
# Find columns with missing data (NA)
# The `colSums` function sums the number of `NA` values in each column.
missing_data_cols <- colSums(is.na(tourism_data_final))
missing_data_cols

# Identify columns to keep (those with no missing values)
complete_data_cols <- names(missing_data_cols[missing_data_cols == 0])
complete_data_cols

View(tourism_data_final)

#Save final data to excel
write.xlsx(tourism_data_final, "data/processed/processed_tourist_arrivals.xlsx", rowNames = FALSE)

