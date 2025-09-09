library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)



#Data paths
tourist_arrivals_path <- "data/processed/processed_tourist_arrivals.xlsx"
gdp_data_path <- "data/processed/processed_gdp.xlsx"
unemployment_data_path <- "data/processed/processed_unemployement.xlsx"
oil_prices_data_path <- "data/processed/processed_brent_crude_oil.xlsx"
median_age_data_path <- "data/processed/processed_median_age.xlsx"
population_data_path <- "data/processed/processed_population_change.xlsx"
exchange_rate_data_path <- "data/processed/processed_exchange_rates.xlsx"


# Load datasets
tourist_arrivals <- read.xlsx(tourist_arrivals_path)
gdp_data <- read.xlsx(gdp_data_path)
unemployment_data <- read.xlsx(unemployment_data_path)
oil_prices_data <- read.xlsx(oil_prices_data_path)
median_age_data <- read.xlsx(median_age_data_path)
population_data <- read.xlsx(population_data_path)
exchange_rate_data <- read.xlsx(exchange_rate_data_path)

# Manual View
View(oil_prices_data)

# Final data preparation

# Drop Germany France and Austria from exchange rate plots (same currency)
exchange_rate_data <- exchange_rate_data %>%
  select(-c(Germany, France, Austria))

# Rename exchange rate columns to match countries in tourist arrivals
study_countries <- names(tourist_arrivals)[-1]
study_countries <- sort(study_countries)  
study_countries
#Drop countries Germany, France and Austria (same currency)
study_countries_exchange_rate <- study_countries[!study_countries %in% c("Germany", "France", "Austria")]
study_countries_exchange_rate

curr_names <- names(exchange_rate_data)[-1]
curr_names <- sort(curr_names)
curr_names

#Rename columns in exchange_rate_data to match countries in tourist arrivals
colnames(exchange_rate_data) <- c("Year", study_countries_exchange_rate)

View(exchange_rate_data)

# Repeat oil prices column for all countries
new_cols <- replicate(length(study_countries), oil_prices_data$Brent_Crude_Oil_USD_per_barrel)
new_df <- data.frame(new_cols)
colnames(new_df) <- study_countries
oil_prices_data <- cbind(oil_prices_data["Year"], new_df)

# Ensure consistent column order and names

# Function to sort columns alphabetically, keeping 'Year' first
sort_columns <- function(df) {
  year_col <- "Year"
  other_cols <- setdiff(names(df), year_col)
  sorted_other_cols <- sort(other_cols)
  new_order <- c(year_col, sorted_other_cols)
  return(df[, new_order])
}

#Function to rename columns to match tourist arrivals
rename_columns <- function(df, target_names) {
  colnames(df) <- c("Year", target_names)
  return(df)
}

# Apply to all datasets
gdp_data <- sort_columns(gdp_data)
gdp_data <- rename_columns(gdp_data, study_countries)
unemployment_data <- sort_columns(unemployment_data)
unemployment_data <- rename_columns(unemployment_data, study_countries)
oil_prices_data <- sort_columns(oil_prices_data)
oil_prices_data <- rename_columns(oil_prices_data, study_countries)
median_age_data <- sort_columns(median_age_data)
median_age_data <- rename_columns(median_age_data, study_countries)
population_data <- sort_columns(population_data)
population_data <- rename_columns(population_data, study_countries)
exchange_rate_data <- sort_columns(exchange_rate_data)
exchange_rate_data <- rename_columns(exchange_rate_data, study_countries_exchange_rate)

# Convert Year columns to numeric (GDP and Unemployement got left out)
gdp_data$Year <- as.numeric(gdp_data$Year)
unemployment_data$Year <- as.numeric(unemployment_data$Year)
oil_prices_data$Year <- as.numeric(oil_prices_data$Year)
median_age_data$Year <- as.numeric(median_age_data$Year)
population_data$Year <- as.numeric(population_data$Year)
exchange_rate_data$Year <- as.numeric(exchange_rate_data$Year)
tourist_arrivals$Year <- as.numeric(tourist_arrivals$Year)


# View datasets to confirm
View(tourist_arrivals)
View(gdp_data)
View(unemployment_data)
View(exchange_rate_data)
View(oil_prices_data)
View(median_age_data)
View(population_data)


# ---- Helpers ----

# strict log (no eps padding)
safe_log <- function(x) {
  x <- as.numeric(x)
  out <- ifelse(x > 0, log(x), NA_real_)
  out
}

# Δlog (preferred for growth)
dlog <- function(x) {
  x <- as.numeric(x)
  v <- ifelse(x > 0, log(x), NA_real_)
  c(NA_real_, diff(v))
}

# % change (for presentation or robustness)
pct_change <- function(x) {
  x <- as.numeric(x)
  pc <- (x - dplyr::lag(x)) / dplyr::lag(x) * 100
  # guard against lag==0
  pc[is.infinite(pc)] <- NA_real_
  pc
}

# Wide -> long
pivot_long <- function(data){
  data %>%
    pivot_longer(-Year, names_to = "Country", values_to = "Value") %>%
    arrange(Country, Year)
}

# Histogram (faceted), with free x and 12 bins
plot_distribution <- function(data, title, xlab){
  
  data_long <- pivot_long(data)
  
  p <- ggplot(data_long, aes(x = Value)) +
    geom_histogram(bins = 12, fill = "lightblue", color = "black", na.rm = TRUE) +
    facet_wrap(~ Country, scales = "free_y") +
    labs(title = title, x = xlab, y = "Count") +
    theme_bw(base_size = 11) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.margin = unit(c(8,8,8,8), "pt"))
  return(p)
}

# Time series (faceted)
plot_time_series <- function(data, title, ylab){
  
  data_long <- pivot_long(data)
  
  p <- ggplot(data_long, aes(x = Year, y = Value)) +
    geom_point(color = "steelblue") +
    geom_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x, alpha=0.6) +
    facet_wrap(~ Country, scales = "free_y") +
    labs(title = title, x = "Year", y = ylab) +
    scale_x_continuous(breaks = pretty) +
    theme_bw(base_size = 11) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.margin = unit(c(8,8,8,8), "pt"))
  return(p)
  
}

# Function to plot variable vs tourist arrivals per country
plot_vs_arrivals <- function(x_wide,
                             x_name,           # short id e.g. "GDPpc"
                             x_lab,            # x-axis label for plot
                             arrivals_wide = tourist_arrivals,
                             exclude_years = integer(0),
                             free_x = FALSE,   # FALSE = comparable x across panels
                             log_x_axis = FALSE,
                             log_y_axis = FALSE,
                             point_alpha = 0.6) {
  
  # wide -> long
  arrivals_long <- pivot_long(arrivals_wide) %>% rename(arrivals = Value)
  x_long        <- pivot_long(x_wide)        %>% rename(xvar     = Value)
  
  # align & clean
  df <- dplyr::inner_join(arrivals_long, x_long,
                          by = c("Year","Country")) %>%
    dplyr::filter(!Year %in% exclude_years) %>%
    dplyr::filter(is.finite(arrivals), is.finite(xvar))
  
  # quick per-country correlations (printed to console)
  cor_tbl <- df %>%
    dplyr::group_by(Country) %>%
    dplyr::summarise(n = dplyr::n(),
                     r = suppressWarnings(stats::cor(arrivals, xvar, use = "complete.obs")),
                     .groups = "drop") %>%
    dplyr::arrange(desc(r))
  message("\nPer-country correlations with arrivals for: ", x_name)
  print(cor_tbl, n = Inf)
  
  
  # base plot
  p <- ggplot(df, aes(x = xvar, y = arrivals)) +
    geom_point(color = "darkgreen", alpha = point_alpha) +
    geom_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x) +
    facet_wrap(~ Country,
               scales = if (free_x) "free" else "free_y") +
    labs(
      title = paste("Tourist Arrivals vs", x_name),
      x = x_lab,
      y = "Tourist Arrivals"
    ) +
    theme_bw(base_size = 11) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.margin = unit(c(8,8,8,8), "pt"))
  
  if (log_x_axis) p <- p + scale_x_log10()
  if (log_y_axis) p <- p + scale_y_log10()
  
  # Return plot object and correlations table invisibly
  
  return(p)
}

# Function for saving plots
save_plot <- function(plot, filename, width = 12, height = 8) {
  ggsave(filename, plot = plot, width = width, height = height)
  message(" Plot saved to: ", filename)
}

# ---- Transformations ----
# Expect wide frames: Year + countries

# Log transformations
tourist_arrivals_log <- tourist_arrivals %>%
  mutate(across(-Year, safe_log, .names = "log_{col}")) %>%
  select(Year, starts_with("log_")) %>%
  rename_with(~ gsub("log_", "", .x), starts_with("log_"))

oil_prices_log <- oil_prices_data %>%
  mutate(across(-Year, safe_log, .names = "log_{col}")) %>%
  select(Year, starts_with("log_")) %>%
  rename_with(~ gsub("log_", "", .x), starts_with("log_"))

unemployment_log <- unemployment_data %>%
  mutate(across(-Year, safe_log, .names = "log_{col}")) %>%
  select(Year, starts_with("log_")) %>%
  rename_with(~ gsub("log_", "", .x), starts_with("log_"))

gdp_log <- gdp_data %>%
  mutate(across(-Year, safe_log, .names = "log_{col}")) %>%
  select(Year, starts_with("log_")) %>%
  rename_with(~ gsub("log_", "", .x), starts_with("log_"))

# Delta log transformations (preferred for growth)
gdp_dlog <- gdp_data %>%
  mutate(across(-Year, dlog, .names = "dlog_{col}")) %>%
  select(Year, starts_with("dlog_")) %>%
  rename_with(~ gsub("dlog_", "", .x), starts_with("dlog_"))

exchange_rate_dlog <- exchange_rate_data %>%
  mutate(across(-Year, dlog, .names = "dlog_{col}")) %>%
  select(Year, starts_with("dlog_"))%>%
  rename_with(~ gsub("dlog_", "", .x), starts_with("dlog_"))

# % change versions (Alternatives)
gdp_pct <- gdp_data %>%
  mutate(across(-Year, pct_change, .names = "pct_{col}")) %>%
  select(Year, starts_with("pct_")) %>%
  rename_with(~ gsub("pct_", "", .x), starts_with("pct_"))

exchange_rate_pct  <- exchange_rate_data %>%
  mutate(across(-Year, pct_change, .names = "pct_{col}")) %>%
  select(Year, starts_with("pct_")) %>%
  rename_with(~ gsub("pct_", "", .x), starts_with("pct_"))

tourist_arrivals_pct <- tourist_arrivals %>%
  mutate(across(-Year, pct_change, .names = "pct_{col}")) %>%
  select(Year, starts_with("pct_")) %>%
  rename_with(~ gsub("pct_", "", .x), starts_with("pct_"))

# Unemployment & Median age kept raw

#View all transformed data
View(gdp_pct)
View(gdp_log)
View(exchange_rate_pct)
View(tourist_arrivals_log)
View(oil_prices_data)
View(unemployment_data)
View(median_age_data)
View(population_data)



# Save final datasets
write.xlsx(gdp_pct, "data/final/transformed_gdp_pct.xlsx", overwrite = TRUE)
write.xlsx(gdp_log, "data/final/transformed_gdp_log.xlsx", overwrite = TRUE)
write.xlsx(exchange_rate_pct, "data/final/transformed_exchange_rate_pct.xlsx", overwrite = TRUE)
write.xlsx(tourist_arrivals_log, "data/final/transformed_tourist_arrivals_log.xlsx", overwrite = TRUE)
write.xlsx(tourist_arrivals_pct, "data/final/transformed_tourist_arrivals_pct.xlsx", overwrite = TRUE)
write.xlsx(oil_prices_log, "data/final/transformed_oil_prices_log.xlsx", overwrite = TRUE)
write.xlsx(oil_prices_data, "data/final/oil_prices_data.xlsx", overwrite = TRUE)
write.xlsx(unemployment_data, "data/final/unemployment_data.xlsx", overwrite = TRUE)
write.xlsx(median_age_data, "data/final/median_age_data.xlsx", overwrite = TRUE)
write.xlsx(population_data, "data/final/population_data.xlsx", overwrite = TRUE)


# ---- Plots ----

# Distributions
ta_dist = plot_distribution(tourist_arrivals, "Tourist Arrivals", "arrivals")
oil_dist = plot_distribution(oil_prices_data, "Brent Oil Prices", "USD/barrel")
exrate_dist = plot_distribution(exchange_rate_data, "Exchange Rates", "local currency/USD")
gdp_dist = plot_distribution(gdp_data, "GDP per Capita", "USD (PPP)")
un_dist = plot_distribution(unemployment_data, "Unemployment Rate", "%")
mage_dist = plot_distribution(median_age_data, "Median Age", "years")
pop_dist = plot_distribution(population_data, "Population Change", "% change")

# Time series
ta_ts = plot_time_series(tourist_arrivals, "Tourist Arrivals", "arrivals")
oil_ts = plot_time_series(oil_prices_data, "Brent Oil Prices", "USD/barrel")
exrate_ts = plot_time_series(exchange_rate_data, "Exchange Rates", "local currency/USD")
gdp_ts = plot_time_series(gdp_data, "GDP per Capita", "USD (PPP)")
un_ts = plot_time_series(unemployment_data, "Unemployment Rate", "rate %")
mage_ts = plot_time_series(median_age_data, "Median Age", "years")
pop_ts = plot_time_series(population_data, "Population Change", "% change")


# Distributions (Transformed)
gdp_dist_tr = plot_distribution(gdp_pct, "GDP per Capita – % Change", "% change")
exrate_dist_tr = plot_distribution(exchange_rate_pct, "Exchange Rate – % Change", "% change")
ta_dist_tr = plot_distribution(tourist_arrivals_log, "Tourist Arrivals (log)", "log(arrivals)")
oil_dist_tr = plot_distribution(oil_prices_log,"Brent Oil (log)", "log(USD/barrel)")
un_dist_tr = plot_distribution(unemployment_log, "Unemployment Rate", "log(rate %)")

# Time series (Transformed)
gdp_ts_tr = plot_time_series(gdp_pct, "GDP per Capita – % Change", "% change")
exrate_ts_tr = plot_time_series(exchange_rate_pct,"Exchange Rate – % Change", "% change")
ta_ts_tr = plot_time_series(tourist_arrivals_log, "Tourist Arrivals (log)", "log(arrivals)")
oil_ts_tr = plot_time_series(oil_prices_log, "Brent Oil (log)", "log(USD/barrel)")
un_ts_tr = plot_time_series(unemployment_log, "Unemployment Rate", "rate %")



# Scatter plots vs tourist arrivals

View(gdp_pct)
# GDP per capita (raw and % change)
gdp_scatter_raw = plot_vs_arrivals(gdp_data, x_name = "GDP per Capita (PPP)",
                 x_lab = "USD (PPP)",
                 free_x = FALSE, log_y_axis = TRUE)

gdp_scatter_raw

gdp_scatter_pct = plot_vs_arrivals(gdp_pct, x_name = "GDP per Capita (PPP) % change",
                 x_lab = "USD (PPP) % change",
                 free_x = FALSE, log_y_axis = TRUE)
gdp_scatter_pct

#View(exchange_rate_pct)
# Exchange rate (raw and % change)
exrate_scatter_raw = plot_vs_arrivals(exchange_rate_data, x_name = "Exchange Rate",
                 x_lab = "local currency/USD",
                 free_x = FALSE, log_y_axis = TRUE)
exrate_scatter_raw

exrate_scatter_pct = plot_vs_arrivals(exchange_rate_pct, x_name = "Exchange Rate % change",
                 x_lab = "local currency/USD % change",
                 free_x = FALSE, log_y_axis = FALSE)
exrate_scatter_pct

View(oil_prices_data)
# Oil price (raw and log)
oil_scatter_raw = plot_vs_arrivals(oil_prices_data, "Brent Oil Price", "USD/barrel",
                 free_x = FALSE, log_y_axis = TRUE)
oil_scatter_raw

oil_scatter_log = plot_vs_arrivals(oil_prices_log, "Brent Oil Price", "USD/barrel",
                 free_x = FALSE, log_y_axis = TRUE)
oil_scatter_log

#View(unemployment_data)
# Unemployment rate (raw)
un_scatter_raw = plot_vs_arrivals(unemployment_data, "Unemployment Rate", "%",
                 free_x = FALSE, log_y_axis = TRUE)
un_scatter_raw

un_scatter_log = plot_vs_arrivals(unemployment_log, "Unemployment Rate", "log(%)",
                 free_x = FALSE, log_y_axis = TRUE)
un_scatter_log

# Median age (raw)
mage_scatter_raw = plot_vs_arrivals(median_age_data, "Median Age", "years",
                 free_x = FALSE, log_y_axis = TRUE)
mage_scatter_raw

# 6) Population (% change) (already % change)
pop_scatter_raw = plot_vs_arrivals(population_data, "Population % change", "%",
                 free_x = FALSE, log_y_axis = TRUE)
pop_scatter_raw


# Save plots 

# Define saving paths
distributions_path <- "plots/diagnostics/distributions"
time_series_path <- "plots/diagnostics/time_series"
scatter_plots_path <- "plots/diagnostics/scatter_vs_arrivals"

# Save distribution plots

# Raw
save_plot(ta_dist, file.path(distributions_path, "tourist_arrivals_dist_raw.png"))
save_plot(oil_dist, file.path(distributions_path, "oil_prices_dist_raw.png"))
save_plot(exrate_dist, file.path(distributions_path, "exchange_rate_dist_raw.png"))
save_plot(gdp_dist, file.path(distributions_path, "gdp_dist_raw.png"))
save_plot(un_dist, file.path(distributions_path, "unemployment_dist_raw.png"))
save_plot(mage_dist, file.path(distributions_path, "median_age_dist_raw.png"))
save_plot(pop_dist, file.path(distributions_path, "population_dist_raw.png"))

# Transformed
save_plot(ta_dist_tr, file.path(distributions_path, "tourist_arrivals_dist_log.png"))
save_plot(oil_dist_tr, file.path(distributions_path, "oil_prices_dist_log.png"))
save_plot(exrate_dist_tr, file.path(distributions_path, "exchange_rate_dist_pct.png"))
save_plot(gdp_dist_tr, file.path(distributions_path, "gdp_dist_pct.png"))
save_plot(un_dist_tr, file.path(distributions_path, "unemployment_dist_log.png"))

# Save time series plots

# Raw
save_plot(ta_ts, file.path(time_series_path, "tourist_arrivals_ts_raw.png"))
save_plot(oil_ts, file.path(time_series_path, "oil_prices_ts_raw.png"))
save_plot(exrate_ts, file.path(time_series_path, "exchange_rate_ts_raw.png"))
save_plot(gdp_ts, file.path(time_series_path, "gdp_ts_raw.png"))
save_plot(un_ts, file.path(time_series_path, "unemployment_ts_raw.png"))
save_plot(mage_ts, file.path(time_series_path, "median_age_ts_raw.png"))
save_plot(pop_ts, file.path(time_series_path, "population_ts_raw.png"))

# Transformed
save_plot(ta_ts_tr, file.path(time_series_path, "tourist_arrivals_ts_log.png"))
save_plot(oil_ts_tr, file.path(time_series_path, "oil_prices_ts_log.png"))
save_plot(exrate_ts_tr, file.path(time_series_path, "exchange_rate_ts_pct.png"))
save_plot(gdp_ts_tr, file.path(time_series_path, "gdp_ts_pct.png"))
save_plot(un_ts_tr, file.path(time_series_path, "unemployment_ts_log.png"))

# Save scatter plots vs tourist arrivals

# GDP
save_plot(gdp_scatter_raw, file.path(scatter_plots_path, "gdp_scatter_raw.png"))
save_plot(gdp_scatter_pct, file.path(scatter_plots_path, "gdp_scatter_pct.png"))

# Exchange rate
save_plot(exrate_scatter_raw, file.path(scatter_plots_path, "exchange_rate_scatter_raw.png"))
save_plot(exrate_scatter_pct, file.path(scatter_plots_path, "exchange_rate_scatter_pct.png"))

# Oil prices
save_plot(oil_scatter_raw, file.path(scatter_plots_path, "oil_prices_scatter_raw.png"))
save_plot(oil_scatter_log, file.path(scatter_plots_path, "oil_prices_scatter_log.png"))

# Unemployment
save_plot(un_scatter_raw, file.path(scatter_plots_path, "unemployment_scatter_raw.png"))
save_plot(un_scatter_log, file.path(scatter_plots_path, "unemployment_scatter_log.png"))

# Median age
save_plot(mage_scatter_raw, file.path(scatter_plots_path, "median_age_scatter_raw.png"))

# Population
save_plot(pop_scatter_raw, file.path(scatter_plots_path, "population_scatter_raw.png"))



