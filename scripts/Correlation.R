#Correlation Analysis

#Load necessary libraries
library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)


#Data paths
tourist_arrivals_path <- "data/processed/processed_tourist_arrivals.xlsx"
tourist_arrivals_log_path <- "data/final/transformed_tourist_arrivals_log.xlsx"
gdp_data_pct_path <- "data/final/transformed_gdp_pct.xlsx"
unemployment_data_path <- "data/final/unemployment_data.xlsx"
oil_prices_data_path <- "data/processed/processed_brent_crude_oil.xlsx"
oil_prices_data_log_path <- "data/final/transformed_oil_prices_log.xlsx"
median_age_data_path <- "data/final/median_age_data.xlsx"
population_data_path <- "data/final/population_data.xlsx"
exchange_rate_data_pct_path <- "data/final/transformed_exchange_rate_pct.xlsx"


# Load datasets
tourist_arrivals <- read.xlsx(tourist_arrivals_path)
tourist_arrivals_log <- read.xlsx(tourist_arrivals_log_path)
gdp_data_pct <- read.xlsx(gdp_data_pct_path)
unemployment_data <- read.xlsx(unemployment_data_path)
oil_prices_data <- read.xlsx(oil_prices_data_path)
oil_prices_data_log <- read.xlsx(oil_prices_data_log_path)
median_age_data <- read.xlsx(median_age_data_path)
population_data <- read.xlsx(population_data_path)
exchange_rate_data_pct <- read.xlsx(exchange_rate_data_pct_path)

View(population_data)
View(median_age_data)
View(oil_prices_data_log)
View(exchange_rate_data_pct)
View(unemployment_data)
View(gdp_data_pct)
View(tourist_arrivals)
View(tourist_arrivals_log)

# Helpers 

# Correlation Table
# Reuse your pivot_long()
pivot_long <- function(data){
  data %>%
    pivot_longer(-Year, names_to = "Country", values_to = "Value") %>%
    arrange(Country, Year)
}

# Compute r for each Country × Variable
compute_cor_table <- function(arrivals_wide,
                              var_list_named,     # named list: list("GDP % change"=gdp_change, ...)
                              exclude_years = integer(0),
                              min_pairs = 5) {
  # Convert arrivals to long format
  message(" Reshaping arrivals data to long format...")
  arrivals_long <- pivot_long(arrivals_wide) %>%
    rename(arrivals = Value) %>%
    mutate(arrivals = as.numeric(arrivals))
  
  message(" Computing correlations...")
  # For each variable, compute r by Country (use lapply (avoids loop))
  out_list <- lapply(names(var_list_named), function(vname){
    x_long <- pivot_long(var_list_named[[vname]]) %>%
      rename(xvar = Value) %>%
      mutate(xvar = as.numeric(xvar))
    
    df <- inner_join(arrivals_long, x_long, by = c("Year","Country")) %>%
      filter(!Year %in% exclude_years)
    
    df %>%
      mutate(complete = is.finite(arrivals) & is.finite(xvar)) %>%
      group_by(Country) %>%
      summarise(
        Variable = vname,
        n_comp   = sum(complete),
        r        = if (n_comp >= min_pairs)
          suppressWarnings(cor(arrivals[complete], xvar[complete]))
        else NA_real_,
        .groups = "drop"
      )
  })
  
  bind_rows(out_list)
}

# Heatmap plotting function
plot_cor_heatmap <- function(cor_tbl, title = "Arrivals vs Drivers — Correlation by Country",
                             show_labels = TRUE) {
  
  # Order countries by average |r| so the structure pops
  country_order <- cor_tbl %>%
    group_by(Country) %>%
    summarise(score = mean(abs(r), na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(score)) %>% pull(Country)
  
  cor_tbl <- cor_tbl %>%
    mutate(
      Country  = factor(Country, levels = country_order),
      Variable = factor(Variable, levels = unique(Variable))
    )
  
  p <- ggplot(cor_tbl, aes(x = Country, y = Variable, fill = r)) +
    geom_tile(color = "white", size = 0.2, na.rm = TRUE) +
    scale_fill_gradient2(limits = c(-1, 1), oob = scales::squish,
                         low = "#b2182b", mid = "white", high = "#2166ac",
                         name = "r") +
    coord_fixed() +
    labs(title = title, x = NULL, y = NULL) +
    theme_bw(base_size = 11) +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1),
      panel.grid   = element_blank(),
      plot.title   = element_text(hjust = 0.5)
    )
  
  if (show_labels) {
    p <- p + geom_text(aes(label = ifelse(is.na(r), "", sprintf("%.2f", r))),
                       size = 3, color = "black")
  }
  p
}

# Prepare variables for correlation analysis
# Use the transformed frames you already created.
# Examples (adjust to what you finalized):
#  - gdp_change                -> % change
#  - exchange_rate_change      -> % change (use normalized FX with country names)
#  - oil_countrywide           -> levels; or use your log/Δlog version
#  - unemployment_data         -> levels
#  - median_age_data           -> levels
#  - population_data           -> % change

vars <- list(
  "GDP per capita (% change)"   = gdp_data_pct,
  "Exchange rate (% change)"    = exchange_rate_data_pct,
  "Brent oil (log level)"    = oil_prices_data_log,
  "Unemployment rate (%)"    = unemployment_data,
  "Median age (years)"       = median_age_data,
  "Population (% change)"       = population_data
  
)

# Compute correlation table
cor_tbl <- compute_cor_table(
  arrivals_wide   = tourist_arrivals,
  var_list_named  = vars,  # optional: remove COVID shock
  min_pairs       = 5                # require at least 5 overlapping years
)
View(cor_tbl)

# Compute correlation table excluding COVID years
cor_tbl_no_covid <- compute_cor_table(
  arrivals_wide   = tourist_arrivals,
  exclude_years   = c(2020,2021),
  var_list_named  = vars
)

heatmap <- plot_cor_heatmap(cor_tbl,
                            title = "Correlation of Tourist Arrivals with Drivers (by Country)",
                            show_labels = TRUE)
print(heatmap)

heatmap_no_covid <- plot_cor_heatmap(cor_tbl_no_covid,
                            title = "Correlation of Tourist Arrivals with Drivers (by Country) — Excluding COVID Years",
                            show_labels = TRUE)
print(heatmap_no_covid)
