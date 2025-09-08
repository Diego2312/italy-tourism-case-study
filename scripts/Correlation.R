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
gdp_data_log_path <- "data/final/transformed_gdp_log.xlsx"
unemployment_data_path <- "data/final/unemployment_data.xlsx"
oil_prices_data_path <- "data/final/oil_prices_data.xlsx"
oil_prices_data_log_path <- "data/final/transformed_oil_prices_log.xlsx"
median_age_data_path <- "data/final/median_age_data.xlsx"
population_data_path <- "data/final/population_data.xlsx"
exchange_rate_data_pct_path <- "data/final/transformed_exchange_rate_pct.xlsx"


# Load datasets
tourist_arrivals <- read.xlsx(tourist_arrivals_path)
tourist_arrivals_log <- read.xlsx(tourist_arrivals_log_path)
gdp_data_pct <- read.xlsx(gdp_data_pct_path)
gdp_data_log <- read.xlsx(gdp_data_log_path)
unemployment_data <- read.xlsx(unemployment_data_path)
oil_prices_data <- read.xlsx(oil_prices_data_path)
oil_prices_data_log <- read.xlsx(oil_prices_data_log_path)
median_age_data <- read.xlsx(median_age_data_path)
population_data <- read.xlsx(population_data_path)
exchange_rate_data_pct <- read.xlsx(exchange_rate_data_pct_path)

View(population_data)
View(median_age_data)
View(oil_prices_data_log)
View(oil_prices_data)
View(exchange_rate_data_pct)
View(unemployment_data)
View(gdp_data_pct)
View(gdp_data_log)
View(tourist_arrivals)
View(tourist_arrivals_log)

# Helpers 

# Define european and overseas countries
# European countries: Austria, France, Germany, Russia, Turkey, United Kingdom
# Overseas countries: Australia, Brazil, Canada, China, Japan, South Africa, United States

european_countries <- c("Austria", "France", "Germany", "Russia", "Turkey", "United.Kingdom")
overseas_countries <- c("Australia", "Brazil", "Canada", "China", "Japan", "South.Africa", "United.States")


# Correlation Table

# Function to pivot data to long format
pivot_long <- function(data){
  data %>%
    pivot_longer(-Year, names_to = "Country", values_to = "Value") %>%
    arrange(Country, Year)
}


# Function to lag data by specified years
lag_data <- function(data, lag_years = 1) {
  data %>%
    pivot_longer(-Year, names_to = "Country", values_to = "Value") %>%
    arrange(Country, Year) %>%
    group_by(Country) %>%
    mutate(Value = dplyr::lag(Value, n = lag_years)) %>%
    ungroup() %>%
    pivot_wider(names_from = Country, values_from = Value)
}
# Create lagged GDP data (1 year lag)
lagged_gdp_data_pct <- lag_data(gdp_data_pct, lag_years = 1)
lagged_2_gdp_data_pct <- lag_data(gdp_data_pct, lag_years = 2)


View(lagged_gdp_data_pct)
View(gdp_data_pct)

# Compute r for each Country × Variable
compute_cor_table <- function(arrivals_wide,
                              var_list_named,
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
                             subtitle = NULL, show_labels = TRUE) {
  
  country_order <- cor_tbl %>%
    group_by(Country, Group) %>%
    summarise(score = mean(abs(r), na.rm = TRUE), .groups = "drop") %>%
    arrange(Group, desc(score)) %>% pull(Country)
  
  cor_tbl <- cor_tbl %>%
    mutate(
      Country  = factor(Country, levels = country_order),
      Variable = factor(Variable, levels = unique(Variable))
    )
  
  p <- ggplot(cor_tbl, aes(x = Country, y = Variable, fill = r)) +
    geom_tile(color = "white", size = 0.2, na.rm = TRUE) +
    scale_fill_gradient2(limits = c(-1, 1), oob = scales::squish,
                         low = "red", mid = "white", high = "skyblue",
                         name = "r") +
    facet_grid(~ Group, scales = "free_x", space = "free_x") +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
    theme_bw(base_size = 11) +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1),
      panel.grid   = element_blank(),
      plot.title   = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  if (show_labels) {
    p <- p + geom_text(aes(label = ifelse(is.na(r), "", sprintf("%.2f", r))),
                       size = 3, color = "black")
  }
  p
}


plot_cor_var <- function(cor_tbl, var_name, title) {
  
  cor_tbl_var <- cor_tbl %>%
    filter(Variable == var_name) %>%
    mutate(Group = if_else(Country %in% european_countries, "European", "Overseas"))
  
  # group means (ignore NA r)
  stats <- cor_tbl_var %>%
    group_by(Group) %>%
    summarise(mean_r = mean(r, na.rm = TRUE),
              n = sum(!is.na(r)),
              .groups = "drop") %>%
    tidyr::pivot_wider(names_from = Group, values_from = c(mean_r, n))
  
  mean_eu <- stats$mean_r_European
  mean_ov <- stats$mean_r_Overseas
  n_eu    <- stats$n_European
  n_ov    <- stats$n_Overseas
  delta   <- mean_ov - mean_eu
  
  # Welch t-test (robust to unequal variances)
  t_out <- t.test(r ~ Group, data = cor_tbl_var)
  p_t   <- t_out$p.value
  p_str  <- paste0("T-test p = ", sprintf("%.3f", p_t))
  
  subtitle <- paste0(
    "Mean r — Europe: ", sprintf("%.2f", mean_eu),
    "  Overseas: ", sprintf("%.2f", mean_ov),
    "   (Overseas − Europe) = ", sprintf("%.2f", delta), "   ", p_str
  )
  
  heatmap_var <- plot_cor_heatmap(
    cor_tbl_var,
    title = title,
    subtitle = subtitle,
    show_labels = TRUE
  )
  
  print(heatmap_var)
  
  # return stats invisibly for logging/tables
  invisible(list(
    mean_europe = mean_eu,
    mean_overseas = mean_ov,
    delta = delta,
    p_t_test = p_t
  ))
}

# Prepare variables for correlation analysis

vars <- list(
  "GDP per capita (% change)"   = gdp_data_pct,
  "Exchange rate (% change)"    = exchange_rate_data_pct,
  "Brent oil (log level)"    = oil_prices_data,
  "Unemployment rate (%)"    = unemployment_data,
  "Median age (years)"       = median_age_data,
  "Population (% change)"       = population_data,
  "GDP per capita (% change) (lagged 1 year)" = lagged_gdp_data_pct,
  "GDP per capita (% change) (lagged 2 years)" = lagged_2_gdp_data_pct
  
)

# Compute correlation table
cor_tbl <- compute_cor_table(
  arrivals_wide   = tourist_arrivals,
  var_list_named  = vars,
)


# Compute correlation table excluding COVID years
cor_tbl_no_covid <- compute_cor_table(
  arrivals_wide   = tourist_arrivals,
  exclude_years   = c(2020,2021),
  var_list_named  = vars
)


# Filter for only European and Overseas countries
cor_tbl_overseas <- cor_tbl %>%
  filter(Country %in% overseas_countries)

cor_tbl_europe <- cor_tbl %>%
  filter(Country %in% european_countries)

# Have a correlation table for each variable
cor_tbl_gdp <- cor_tbl %>%
  filter(Variable == "GDP per capita (% change)")
cor_tbl_gdp_lagged <- cor_tbl %>%
  filter(Variable == "GDP per capita (% change) (lagged 1 year)")
cor_tbl_gdp_lagged2 <- cor_tbl %>%
  filter(Variable == "GDP per capita (% change) (lagged 2 years)")


cor_tbl_exchange <- cor_tbl %>%
  filter(Variable == "Exchange rate (% change)")
cor_tbl_oil <- cor_tbl %>%
  filter(Variable == "Brent oil (log level)")
cor_tbl_unemployment <- cor_tbl %>%
  filter(Variable == "Unemployment rate (%)")
cor_tbl_median_age <- cor_tbl %>%
  filter(Variable == "Median age (years)")
cor_tbl_population <- cor_tbl %>%
  filter(Variable == "Population (% change)")


print(cor_tbl_gdp)
print(cor_tbl_gdp_lagged)

# Plot individual variable heatmaps

plot_cor_var(cor_tbl_gdp, "GDP per capita (% change)", "Correlation of Tourist Arrivals with GDP per capita (% change)")
plot_cor_var(cor_tbl_exchange, "Exchange rate (% change)", "Correlation of Tourist Arrivals with Exchange rate (% change)")
plot_cor_var(cor_tbl_oil, "Brent oil (log level)", "Correlation of Tourist Arrivals with Brent oil (log level)")
plot_cor_var(cor_tbl_unemployment, "Unemployment rate (%)", "Correlation of Tourist Arrivals with Unemployment rate (%)")
plot_cor_var(cor_tbl_median_age, "Median age (years)", "Correlation of Tourist Arrivals with Median age (years)")
plot_cor_var(cor_tbl_population, "Population (% change)", "Correlation of Tourist Arrivals with Population (% change)")

# Plot heatmaps

# Europe x Overseas split
heatmap_overseas <- plot_cor_heatmap(cor_tbl_overseas,
                            title = "Correlation of Tourist Arrivals with Drivers (Overseas Countries)",
                            show_labels = TRUE)
print(heatmap_overseas)

heatmap_europe <- plot_cor_heatmap(cor_tbl_europe,
                            title = "Correlation of Tourist Arrivals with Drivers (European Countries)",
                            show_labels = TRUE)
print(heatmap_europe)





# With covid years
heatmap <- plot_cor_heatmap(cor_tbl,
                            title = "Correlation of Tourist Arrivals with Drivers (by Country)",
                            show_labels = TRUE)
print(heatmap)

# No covid years
heatmap_no_covid <- plot_cor_heatmap(cor_tbl_no_covid,
                            title = "Correlation of Tourist Arrivals with Drivers (by Country) — Excluding COVID Years",
                            show_labels = TRUE)
print(heatmap_no_covid)

# Save heatmaps

# Define output paths
plot_path <- "plots/results/correlation_heatmap.png"
plot_no_covid_path <- "plots/results/correlation_heatmap_no_covid.png"

# Save plots
ggsave(plot_path, plot = heatmap, width = 10, height = 6)
ggsave(plot_no_covid_path, plot = heatmap_no_covid, width = 10, height = 6)
