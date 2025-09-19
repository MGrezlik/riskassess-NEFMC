## Developing candidate Commercial Fishery Characterizations for the New England Fishery Management Council's Risk Policy
## Indicators informed by the Risk Policy Statement and Concept (2025)
##
## Author: M.T.Grezlik
## Date: 05/15/2025

## Indicator: Effective Shannon index of top n ports

# unique(ecodata::commercial_div$Var)
# unique(ecodata::commercial_div$EPU)

library(dplyr)
library(ggplot2)
library(ecodata)
library(purrr)
library(here)
library(patchwork) 

# 
# Load and filter data ---------------
#

data_filtered <- ecodata::commercial_div |>
  filter(Var == "Fleet diversity in revenue", EPU == "NE") |>
  filter(!is.na(Time)) |>
  arrange(Time)

# Define terminal years (retrospective range)
terminal_years <- 2022:2012

output_dir <- here::here("05_images/")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 
# Function to generate plot for a single terminal year -----------
# 
plot_for_terminal_year <- function(terminal_year) {
  
  years_window <- (terminal_year - 9):terminal_year
  
  recent_data <- data_filtered |>
    filter(Time >= terminal_year - 9, Time <= terminal_year)
  
  # Fit linear model if possible
  if (nrow(recent_data) >= 2) {
    lm_fit <- lm(Value ~ Time, data = recent_data)
    p_value <- summary(lm_fit)$coefficients["Time", "Pr(>|t|)"]
  } else {
    p_value <- NA
  }
  
  p <- ggplot(data_filtered, aes(x = Time, y = Value)) +
    geom_line() +
    geom_point() +
    ylab("Effective Shannon index") +
    ggtitle(paste("Fleet Diversity in Revenue (NE) – Terminal Year:", terminal_year)) +
    theme_minimal()
  
  if (!is.na(p_value) && p_value < 0.05) {
    p <- p + geom_smooth(
      data = recent_data,
      method = "lm", se = FALSE, color = "red", size = 1
    )
  }
  
  return(p)
}

# 
# Loop through terminal years, store trend info, save individual plots ------------
# 
trend_summary <- map_dfr(terminal_years, function(yr) {
  
  recent_data <- data_filtered |>
    filter(Time >= yr - 9, Time <= yr)
  
  trend_direction <- "None"
  slope <- NA
  p_val <- NA
  
  if (nrow(recent_data) >= 2) {
    lm_fit <- lm(Value ~ Time, data = recent_data)
    coef_summary <- summary(lm_fit)$coefficients
    slope <- coef_summary["Time", "Estimate"]
    p_val <- coef_summary["Time", "Pr(>|t|)"]
    
    if (!is.na(p_val) && p_val < 0.05) {
      trend_direction <- ifelse(slope > 0, "Positive", "Negative")
    }
  }
  
  # Save individual plot
  plot <- plot_for_terminal_year(yr)
  filename <- file.path(output_dir, paste0("fleet_trend_", yr, ".png"))
  ggsave(filename, plot = plot, width = 8, height = 5, dpi = 300)
  
  # Return summary info
  tibble(
    Terminal_Year = yr,
    Trend = trend_direction,
    Slope = slope,
    P_value = p_val
  )
})

# 
# Add management risk signal------------------
# 
trend_summary <- trend_summary |>
  mutate(Risk_Signal = case_when(
    Trend == "Positive" ~ "Less Risk Averse",
    Trend == "Negative" ~ "More Risk Averse",
    TRUE ~ "No Change"
  ))

# 
# Summarize trends across all terminal years ------------------
# 
trend_plot <- ggplot(trend_summary, aes(x = Terminal_Year, y = Trend, color = Risk_Signal)) +
  geom_point(size = 3) +
  scale_y_discrete(limits = c("Negative", "None", "Positive")) +
  scale_x_continuous(breaks = seq(min(trend_summary$Terminal_Year),
                                  max(trend_summary$Terminal_Year), 1)) +
  theme_minimal() +
  labs(
    title = "Fleet Diversity Trend Calls by Terminal Year",
    y = "Trend Direction",
    x = "Terminal Year"
  )

# Save summary plot
ggsave(file.path(output_dir, "fleet_trend_summary.png"), plot = trend_plot, width = 8, height = 5, dpi = 300)

# 
# Optionally combine a few example terminal-year plots with the summary -----------------
# 
example_plots <- map(c(2022, 2017, 2012), plot_for_terminal_year)
combined_plot <- wrap_plots(example_plots) / trend_plot + plot_layout(heights = c(2, 1))

ggsave(file.path(output_dir, "fleet_trend_combined.png"), plot = combined_plot, width = 12, height = 10, dpi = 300)

# 
# Save the trend summary table ------------------------
# 
write.csv(trend_summary, file.path(output_dir, "fleet_trend_summary.csv"), row.names = FALSE)


# 
# Threshold approach for Fleet Diversity -------------------------
# 

# Calculate mean and SD across full time series
fleet_mean <- mean(data_filtered$Value, na.rm = TRUE)
fleet_sd   <- sd(data_filtered$Value, na.rm = TRUE)

# Classify each year by threshold
fleet_threshold_class <- data_filtered |>
  mutate(Risk_Signal_Threshold = case_when(
    Value < fleet_mean - fleet_sd ~ "More Risk Averse",   # Below -1 SD
    Value > fleet_mean + fleet_sd ~ "Less Risk Averse",   # Above +1 SD
    TRUE ~ "No Change"
  ))

# Plot full time series with mean ±1 SD and classified points
fleet_threshold_plot <- ggplot(fleet_threshold_class,
                               aes(x = Time, y = Value,
                                   color = Risk_Signal_Threshold)) +
  geom_line(color = "grey60") +
  geom_point(size = 2) +
  geom_hline(yintercept = fleet_mean, linetype = "dashed", color = "black") +
  geom_hline(yintercept = fleet_mean + fleet_sd, linetype = "solid", color = "blue") +
  geom_hline(yintercept = fleet_mean - fleet_sd, linetype = "solid", color = "blue") +
  scale_color_manual(values = c(
    "More Risk Averse" = "red",
    "No Change" = "black",
    "Less Risk Averse" = "green"
  )) +
  theme_minimal() +
  labs(
    title = "Fleet Diversity in Revenue – Threshold Classification (NE)",
    y = "Effective Shannon Index",
    x = "Year",
    color = "Risk Signal"
  )

# Save threshold plot
ggsave(file.path(output_dir, "fleet_threshold_timeseries.png"),
       plot = fleet_threshold_plot, width = 8, height = 5, dpi = 300)

