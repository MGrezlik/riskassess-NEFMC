## Developing candidate Climate and Ecosystem Considerations for the New England Fishery Management Council's Risk Policy
## Indicators informed by ToR1 of the 2024 Research Track Assessment of Yellowtail flounder
##
## Author: M.T.Grezlik
## Date: 05/15/2025
##
## Stock: Southern New England
## Indicator: Mid-Atlantic Bight (MAB) Cold Pool persistence index
## Significant positive correlation found (i.e. longer cold pool persistence results in higher recruitment)
## Indicator available from Mid Atlantic SOE

ecodata::plot_cold_pool(report = "MidAtlantic", varName = 'persistence')


## Stock: Georges Bank
## Indicator: Bottom temperature
## Significant negative correlation found (i.e. higher bottom temperature results in lower recruitment)
## Indicator available from New England SOE

# ecodata::plot_bottom_temp(report = "NewEngland")

library(dplyr)
library(ggplot2)
library(ecodata)
library(purrr)
library(here)
library(patchwork)

# 
# Load Georges Bank bottom temperature anomaly
# 
data_bt <- ecodata::bottom_temp_comp |>
  filter(Var == "Annual_Bottom Temp", EPU == "GB") |>
  filter(!is.na(Time)) |>
  arrange(Time)

# Calculate mean and sd for threshold lines
bt_mean <- mean(data_bt$Value, na.rm = TRUE)
bt_sd   <- sd(data_bt$Value, na.rm = TRUE)

# Terminal years to test
terminal_years <- 2022:2012

output_dir <- here::here("05_images/bottom_temp/")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

#
# Function to generate retrospective plot for one terminal year ----------------
#
plot_for_terminal_year_bt <- function(terminal_year) {
  
  recent_data <- data_bt |>
    filter(Time >= terminal_year - 9, Time <= terminal_year)
  
  # Fit linear model if possible
  if (nrow(recent_data) >= 2) {
    lm_fit <- lm(Value ~ Time, data = recent_data)
    p_value <- summary(lm_fit)$coefficients["Time", "Pr(>|t|)"]
  } else {
    p_value <- NA
  }
  
  p <- ggplot(data_bt, aes(x = Time, y = Value)) +
    geom_line() +
    geom_point() +
    ylab("Bottom Temperature Anomaly (°C)") +
    ggtitle(paste("Georges Bank – Bottom Temp Anomaly, Terminal Year:", terminal_year)) +
    theme_minimal() +
    geom_hline(yintercept = bt_mean, linetype = "dashed", color = "black") +
    geom_hline(yintercept = bt_mean + bt_sd, linetype = "solid", color = "blue") +
    geom_hline(yintercept = bt_mean - bt_sd, linetype = "solid", color = "blue")
  
  # Add trend line if significant
  if (!is.na(p_value) && p_value < 0.05) {
    p <- p + geom_smooth(
      data = recent_data,
      method = "lm", se = FALSE, color = "red", size = 1
    )
  }
  
  return(p)
}

# 
# Loop through terminal years, store trend + threshold signals ----------
# 
bt_summary <- map_dfr(terminal_years, function(yr) {
  
  recent_data <- data_bt |>
    filter(Time >= yr - 9, Time <= yr)
  
  trend_direction <- "None"
  slope <- NA
  p_val <- NA
  risk_trend <- "No Change"
  
  if (nrow(recent_data) >= 2) {
    lm_fit <- lm(Value ~ Time, data = recent_data)
    coef_summary <- summary(lm_fit)$coefficients
    slope <- coef_summary["Time", "Estimate"]
    p_val <- coef_summary["Time", "Pr(>|t|)"]
    
    if (!is.na(p_val) && p_val < 0.05) {
      trend_direction <- ifelse(slope > 0, "Positive", "Negative")
      risk_trend <- ifelse(slope > 0, "More Risk Averse", "Less Risk Averse")
    }
  }
  
  # Threshold rule: terminal year value vs mean ± 1 SD
  terminal_val <- data_bt$Value[data_bt$Time == yr]
  risk_threshold <- "No Change"
  if (length(terminal_val) == 1) {
    if (terminal_val > bt_mean + bt_sd) risk_threshold <- "More Risk Averse"
    if (terminal_val < bt_mean - bt_sd) risk_threshold <- "Less Risk Averse"
  }
  
  # Save individual plot
  plot <- plot_for_terminal_year_bt(yr)
  filename <- file.path(output_dir, paste0("bt_trend_", yr, ".png"))
  ggsave(filename, plot = plot, width = 8, height = 5, dpi = 300)
  
  tibble(
    Terminal_Year = yr,
    Trend = trend_direction,
    Slope = slope,
    P_value = p_val,
    Risk_Signal_Trend = risk_trend,
    Risk_Signal_Threshold = risk_threshold
  )
})

# 
# Trend summary plot ------------------
# 
bt_trend_plot <- ggplot(bt_summary, aes(x = Terminal_Year, y = Trend, color = Risk_Signal_Trend)) +
  geom_point(size = 3) +
  scale_y_discrete(limits = c("Negative", "None", "Positive")) +
  scale_x_continuous(breaks = seq(min(bt_summary$Terminal_Year),
                                  max(bt_summary$Terminal_Year), 1)) +
  theme_minimal() +
  labs(
    title = "Bottom Temp Anomaly – Trend Calls by Terminal Year (GB)",
    y = "Trend Direction",
    x = "Terminal Year"
  )

ggsave(file.path(output_dir, "bt_trend_summary.png"), bt_trend_plot, width = 8, height = 5, dpi = 300)

# 
# Combined figure for communication -----------------
# 
example_plots_bt <- map(c(2022, 2017, 2012), plot_for_terminal_year_bt)

combined_plot_bt <- wrap_plots(example_plots_bt) / bt_trend_plot + 
  plot_layout(heights = c(2, 1))

ggsave(file.path(output_dir, "bt_trend_combined.png"),
       plot = combined_plot_bt, width = 12, height = 10, dpi = 300)

# 
# Save summary table -------------------------
# 
write.csv(bt_summary, file.path(output_dir, "bt_trend_summary.csv"), row.names = FALSE)

# 
# Threshold classification for each year -------------
#
bt_threshold_class <- data_bt |>
  mutate(Risk_Signal_Threshold = case_when(
    Value > bt_mean + bt_sd ~ "More Risk Averse",
    Value < bt_mean - bt_sd ~ "Less Risk Averse",
    TRUE ~ "No Change"
  ))

#
# Threshold plot: full time series with mean ±1 SD ------------------
# 
bt_threshold_timeseries <- ggplot(bt_threshold_class,
                                  aes(x = Time, y = Value,
                                      color = Risk_Signal_Threshold)) +
  geom_line(color = "grey60") +
  geom_point(size = 2) +
  geom_hline(yintercept = bt_mean, linetype = "dashed", color = "black") +
  geom_hline(yintercept = bt_mean + bt_sd, linetype = "solid", color = "blue") +
  geom_hline(yintercept = bt_mean - bt_sd, linetype = "solid", color = "blue") +
  scale_color_manual(values = c(
    "More Risk Averse" = "red",
    "No Change" = "black",
    "Less Risk Averse" = "green"
  )) +
  theme_minimal() +
  labs(
    title = "Georges Bank Bottom Temp Anomaly – Threshold Classification",
    y = "Bottom Temperature Anomaly (°C)",
    x = "Year",
    color = "Risk Signal"
  )

ggsave(file.path(output_dir, "bt_threshold_timeseries.png"),
       bt_threshold_timeseries, width = 8, height = 5, dpi = 300)
