## Developing candidate Climate and Ecosystem Considerations for the New England Fishery Management Council's Risk Policy
## Indicators informed by ToR1 of the 2022 Research Track Assessment of American plaice
##
## Author: M.T.Grezlik
## Date: 05/15/2025

## Indicator: Annual mean bottom temperature anomaly
## Proxy for distribution shifts in response to temperature
## Significant positive correlation with fall and spring mean latitude (i.e. Plaice move Northeast with higher temperatures)

ecodata::plot_bottom_temp_seasonal_gridded(report = "NewEngland")
bt <- ecodata::bottom_temp

# filter for just bt$Var == 'bottom temp anomaly in situ'
insit <- dplyr::filter(bt, Var == 'bottom temp anomaly in situ')

# ggplot a line graph with inst$Time on the x axis and insit$Value on the y axis
library(ggplot2)
ggplot(data = insit, aes(x = Time, y = Value)) +
  geom_line() +
  geom_point() +
  labs(title = "Annual Mean Bottom Temperature Anomaly",
       x = "Year",
       y = "Bottom Temperature Anomaly (°C)") +
  theme_minimal()
