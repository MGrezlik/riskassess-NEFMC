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

ecodata::plot_bottom_temp(report = "NewEngland")

## need to edit this script to only plot GB