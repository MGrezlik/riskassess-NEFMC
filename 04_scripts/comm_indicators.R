## Developing candidate Commercial Fishery Characterizations for the New England Fishery Management Council's Risk Policy
## Indicators informed by the Risk Policy Statement and Concept (2025)
##
## Author: M.T.Grezlik
## Date: 05/15/2025

## Indicator: Effective Shannon index of top n ports

# ecodata::plot_comdat(report = "MidAtlantic", varName = "Fleet diversity in revenue")

# not sure why the above code is not working. I'll try in manually for now


# call in commercial fishery diversity data
data <- ecodata::commercial_div

# plot fleet diversity in revenue for New England
data |> 
  dplyr::filter(Var == "Fleet diversity in revenue") |>
  dplyr::filter(EPU == "NE") |>
  #plot Value by Time
  ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::ylab("Effective Shannon index")
  
# Above looks good but shows Effective Shannon index for all NE ports

# Joe suggested using ecodata::engagement to get effective Shannon index of top n ports

data <- ecodata::engagement

# filter for NE commercial data only
com_data <- data |> 
  dplyr::filter(Fishery == 'Commercial') |> 
  dplyr::filter(EPU == 'NE')

# select top 3 ports by Eng
com_data |> 
  dplyr::arrange(dplyr::desc(Eng)) |> 
  dplyr::slice_head(n =3)

# Top 3 ports are New Bedford, MA; Gloucester, MA; and Narragansett/Point Judith, RI

com_data |> 
  dplyr::arrange(dplyr::desc(Eng)) |> 
  dplyr::slice_head(n =5)

# Top 5 would include Portland, ME; and Chatham, MA. Engagement falls off sharply when going to 5 ports