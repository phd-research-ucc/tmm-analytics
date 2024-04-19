# Meta Data --------------------------------------------------------------------
#
# Version:      1.1
# Author:       Oleksii Dovhaniuk
# Created on:   2024-04-17
# Updated on:   2024-04-17
#
# Description:  Script which returns functions get_specialty_demand_rates_pl()
#               and get_specialty_demand_rates_df() with plot for demand rates
#               in regards to different specialties for the inputted data set.
#
# Location:     script/14_specialty_demand_rates.R
#



# Setup the Script -------------------------------------------------------------

source('script/01_read_and_prep.R')


# Specialty Demand Rates -------------------------------------------------------

get_specialty_demand_rates_df <- function(){
  
  df <- prepared_df |> 
    select(
      specialty,
      duration,
      incore_time,
      early_start,
      over_run
    ) |> 
    
    filter(
      !is.na(duration)
    ) |> 
    
    group_by(
      specialty,
    ) |> 
    
    summarize(
      total = sum(duration),
      core = sum(incore_time),
      early_start = sum(early_start),
      over_run = sum(over_run),
      noncore = total - core - early_start - over_run
    ) |> 
    
    pivot_longer(
      cols = c(core, early_start, over_run, noncore),
      names_to = c("type"),
      values_to = "spent_min"
    )
  
  return (df)
}


get_specialty_demand_rates_pl <- function(){
  
  df <- get_specialty_demand_rates_df()
  
  ggplot(
    df, 
    aes(fill = specialty, y = spent_min, x = type)
  ) + 
    
    geom_bar(
      position="fill", 
      stat="identity",
      color = "black"
    ) +
    coord_flip()
}



