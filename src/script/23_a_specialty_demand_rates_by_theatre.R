# Meta Data --------------------------------------------------------------------
#
# Version:      1.1
# Author:       Oleksii Dovhaniuk
# Created on:   2024-04-18
# Updated on:   2024-04-18
#
# Description:  Script which returns functions get_a_specialty_demand_rates_by_theatre_pl()
#               and get_a_specialty_demand_rates_by_theatre_df() with plot for demand rates
#               in regards to a specialty by theatre for the inputted data set.
#
# Location:     script/23_a_specialty_demand_rates_by_theatre.R
#



# Setup the Script -------------------------------------------------------------

source('script/01_read_and_prep.R')


# Specialty Demand Rates -------------------------------------------------------

get_a_specialty_demand_rates_by_theatre_df <- function(target_specialty){
  
  df <- prepared_df |> 
    select(
      specialty,
      duration,
      incore_time,
      early_start,
      over_run,
      theatre
    ) |> 
    
    filter(
      !is.na(duration),
      specialty == target_specialty
    ) |> 
    
    group_by(
      specialty,
      theatre
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


get_a_specialty_demand_rates_by_theatre_pl <- function(target_specialty){
  
  df <- get_a_specialty_demand_rates_by_theatre_df(target_specialty)
  
  pl <- ggplot(
    df, 
    aes(fill = theatre, y = spent_min, x = type)
  ) + 
    
    geom_bar(
      position="fill", 
      stat="identity",
      color = "black"
    ) +
    coord_flip()
  
  return(pl)
}



