# Meta Data --------------------------------------------------------------------
#
# Version:      1.1
# Author:       Oleksii Dovhaniuk
# Created on:   2024-04-17
# Updated on:   2024-04-17
#
# Description:  Script which returns functions get_specialty_durations_pl()
#               and get_specialty_durations_df() with plot for spent time 
#               in minutes in regards to different specialties
#               for the inputted data set.
#
# Location:     script/14_specialty_duration.R
#



# Setup the Script -------------------------------------------------------------

source('script/01_read_and_prep.R')

# Number of Cases per Specialty ------------------------------------------------

get_specialty_durations_df <- function(){
  
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
  
  return(df)
  
}

get_specialty_durations_pl <- function(){
  
  df <- get_specialty_durations_df()
  
  pl <- ggplot(
    df, 
    aes(fill = type, y = spent_min, x = specialty)
  ) + 
    
    geom_bar(
      position="stack", 
      stat="identity", 
      color="black"
    ) +
    
    geom_text(
      aes(
        y = total,
        label = total
      ),
      vjust = -0.5, 
      color = "black", 
      hjust = 0.5,
      fontface = 'bold',
      size = 3
    )
  
  return (pl)
  
}

# get_specialty_durations_pl()
