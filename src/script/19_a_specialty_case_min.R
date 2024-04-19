# Meta Data --------------------------------------------------------------------
#
# Version:      1.1
# Author:       Oleksii Dovhaniuk
# Created on:   2024-04-17
# Updated on:   2024-04-17
#
# Description:  Script which returns functions get_a_specialty_case_min_pl()
#               and get_a_specialty_case_min_df() with plot for minutes spent 
#               of cases in regards to a single specialty
#               for the inputted data set.
#
# Location:     script/18_a_specialty_case_min.R
#



# Setup the Script -------------------------------------------------------------

source('script/01_read_and_prep.R')




# Minutes of Cases for a Specialty ---------------------------------------------

get_a_specialty_case_min_df <- function(target_specialty) {
  
  df <- prepared_df |> 
    select(
      specialty,
      duration,
      incore_time,
      early_start,
      over_run,
      surgery_start_date
    ) |> 
    
    filter(
      !is.na(duration),
      specialty == target_specialty
    ) |> 
    
    group_by(
      specialty,
      surgery_start_date
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
    ) |> 
    
    rename(
      date = surgery_start_date
    )
 
  return(df) 
}
  
 
get_a_specialty_case_min_pl <- function(target_specialty) {
  
  df <- get_a_specialty_case_min_df(target_specialty)
  
  pl <- ggplot(
    df, 
    aes(fill = type, y = spent_min, x = date)
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
    ) +
    
    scale_x_date(
      date_breaks = "1 day", 
      date_labels = "%Y-%m-%d"
    ) +
    
    # scale_y_continuous(
    #   breaks = seq(
    #     0, 
    #     max(df$total),
    #     by = 100
    #   )
    # ) +
    
    theme(
      axis.text.x = element_text(
        angle = 90, 
        hjust = 1,
        vjust = .5
      )
    ) +
    
    theme(
      # panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
  
  return(pl)
}

