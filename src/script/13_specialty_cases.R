# Meta Data --------------------------------------------------------------------
#
# Version:      1.1
# Author:       Oleksii Dovhaniuk
# Created on:   2024-04-17
# Updated on:   2024-04-17
#
# Description:  Script which returns functions get_specialty_cases_pl()
#               and get_specialty_cases_df() with plot for number 
#               of cases in regards to different specialties
#               for the inputted data set.
#
# Location:     script/13_specialty_cases.R
#



# Setup the Script -------------------------------------------------------------

source('script/01_read_and_prep.R')

# Number of Cases per Specialty ------------------------------------------------

get_specialty_cases_df <- function(){
  
  rename_if_exists <- function(df, old_name, undefined) {
    if ( exists(old_name, where = df) ) {
      df <- rename(df, undefined = old_name)
    }
    
    return(df)
  }
  
  df <- prepared_df |> 
    select(
      specialty,
      case_type,
      is_cancelled_case
    ) |> 
    
    group_by(
      specialty,
      case_type,
      is_cancelled_case
    ) |> 
    
    summarize(
      count = n()
    ) |> 
    
    pivot_wider(
      names_from = c(case_type, is_cancelled_case),
      values_from = count,
      values_fill = 0
    ) |> 
    
    mutate(
      cancelled = -(Sch_TRUE + Unsch_TRUE),
    ) |> 
    
    select(
      -Sch_TRUE,
      -Unsch_TRUE
    ) |> 
    
    rename(
      scheduled = Sch_FALSE,
      unscheduled = Unsch_FALSE
    ) |> 
    
    rename_if_exists(
      old_name = "_FALSE",
      undefined = "undefined"
    ) |> 
    
    rename_if_exists(
      old_name = "_TRUE",
      undefined = "undefined"
    ) |> 
    
    # mutate(
    #   total_labels = scheduled + unscheduled + undefined,
    #   cancelled_labels = cancelled
    # ) |> 
    
    pivot_longer(
      cols = c("scheduled", "unscheduled", "undefined", "cancelled"),
      names_to = c("type"),
      values_to = "cases"
    )
  
  return(df)
  
}

get_specialty_cases_pl <- function(){
  
  df <- get_specialty_cases_df()
  
  pl <- ggplot(
    df, 
    aes(
      fill=type, 
      y=cases, 
      x=specialty
    )
  ) + 
    geom_bar(
      position="stack", 
      stat="identity", 
      color="black"
    )
    
    # geom_text(
    #   aes(
    #     y = total_labels,
    #     label = total_labels
    #   ),
    #   vjust = -0.5, 
    #   color = "black", 
    #   hjust = 0.5,
    #   fontface = 'bold',
    #   size = 3
    # ) +
    # 
    # geom_text(
    #   aes(
    #     y = cancelled_labels,
    #     label = cancelled_labels
    #   ),
    #   vjust = -0.5, 
    #   color = "black", 
    #   hjust = 0.5,
    #   fontface = 'bold',
    #   size = 3
    # )

  # theme_minimal() +
  # 
  # scale_y_continuous(
  #   breaks = seq(
  #     -10, 
  #     max(df$cases) * 2,
  #     by = 10
  #   )
  # ) 
  
  return (pl)
  
}

# get_specialty_cases_pl()
