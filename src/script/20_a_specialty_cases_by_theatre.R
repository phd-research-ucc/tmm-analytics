# Meta Data --------------------------------------------------------------------
#
# Version:      1.1
# Author:       Oleksii Dovhaniuk
# Created on:   2024-04-18
# Updated on:   2024-04-18
#
# Description:  Script which returns functions get_a_specialty_cases_by_theatre_pl()
#               and get_a_specialty_cases_by_theatre_df() with plot for number 
#               of cases in regards to a single specialty by theatre
#               for the inputted data set.
#
# Location:     script/20_a_specialty_cases_by_theatre.R
#



# Setup the Script -------------------------------------------------------------

source('script/01_read_and_prep.R')

# Number of Cases for a Specialty ----------------------------------------------

get_a_specialty_cases_by_theatre_df <- function(target_specialty) {
  
  add_empty_cases <- function(df) {
    empty_cases <- data.frame(
      specialty = first(df$specialty),
      case_type = c("Sch", "Sch", "Unsch", "Unsch", "", ""),
      is_cancelled_case = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
      theatre = first(df$theatre)
    )
    
    df <- bind_rows(df, empty_cases)
    
    return(df)
  }
  
  df <- prepared_df |> 
    select(
      specialty,
      case_type,
      is_cancelled_case,
      theatre
    ) |> 
    
    filter(
      specialty == target_specialty
    ) |> 
    
    add_empty_cases() |>
    
    group_by(
      specialty,
      case_type,
      is_cancelled_case,
      theatre
    ) |> 
    
    summarize(
      count = n()
    ) |>
    
    
    pivot_wider(
      names_from = c(case_type, is_cancelled_case),
      values_from = count,
      values_fill = 0
    ) |> 
    
    rename(
      undefined1 = "_FALSE",
      undefined2 = "_TRUE",
    ) |> 
    
    mutate(
      cancelled = -(Sch_TRUE + Unsch_TRUE),
      undefined = undefined1 + undefined2
    ) |> 
    
    select(
      -Sch_TRUE,
      -Unsch_TRUE,
      -undefined1,
      -undefined2
    ) |> 
    
    rename(
      scheduled = "Sch_FALSE",
      unscheduled = "Unsch_FALSE"
    ) |>
    
    pivot_longer(
      cols = c(scheduled, unscheduled, cancelled),
      names_to = c("type"),
      values_to = "cases"
    ) 
  
  return(df)
}
  
  
get_a_specialty_cases_by_theatre_pl <- function(target_specialty) {
  
  df <- get_a_specialty_cases_by_theatre_df(target_specialty)
  
  pl <- ggplot(
    df, 
    aes(fill=type, y=cases, x=theatre)
  ) + 
    geom_bar(
      position="stack", 
      stat="identity",
      color="black"
    ) +
    
    scale_y_continuous(
      breaks = seq(
        0, 
        max(df$cases) * 2,
        by = 2
      )
    ) +
    
    theme(
      # panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
  
  return(pl)
  
}