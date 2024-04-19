# Meta Data --------------------------------------------------------------------
#
# Version:      1.1
# Author:       Oleksii Dovhaniuk
# Created on:   2024-04-17
# Updated on:   2024-04-17
#
# Description:  Script which returns functions get_a_specialty_cases_pl()
#               and get_a_specialty_cases_df() with plot for number 
#               of cases in regards to a single specialty
#               for the inputted data set.
#
# Location:     script/18_a_specialty_cases.R
#



# Setup the Script -------------------------------------------------------------

source('script/01_read_and_prep.R')

# Number of Cases for a Specialty ----------------------------------------------

get_a_specialty_cases_df <- function(target_specialty) {
  
  add_empty_cases <- function(df) {
    empty_cases <- data.frame(
      specialty = first(df$specialty),
      case_type = c("Sch", "Sch", "Unsch", "Unsch", "", ""),
      is_cancelled_case = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
      surgery_start_date = first(df$surgery_start_date)
    )
    
    df <- bind_rows(df, empty_cases)
    
    return(df)
  }
  
  df <- prepared_df |> 
    select(
      specialty,
      case_type,
      is_cancelled_case,
      surgery_start_date
    ) |> 
    
    filter(
      specialty == target_specialty
    ) |> 
    
    add_empty_cases() |>
    
    group_by(
      specialty,
      case_type,
      is_cancelled_case,
      surgery_start_date
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
      unscheduled = "Unsch_FALSE",
      date = surgery_start_date
    ) |>
    
    pivot_longer(
      cols = c(scheduled, unscheduled, cancelled),
      names_to = c("type"),
      values_to = "cases"
    ) 
  
  return(df)
}
  
  
get_a_specialty_cases_pl <- function(target_specialty) {
  
  df <- get_a_specialty_cases_df(target_specialty)
  
  pl <- ggplot(
    df, 
    aes(fill=type, y=cases, x=date)
  ) + 
    geom_bar(
      position="stack", 
      stat="identity",
      color="black"
    ) +
    
    scale_x_date(
      date_breaks = "1 day", 
      date_labels = "%Y-%m-%d"
    ) +
    
    scale_y_continuous(
      breaks = seq(
        min(df$cases), 
        max(df$cases) * 2,
        by = 2
      )
    ) +
    
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