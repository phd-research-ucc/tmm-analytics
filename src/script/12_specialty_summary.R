# Meta Data --------------------------------------------------------------------
#
# Version:      1.1
# Author:       Oleksii Dovhaniuk
# Created on:   2024-03-19
# Updated on:   2024-04-17
#
# Description:  The summary analysis of the specialties of the dataset.
#
# Location:     script/12_specialty_summary.R
#



# Setup the Script -------------------------------------------------------------

source('script/01_read_and_prep.R')
library(ggpubr)
source('script/13_specialty_cases.R')
source('script/14_specialty_durations.R')
source('script/15_specialty_demand_rates.R')
source('script/16_specialty_total_rate.R')


# Combined Plot of Specialty Summary -------------------------------------------

specialty_cases_pl <- get_specialty_cases_pl()
specialty_durations_pl <- get_specialty_durations_pl()
specialty_demand_rates_pl <- get_specialty_demand_rates_pl()
specialty_total_rate_pl <- get_specialty_total_rate_pl()

specialty_summary_pl <- ggarrange(
  specialty_cases_pl, 
  specialty_durations_pl, 
  specialty_total_rate_pl,
  specialty_demand_rates_pl, 
  labels = c("A", "B", "C", "D"),
  ncol = 2, nrow = 2)

specialty_summary_pl




# Cases of a Specialty per Theatre ---------------------------------------------

get_cases_of_a_specialty_per_theatre_pl <- function(specialty_target){
  
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
  
  cases_of_a_specialty_df <- prepared_df |> 
    select(
      specialty,
      case_type,
      is_cancelled_case,
      theatre
    ) |> 
    
    filter(
      specialty == specialty_target
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
      unscheduled = "Unsch_FALSE",
    ) |>
    
    pivot_longer(
      cols = c(scheduled, unscheduled, cancelled),
      names_to = c("type"),
      values_to = "cases"
    ) 
  
  
  ggplot(
    cases_of_a_specialty_df, 
    aes(fill=type, y=cases, x=theatre)
  ) + 
    geom_bar(position="stack", stat="identity") +
    theme_minimal() +
    
    scale_y_continuous(
      breaks = seq(
        min(cases_of_a_specialty_df$cases), 
        max(cases_of_a_specialty_df$cases) * 2,
        by = 2
      )
    ) +
    
    theme(
      axis.text.x = element_text(
        hjust = 1,
        vjust = .5
      )
    ) +
    
    theme(
      # panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
}

get_cases_of_a_specialty_per_theatre_pl(43)


