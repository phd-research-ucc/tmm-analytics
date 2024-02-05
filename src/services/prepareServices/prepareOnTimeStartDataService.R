# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2024-02-05
# Updated on:   2024-02-05
#
# Description:  Prepares data for plotting On Time Start Chart.
#
# Location:     src/services/prepareServices/prepareOnTimeStartDataService.R
#


# Service ----------------------------------------------------------------------

prepare_on_time_start_data <- function(
      data_entry_df, 
      theatre_timetables_df,
      theatre_adhocs_df
    ){

  library(tidyverse)
  
  data_entry_df <-
  theatre_timetables_df <-
  theatre_adhocs_df <-

  theatre_adhocs_df <- theatre_adhocs_df |>
    rename(
      surgery_start_date = date,
      theatre_open = theatre_adhoc_open,
      theatre_close = theatre_adhoc_close
    )
  
  prepared_df <- data_entry_df |>

    merge(
      theatre_adhocs_df, 
      by = 'surgery_start_date', 
      all.y = TRUE
    ) |>

    mutate(
      weekday = format(surgery_start_date, '%a')
      theatre_open = case_when(
        is.na(theatre_open) ~ theatre_timetables_df$theatre_open,
        TRUE ~ theatre_open
      ),
      theatre_close = case_when(
        is.na(theatre_close) ~ theatre_timetables_df$theatre_close,
        TRUE ~ theatre_close
      )
    ) 


  on_time_start_df <- prepared_df |> 

    filter( !is.na(anaesthetic_start) ) |> 
  
    group_by(surgery_start_date, theatre) |> 
    
    reframe(
      week = week[which.min(anaesthetic_start)],
      first_surgery_start = min(anaesthetic_start, na.rm = TRUE),
      theatre_open = theatre_open[which.min(anaesthetic_start)],
      duration = duration[which.min(anaesthetic_start)],
      start_delta = difftime(first_surgery_start, theatre_open, units = 'mins'),
      type = case_when(
        first_surgery_start + duration < theatre_open ~ 'out-core', 
        start_delta <= 0 ~ 'on-time',
        start_delta > 0 ~ 'late'
      ),
    ) |> 
    
    # Grouping by weeks and theatres:
    group_by(week) |>
    reframe(
      days_on_time = sum(type == 'on-time') / n(),
      late_minutes = sum(
        as.integer( start_delta[ which(type == 'late') ] ), 
        rm.na = TRUE
      ),
      early_minutes = sum(
        as.integer( start_delta[ which(type == 'on-time') ] ), 
        rm.na = TRUE
      )
    ) 

  return(on_time_start_df)
}




