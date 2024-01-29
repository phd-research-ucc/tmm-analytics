# Meta Data --------------------------------------------------------------------
#
# Version:      1.1
# Author:       Oleksii Dovhaniuk
# Created on:   2024-01-15
# Updated on:   2024-01-18
#
# Description:  Prepares data for further analysis and plotting.
#
# Location:     services/prepareServices/prepareDataService.R
#



# Service ----------------------------------------------------------------------


prepareDataService <- function(
    data_entry_df,
    mng_theatres_df
  ){

  library(tidyverse)
  library(lubridate)
  library(hms)
  library(janitor)
  
  
  return(data_entry_df)
}


# Functions --------------------------------------------------------------------

character_to_hms <- function(character_hms){
  character_hms <- case_when(
    is.na(character_hms) ~ '00:00:00',
    TRUE ~ character_hms
  )
  
  time <- lubridate::hms(character_hms) |> 
    round()
  
  hms::hms(time)
}



source('services/fileServices/readDemoManageTheatresService.R')
mng_theatres_df <- readDemoManageTheatresService()



# Clean Manage Theatres Data Sheet ---------------------------------------------


mng_theatres_df <- janitor::clean_names(mng_theatres_df)

theatre_names_df <- mng_theatres_df |> 
  select(
    theatre_1,
    th_name,
  )  |>
  
  filter( !is.na(theatre_1) ) |> 
  
  rename(
    theatre_id = theatre_1,
    theatre_name = th_name
  ) |> 
  
  mutate(
    theatre_id = as.factor(theatre_id),
    theatre_name = as.factor(theatre_name)
  )

midnight_hms = character_to_hms('00:00:00')
midday_hms = character_to_hms('12:00:00')

theatre_timetable_df <- mng_theatres_df |> 
  select(
    day,
    th_open,
    th_close
  )  |>
  
  filter( !is.na(day) ) |> 
  
  rename(
    weekday = day,
    theatre_open = th_open,
    theatre_close = th_close
  ) |> 

  mutate(
    weekday = as.factor(weekday),
    theatre_open = case_when(
      grepl(':', theatre_open) ~ character_to_hms(theatre_open),
      TRUE ~ midnight_hms
    ),
    theatre_close = case_when(
      grepl(':', theatre_close) ~ character_to_hms(theatre_close),
      TRUE ~ midnight_hms
    )
  )

  
theatre_adhocs_df <- mng_theatres_df |> 
  select(
    theatre_11,
    date,
    am_open,
    am_close,
    pm_open,
    pm_close,
    reason
  )  |>
  
  filter( !is.na(theatre_11) ) |> 
  
  pivot_longer(
    cols = starts_with('am_') | starts_with('pm_'),
    names_to = c('time_of_day', '.value'),
    names_pattern = '(am|pm)_(.*)'
  ) |>
  
  select(-time_of_day) |> 

  distinct() |> 
  
  group_by(theatre_11, date, reason) |>
  filter( !(open == close & n() > 1) ) |>
  ungroup() |> 
  
  rename(
    theatre_id = theatre_11,
    theatre_adhoc_open = open,
    theatre_adhoc_close = close,
    theatre_adhoc_reason = reason
  ) |> 
  
  mutate(
    theatre_id = as.factor(theatre_id),
    date = as.Date(date),
    weekday = format(date, '%a'),
    theatre_adhoc_open = character_to_hms(theatre_adhoc_open),
    theatre_adhoc_close = character_to_hms(theatre_adhoc_close),
    theatre_adhoc_reason = as.factor(theatre_adhoc_reason)
  ) |> 
  
  merge(
    theatre_timetable_df, 
    by = 'weekday', 
    all.x = FALSE
  ) |> 
  
  mutate(
    theatre_default_open = theatre_close - theatre_open,
    max_open = case_when(
      theatre_adhoc_open > theatre_open ~ theatre_adhoc_open,
      TRUE ~ theatre_open
    ),
    min_close = case_when(
      theatre_adhoc_close < theatre_close ~ theatre_adhoc_close,
      TRUE ~ theatre_close
    ),
    overlap = case_when(
      min_close < max_open ~ min_close - min_close,
      TRUE ~ min_close - max_open
    ),
    theatre_core_min_lost = as.integer(
        (theatre_default_open - overlap) / 60
    ),
    theatre_core_min_remain = as.integer(overlap / 60)
  ) |> 
  
  select(
    -theatre_default_open,
    -max_open,
    -min_close,
    -overlap,
  )



# Clean Data Entry Data Sheet --------------------------------------------------

source('services/fileServices/readDemoDataEntryService.R')
data_entry_df <- readDemoDataEntryService() |> 
  janitor::clean_names() |> 
  mutate(
    # original data fields
    theatre = as.factor(theatre),
    case_type = as.factor(case_type),
    specialty = as.factor(specialty),
    surgery_status = as.factor(surgery_status),
    surgery_start_date = lubridate::ymd(surgery_start_date),
    anaesthetic_start = character_to_hms(anaesthetic_start),
    surgery_start = character_to_hms(surgery_start),
    surgery_finish = character_to_hms(surgery_finish),
    anaesthetic_finish = character_to_hms(anaesthetic_finish),
    left_theatre = character_to_hms(left_theatre),
    surgery_end_date = lubridate::ymd(surgery_end_date),
    weekday = format(surgery_start_date, '%a')
    
    # data fields from manage theatres data frame
    
  ) |> 
  select(
    -comment
  )


  
  
  
  
  
  
