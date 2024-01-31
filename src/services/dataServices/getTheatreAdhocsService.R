# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2024-01-31
# Updated on:   2024-01-31
#
# Description:  Exstracts and clears theatre ad-hocs data frame 
#               from the mng_theatre_df.
#
# Location:     src/services/dataServices/getTheatreAdhocsService.R
#


# Service ----------------------------------------------------------------------

get_theatre_adhocs <- function(mng_theatres_df){

  library(tidyverse)
  library(janitor)

  source('dataServices/characterToHMSService.R')
  
  mng_theatres_df <- janitor::clean_names(mng_theatres_df)
  
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
  )

  return(theatre_adhocs_df)
}




