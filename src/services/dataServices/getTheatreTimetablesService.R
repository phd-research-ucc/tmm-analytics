# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2024-01-31
# Updated on:   2024-01-31
#
# Description:  Exstracts and clears theatre timetables data frame 
#               from the mng_theatre_df.
#
# Location:     src/services/dataServices/getTheatreTimetablesService.R
#


# Service ----------------------------------------------------------------------

get_theatre_timetables <- function(mng_theatres_df){

  library(tidyverse)
  library(janitor)

  source('dataServices/characterToHMSService.R')
  
  mng_theatres_df <- janitor::clean_names(mng_theatres_df)
  
  midnight_hms = character_to_hms('00:00:00')
  midday_hms = character_to_hms('12:00:00')

  theatre_timetables_df <- mng_theatres_df |> 
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

  return(theatre_timetables_df)
}




