# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2024-01-31
# Updated on:   2024-01-31
#
# Description:  Clears the data entry data frame 
#               from the data_entry_df.
#
# Location:     src/services/dataServices/getDataEntryService.R
#


# Service ----------------------------------------------------------------------

get_data_entry <- function(data_entry_df){

  library(tidyverse)
  library(janitor)

  source('dataServices/characterToHMSService.R')
  
  data_entry_df <- janitor::clean_names(data_entry_df) |> 
    
    mutate(
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
    ) |> 
    
    select(-comment)

  return(data_entry_df)
}




