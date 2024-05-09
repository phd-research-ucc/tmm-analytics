# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2024-05-09
# Updated on:   2024-05-09
#
# Description:  Retrieves and prepares on recovery wards data for analysis.
#
# Location:     script/recovery_wards/01_get_data.R
#


# Setup the Script -------------------------------------------------------------

library(glue)
library(readxl)
library(janitor)
library(tidyverse)



# Read the XLS File ------------------------------------------------------------

get_data_entry <- function(file){
  
  if(!is.character(file)) stop("file must be a string type");
  
  data_entry <- read_excel(file, sheet = 'Data Entry') |> 
    clean_names() |> 
    select(
      "theatre",
      "surgeon",
      "case_type",
      "specialty",
      "surgery_status",
      "surgery_start_date",
      "patient_called",
      "arrival",
      "into_th_or_ar",
      "anaesthetic_start",
      "surgery_start",
      "surgery_finish",
      "anaesthetic_finish",
      "left_theatre",
      "bay",
      "ward_called",
      "out_of_recov",
      "surgery_end_date"
    ) |> 
    mutate(
      theatre = as.factor(theatre),
      surgeon = as.factor(surgeon),
      case_type = as.factor(case_type),
      specialty = as.factor(specialty),
      surgery_status = as.factor(surgery_status),
      surgery_start_date = as.Date(surgery_start_date),
      patient_called = as_datetime(patient_called),
      arrival = as_datetime(arrival),
      into_th_or_ar = as_datetime(into_th_or_ar),
      anaesthetic_start = as_datetime(anaesthetic_start),
      surgery_start = as_datetime(surgery_start),
      surgery_finish = as_datetime(surgery_finish),
      anaesthetic_finish = as_datetime(anaesthetic_finish),
      left_theatre = as_datetime(left_theatre),
      bay = as.factor(bay),
      ward_called = (ward_called),
      out_of_recov = as_datetime(out_of_recov),
      surgery_end_date = as.Date(surgery_end_date)
    )

  return(data_entry);
  # str(data_entry)
  # View(data_entry)
}


View(get_data_entry("src/data/raw/2024-08-09/Annonmysed_37-40_v2_8_anon.xls"))




