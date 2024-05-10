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
library(hms)




# Supporting functions ---------------------------------------------------------


seconds_to_hms <- function(char_seconds){
  as.hms(
    round(
      as.numeric(char_seconds) * 24 * 3600
      )
    )
}



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
  
}

get_theatres <- function(file){
  
  if(!is.character(file)) stop("file must be a string type");
  
  theatres <- read_excel(
    file, 
    sheet = 'Mng Theatres', 
    range = cell_cols("C:K")
  );
  
  colnames(theatres) <- as.character(theatres[4, ]);
  
  theatres <- theatres[-(1:4), ] |> 
    clean_names()
    
}

get_theatre_core_time <- function(file){
  if(!is.character(file)) stop("file must be a string type");
  
  core_time <- read_excel(
    file, 
    sheet = 'Mng Theatres', 
    range = cell_cols("M:P")
  );
  
  colnames(core_time) <- as.character(core_time[5, ]);
  
  core_time <- core_time[-(1:5), ] |> 
    clean_names() |> 
    mutate(
      day = as.factor(day),
      th_open = seconds_to_hms(th_open),
      th_close = seconds_to_hms(th_close),
      poss_time = as.numeric(poss_time)
    )
}

get_theatre_adhocs <- function(file){
  
  if(!is.character(file)) stop("file must be a string type");
  
  adhocs <- read_excel(
    file, 
    sheet = 'Mng Theatres', 
    range = cell_cols("R:X")
  );
  
  colnames(adhocs) <- as.character(adhocs[3, ]);
  
  adhocs <- adhocs[-(1:3), ] |> 
    clean_names() |> 
    mutate(
      theatre_no = as.factor(theatre_no),
      date = as_date(as.numeric(date)) - years(70) - days(1),
      am_open = seconds_to_hms(am_open),
      am_close = seconds_to_hms(am_close),
      pm_open = seconds_to_hms(pm_open),
      pm_close = seconds_to_hms(pm_close),
      reason = as.factor(reason)
    )
}

get_recovery_wards <- function(file){
  
  if(!is.character(file)) stop("file must be a string type");
  
  recovery_wards <- read_excel(
    file, 
    sheet = 'Mng Recovery', 
    range = cell_cols("C:E")
  );
  
  colnames(recovery_wards) <- as.character(recovery_wards[1, ]);
  
  recovery_wards <- recovery_wards[-(1:1), ] |> 
    clean_names()
  
}

get_recovery_core_time <- function(file){
  if(!is.character(file)) stop("file must be a string type");
  
  recovery_core_time <- read_excel(
    file, 
    sheet = 'Mng Recovery', 
    range = cell_cols("G:I")
  );
  
  colnames(recovery_core_time) <- as.character(recovery_core_time[1, ]);
  
  recovery_core_time <- recovery_core_time[-(1:1), ] |> 
    clean_names() |>
    mutate(
      b_day = as.factor(b_day),
      bay_open = seconds_to_hms(bay_open),
      bay_closed = seconds_to_hms(bay_closed),
    )
}

get_recovery_adhocs <- function(file){
  
  if(!is.character(file)) stop("file must be a string type");
  
  adhocs <- read_excel(
    file, 
    sheet = 'Mng Recovery', 
    range = cell_cols("K:Q")
  );
  
  colnames(adhocs) <- as.character(adhocs[1, ]);

  adhocs <- adhocs[-(1:1), ] |>
    clean_names() |>
    mutate(
      bay_no = as.factor(bay_no),
      b_date = as_date(as.numeric(b_date)) - years(70) - days(1),
      bay_am_open = seconds_to_hms(bay_am_open),
      bay_am_close = seconds_to_hms(bay_am_close),
      bay_pm_open = seconds_to_hms(bay_pm_open),
      bay_pm_close = seconds_to_hms(bay_pm_close),
      closed = as.factor(closed)
    )
}

tmm_01 <- "src/data/raw/2024-08-09/2023-12-09_tmm-charts-example.xls"
tmm_02 <- "src/data/raw/2024-08-09/Annonmysed_37-40_v2_8_anon.xls"
tmm_03 <- "src/data/raw/2024-08-09/Annonmysed_41-44_v2_8_anon.xls"
tmm_04 <- "src/data/raw/2024-08-09/Annonmysed_49-52_v2_8_anon.xls"

View(get_recovery_adhocs(tmm_01))





