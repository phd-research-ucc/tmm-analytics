# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2024-05-09
# Updated on:   2024-05-09
#
# Description:  Retrieves and prepares on recovery wards data for analysis.
#
# Location:     ~/src/script/recovery_wards/01_get_data.R
#


# Setup the Script -------------------------------------------------------------

library(glue)
library(readxl)
library(janitor)
library(tidyverse)
library(hms)
library(lubridate)




# Supporting functions ---------------------------------------------------------


seconds_to_hms <- function(char_seconds){
  as_hms(
    round(
      as.numeric(char_seconds) * 24 * 3600
      )
    )
}

to_ymd_hms <- function(start_date, current_time, previous_time){
  
  is_next_day = current_time < previous_time;
  time <- format(current_time, "%H:%M:%S");
  date <- case_when(
    is_next_day ~ as.Date(start_date) + 1,
    .default = as.Date(start_date)
  );
  
  return(ymd_hms(paste(date, time)))
}

# start_date = as.POSIXct("2024-05-21 00:00:01");
# current_time = as.POSIXct("1990-01-01 10:20:40");
# previous_time = as.POSIXct("1990-01-01 10:20:50");
# 
# to_ymd_hms(start_date, current_time, previous_time)

# Read the XLS File ------------------------------------------------------------

get_data_entry <- function(file){
  
  if(!is.character(file)) stop("file must be a string type");
  
  data_entry <- read_excel(file, sheet = 'Data Entry') |> 
    clean_names() |> 
    mutate(
      theatre = as.factor(theatre),
      surgeon = as.factor(surgeon),
      case_type = as.factor(case_type),
      specialty = as.factor(specialty),
      surgery_status = as.factor(surgery_status),
      patient_called_dt = to_ymd_hms(surgery_start_date, patient_called, patient_called),
      arrival_dt = to_ymd_hms(surgery_start_date, arrival, patient_called),
      into_th_or_ar_dt = to_ymd_hms(surgery_start_date, into_th_or_ar, patient_called),
      anaesthetic_start_dt = to_ymd_hms(surgery_start_date, anaesthetic_start, patient_called),
      surgery_start_dt = to_ymd_hms(surgery_start_date, surgery_start, patient_called),
      surgery_finish_dt = to_ymd_hms(surgery_start_date, surgery_finish, patient_called),
      anaesthetic_finish_dt = to_ymd_hms(surgery_start_date, anaesthetic_finish, patient_called),
      left_theatre_dt = to_ymd_hms(surgery_start_date, left_theatre, patient_called),
      bay = as.factor(bay),
      ward_called_dt = to_ymd_hms(surgery_start_date, ward_called, patient_called),
      out_of_recov_dt = to_ymd_hms(surgery_start_date, out_of_recov, patient_called),
      surgery_start_date = as.Date(surgery_start_date),
      surgery_end_date = as.Date(surgery_end_date)
    ) |> 
    select(
      "theatre",
      "case_type",
      "specialty",
      "surgery_status",
      "surgery_start_date",
      "patient_called_dt",
      "arrival_dt",
      "into_th_or_ar_dt",
      "anaesthetic_start_dt",
      "surgery_start_dt",
      "surgery_finish_dt",
      "anaesthetic_finish_dt",
      "left_theatre_dt",
      "bay",
      "ward_called_dt",
      "out_of_recov_dt",
      "surgery_end_date"
    ) |> 
    rename(
      patient_called = "patient_called_dt",
      arrival = "arrival_dt",
      into_th_or_ar = "into_th_or_ar_dt",
      anaesthetic_start = "anaesthetic_start_dt",
      surgery_start = "surgery_start_dt",
      surgery_finish = "surgery_finish_dt",
      anaesthetic_finish = "anaesthetic_finish_dt",
      left_theatre = "left_theatre_dt",
      ward_called = "ward_called_dt",
      out_of_recov = "out_of_recov_dt"
    )
  
  current_bay_levels <- levels(data_entry$bay)
  new_bay_levels <- current_bay_levels[
    order( nchar(current_bay_levels) )
  ]
  
  data_entry <- data_entry |> 
    mutate(
      bay = factor(bay, levels=new_bay_levels)
    )
  
  return(data_entry)
  
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

get_data <- function(file){
  return(
    list(
      data_entry = get_data_entry(file),
      theatres = get_theatres(file),
      theatre_core_time = get_theatre_core_time(file),
      theatre_adhocs = get_theatre_adhocs(file),
      recovery_wards = get_recovery_wards(file),
      recovery_core_time = get_recovery_core_time(file),
      recovery_adhocs = get_recovery_adhocs(file)
    )
  )
}


# data <- get_data("~/src/data/raw/2024-05-09/Annonmysed_37-40_v2_8_anon.xls")
# 
# saveRDS(data, "~/src/data/clean/2024-05-21/Annonmysed_37-40_v2_8_anon.rds")

files <- c(
  # "2023-12-09_tmm-charts-example",
  "Annonmysed_37-40_v2_8_anon"
  # "Annonmysed_41-44_v2_8_anon",
  # "Annonmysed_49-52_v2_8_anon"
);

raw_data_path <- "~/src/data/raw/2024-05-09";
clean_data_path <- "~/src/data/clean/2024-05-21";

for (file in files) {
  raw_file <- glue("{raw_data_path}/{file}.xls");
  save_to_file <- glue("{clean_data_path}/{file}.rds");

  # View(get_data(raw_file))
  
  data <- get_data(raw_file)
  saveRDS(data, save_to_file)
}

