# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2024-05-09
# Updated on:   2024-05-11
#
# Description:  The summary analysis of recovery ward data for a month.
#
# Location:     ~/src/script/recovery_wards/02_summary.R
#



# Setup the Script -------------------------------------------------------------

library(ggplot2)
lhms <- lubridate::hms

# tmm_01 <- "src/data/raw/2024-08-09/2023-12-09_tmm-charts-example.xls"
tmm_02 <- "src/data/raw/2024-08-09/Annonmysed_37-40_v2_8_anon.xls"
# tmm_03 <- "src/data/raw/2024-08-09/Annonmysed_41-44_v2_8_anon.xls"
# tmm_04 <- "src/data/raw/2024-08-09/Annonmysed_49-52_v2_8_anon.xls"



# Supporting functions ---------------------------------------------------------

calc_duration <- function(time_finish, time_start){
  
  minutes = as.numeric(
    make_difftime(
      time_finish - time_start,
      units = "minutes"
    )
  );
  
  
  case_when(
    minutes < 0 ~ round(minutes + 24*60),
    TRUE ~ round(minutes)
  )
}





# Combined Plot of Specialty Summary -------------------------------------------


get_recovery_summary <- function(file){
  
  source('~/src/script/recovery_wards/01_get_data.R')
  source('~/src/script/recovery_wards/03_recovery_time_scatter_plots.R')
  source('~/src/script/recovery_wards/04_durations_by_bay.R')
  source('~/src/script/recovery_wards/05_utilisation_by_bay.R')
  source('~/src/script/recovery_wards/06_utilisation_by_dates.R')
  
  df <- get_data(file)
  
  data_entry_df <- df$data_entry |> 
    mutate(
      induction_to_incision_time = calc_duration(surgery_start, anaesthetic_start),
      knife_to_skin_time = calc_duration(surgery_finish, surgery_start),
      anaesthetic_recovery_time = calc_duration(anaesthetic_finish, surgery_finish),
      anaesthetic_time = calc_duration(anaesthetic_finish, anaesthetic_start),
      until_left_theatre_time = calc_duration(left_theatre, anaesthetic_finish),
      ward_waiting_time = calc_duration(ward_called, left_theatre),
      recovery_time = calc_duration(out_of_recov, ward_called)
    ) |> 
    filter(!is.na(induction_to_incision_time)) |> 
    filter(!is.na(anaesthetic_time)) |> 
    filter(!is.na(anaesthetic_recovery_time)) |>
    filter(!is.na(knife_to_skin_time)) |>
    filter(!is.na(ward_waiting_time)) |>
    filter(!is.na(recovery_time))
  
  df$data_entry <- data_entry_df

  # str(df$data_entry)
  
  # View(df$data_entry)
  
  # get_durations_pl(data_entry_df)
  # cases_by_bay_pl <- get_cases_by_bay_pl(df$data_entry)
  # utilisation_by_bay_pl <- get_utilisation_by_bay_pl(df)
  # 
  # recovery_summary_pl <- ggarrange(
  #     cases_by_bay_pl,
  #     utilisation_by_bay_pl,
  #     labels = c("A", "B"),
  #     ncol = 1, 
  #     nrow = 2
  #   )
  
  cases_by_dates_pl <- get_cases_by_dates_pl(df$data_entry)
  utilisation_by_dates_pl <- get_utilisation_by_dates_pl(df)
  
  recovery_summary_pl <- ggarrange(
    cases_by_dates_pl,
    utilisation_by_dates_pl,
      labels = c("A", "B"),
      ncol = 1, 
      nrow = 2
    )

  return(recovery_summary_pl)
  # get_recovery_time_with_dependency_pl(data_entry_df, "anaesthetic_time", "bay")
  # get_recovery_time_with_dependency_pl(data_entry_df, "anaesthetic_recovery_time", "bay")
  # get_recovery_time_with_dependency_pl(data_entry_df, "knife_to_skin_time", "bay")
  # get_recovery_time_with_dependency_pl(data_entry_df, "ward_waiting_time", "bay")
  # get_recovery_time_pl(data_entry_df, "bay")

}

# get_recovery_summary(tmm_02)
# View(df$data_entry)

# source('~/src/script/recovery_wards/01_get_data.R')

# specialty_summary_pl <- ggarrange(
#   specialty_cases_pl, 
#   specialty_durations_pl, 
#   specialty_total_rate_pl,
#   specialty_demand_rates_pl, 
#   labels = c("A", "B", "C", "D"),
#   ncol = 2, nrow = 2)
# 
# specialty_summary_pl