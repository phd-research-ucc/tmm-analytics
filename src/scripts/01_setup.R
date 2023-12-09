# Meta Data ---------------------------------------------------------------
#
# TMM File Analytics
# 01_setup.R
#




# Import Packages ----------------------------------------------------------------

library(tidyverse)
library(glue)




# Read Files ---------------------------------------------------------------

entry_data_df <- read.csv('src/data/raw/2023-12-08_tmm_1month_1hospital_data_entry.csv')
mng_theatres_df <- read.csv('src/data/raw/2023-11-26_tmm_1month_1hospital_mng_theatres.csv')




# Split Manage Theatres Data Frame ----------------------------------------

string_to_unix_timestamps <- function(
    string_time, 
    tz = 'UTC',
    format = '%H:%M:%OS',
    origin = '1970-01-01',
    optional = TRUE) {
  
  as.POSIXct(
    gsub(" ", "", string_time), 
    tz = tz, 
    format = format,
    optional = optional)
}


theatre_names_df <- mng_theatres_df[6:11, c('X.1', 'X.2')] |>
  rename(
    theatre_id = X.1,
    theatre_name = X.2
  )
# View(theatre_names_df)


theatre_core_time_df <- mng_theatres_df[6:12, 8:10] |>
  rename(!!!setNames(
      names(mng_theatres_df)[8:10],
      c('weekday', 'theatre_open', 'theatre_close')
    )) |>
  
  mutate(
    theatre_open = string_to_unix_timestamps(theatre_open),
    theatre_close = string_to_unix_timestamps(theatre_close),
    
    core_time_m = as.integer( 
      difftime(theatre_close, theatre_open, units = 'mins')
    ),
    core_time_h = as.numeric( 
      difftime(theatre_close, theatre_open, units = 'hours')
    ),
    
    theatre_open = format(theatre_open, format = '%H:%M'),
    theatre_close = format(theatre_close, format = '%H:%M')
  )

# View(theatre_core_time_df)


# Note: it may be necessary to rewrite the following script
# for different input AM/ PM open and close times!
theatre_adhoc_reasons <- c('Planned Closure',
                           'Ring-Fenced',
                           'Cancelled List',
                           'Other Use')
theatre_adhocs_df <- mng_theatres_df[6:8, c(13:14, 17:19)] |>
  rename(!!!setNames(
    names(mng_theatres_df)[ c(13:14, 17:19) ], 
    c('theatre_id', 
      'date', 
      'open', 
      'close', 
      'reason'))) |>
  mutate( 
    date = as.Date(date, '%d.%m.%Y'),
    reason = factor(reason, levels = theatre_adhoc_reasons),
    weekday = format(date, '%a'),
    
    open = ifelse(
      str_detect(open, '^\\s*$'), 
      string_to_unix_timestamps('00:00:00'),
      string_to_unix_timestamps(open) ),
    close = ifelse(
      str_detect(close, '^\\s*$'), 
      string_to_unix_timestamps('00:00:00'),
      string_to_unix_timestamps(close) ),
    
    adhoc_time_m = as.integer( 
      difftime(close, open, units = 'mins') ),
    adhoc_time_h = as.integer( 
      difftime(close, open, units = 'hours') ),

    open = strftime(open, '%H:%M'),
    close = strftime(close, '%H:%M') ) |> 
  merge(theatre_core_time_df, by.y = "weekday") |> 
  mutate(
    lost_time_m = ifelse(
      core_time_m - adhoc_time_m > 0,
      core_time_m - adhoc_time_m, 0 ),
    lost_time_h = ifelse(
      core_time_h - adhoc_time_h > 0,
      core_time_h - adhoc_time_h, 0 ) ) |> 
  select(
    -adhoc_time_m,
    -adhoc_time_h,
    -core_time_m,
    -core_time_h
  )
# View(theatre_adhocs_df)
theatre_adhocs_df




# Clean Entry-Data Data Frame ---------------------------------------------

table(is.na(entry_data_df$Comment))

clean_data_df <- entry_data_df |>
  rename_all(tolower) |>
  rename_all(~ str_replace_all(., '\\.', '_')) |>
  select(-comment) |> 
  mutate(
    theatre = factor(theatre),
    case_type = factor(case_type),
    specialty = factor(specialty),
    surgery_status = factor(surgery_status),
    surgery_start_date = as.Date(surgery_start_date, '%d.%m.%Y'),
    anaesthetic_start = string_to_unix_timestamps(anaesthetic_start),
    surgery_start = string_to_unix_timestamps(surgery_start),
    surgery_finish = string_to_unix_timestamps(surgery_finish),
    anaesthetic_finish = string_to_unix_timestamps(anaesthetic_finish),
    left_theatre = string_to_unix_timestamps(left_theatre),
    surgery_end_date = as.Date(surgery_end_date, '%d.%m.%Y')) |> 
  select(
    -surgery_start,
    -surgery_finish,
    -left_theatre,
    -x,
    -x_1
  )
 
View(clean_data_df)




# Prepare Data ------------------------------------------------------------

prepared_df <- clean_data_df |> 
  mutate(
    weekday = format(surgery_start_date, '%a'),
    duration = ifelse(
      difftime(anaesthetic_finish, anaesthetic_start, units = 'mins') > 0,
      difftime(anaesthetic_finish, anaesthetic_start, units = 'mins'),
      difftime(anaesthetic_start, anaesthetic_finish, units = 'mins')) )
View(prepared_df)



# Summary Dashboard - Tier 3 --------------------------------------

adhoc_summary_table <- theatre_adhocs_df |> 
  group_by(reason) |>
  summarise(
    total_lost_time_m = sum(lost_time_m),
    total_lost_time_h = sum(lost_time_h)) |> 
  complete(
    reason, 
    fill = list(
      total_lost_time_m = 0, 
      total_lost_time_h = 0))

summary_df <- clean_data_df |> 
  summarize(
    min_date = min(surgery_start_date),
    max_date = max(surgery_start_date),
    start_date = min_date - as.integer( format(min_date, '%w') ),
    end_date = max_date - as.integer( format(max_date, '%w') ) + 6,
    
    num_weeks = as.integer((end_date - start_date + 1) / 7),
    start_week = 1,
    end_week = start_week + num_weeks - 1,
    
    num_theatres = nlevels(theatre),
    num_specialties = nlevels(specialty),
    
    capacity = num_theatres * num_weeks,
    possible_core_time_h = sum(theatre_core_time_df$core_time_h) * capacity,
    possible_core_time_m = sum(theatre_core_time_df$core_time_m) * capacity,
    
    potential_core_time_h = 
      possible_core_time_h - sum(adhoc_summary_table$total_lost_time_h),
    potential_core_time_m = 
      possible_core_time_m - sum(adhoc_summary_table$total_lost_time_m),
    
    unstaffed_time_h = '-',
    unstaffed_time_m = '-',
    staffed_time_h = potential_core_time_h,
    staffed_time_m = potential_core_time_m,
    
    utilisation_live = '??.?%',
    utilisation_goal = '95.0%',
    
    core_cases_live = as.integer(NA),
    core_cases_goal = core_cases_live * 1.19,
    non_core_cases_live = as.integer(NA),
    non_core_cases_goal = 0
  ) |> 
  select(start_week, end_week, everything(), -min_date, -max_date) |> 
  add_row() |> 
  mutate(
    start_week = c(start_week[1], end_week[1]),
    start_date = c(start_date[1], end_date[1]),
    possible_core_time_m = c(possible_core_time_m[1], possible_core_time_h[1]),
    potential_core_time_m = c(potential_core_time_m[1], potential_core_time_h[1]),
    unstaffed_time_m = c(unstaffed_time_m[1], unstaffed_time_h[1]),
    staffed_time_m = c(staffed_time_m[1], staffed_time_h[1]),
    utilisation_live = c(utilisation_live[1], utilisation_goal[1]),
    core_cases_live = c(core_cases_live[1], core_cases_goal[1]),
    non_core_cases_live = c(non_core_cases_live[1], non_core_cases_goal[1]) ) |> 
  rename(
    dates = start_date,
    weeks = start_week,
    possible_core_time = possible_core_time_m,
    potential_core_time = potential_core_time_m,
    unstaffed_time = unstaffed_time_m,
    staffed_time = staffed_time_m,
    utilisation = utilisation_live,
    core_cases = core_cases_live,
    non_core_cases = non_core_cases_live
  ) |> 
  select(
    -end_date,
    -end_week,
    -possible_core_time_h,
    -potential_core_time_h,
    -unstaffed_time_h,
    -staffed_time_h,
    -utilisation_goal,
    -core_cases_goal,
    -non_core_cases_goal) |>
  t() |>
  as.data.frame() |> 
  rename(c1 = V1, c2=V2)

summary_df
View(summary_df)



