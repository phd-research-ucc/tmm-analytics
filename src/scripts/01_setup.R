# Meta Data ---------------------------------------------------------------
#
# TMM File Analytics
# 01_setup.R
#




# Import Packages ----------------------------------------------------------------

library(tidyverse)
library(glue)




# Read Files ---------------------------------------------------------------

entry_data_df <- read.csv('src/data/raw/2023-11-26_tmm_1month_1hospital_data_entry.csv')
mng_theatres_df <- read.csv('src/data/raw/2023-11-26_tmm_1month_1hospital_mng_theatres.csv')




# Split Manage Theatres Data Frame ----------------------------------------

theatre_names_df <- mng_theatres_df[6:11, c("X.1", "X.2")] |>
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
    theatre_open = as.POSIXct(theatre_open, format = "%H:%M", tz = "UTC"),
    theatre_close = as.POSIXct(theatre_close, format = "%H:%M", tz = "UTC"),
    
    core_time_m = as.numeric( 
      difftime(theatre_close, theatre_open, units = "mins")
    ),
    core_time_h = as.numeric( 
      difftime(theatre_close, theatre_open, units = "hours")
    ),
    
    theatre_open = format(theatre_open, format = "%H:%M"),
    theatre_close = format(theatre_close, format = "%H:%M")
  )

# View(theatre_core_time_df)


# Note: it may be necessary to rewrite the following script
# for different input AM/ PM open and close times!
theatre_adhoc_reasons <- c('Staffed',
                           'Unstaffed',
                           'Ring-Fenced',
                           'Planned Closure',
                           'Cancelled List',
                           'Other Use')
theatre_adhocs_df <- mng_theatres_df[6:8, c(13:14, 17:19)] |>
  rename(!!!setNames(
    names(mng_theatres_df)[ c(13:14, 17:19) ], 
    c('theatre_id', 
      'date', 
      'open', 
      'close', 
      'reason')
  )) |>
  
  mutate(
    reason = factor(reason, levels = theatre_adhoc_reasons)
  )
# View(theatre_adhocs_df)




# Clean Entry-Data Data Frame ---------------------------------------------

table(is.na(entry_data_df$Comment))

clean_data_df <- entry_data_df |>
  rename_all(tolower) |>
  rename_all(~ str_replace_all(., "\\.", "_")) |>
  select(-comment) |> 
  mutate(
    theatre = factor(theatre),
    case_type = factor(case_type),
    specialty = factor(specialty),
    surgery_status = factor(surgery_status),
    surgery_start_date = as.Date(surgery_start_date, '%d.%m.%Y'),
    anaesthetic_start = format(strptime(anaesthetic_start, '%H:%M:%S'), '%H:%M:%S'),
    surgery_start = format(strptime(surgery_start, '%H:%M:%S'), '%H:%M:%S'),
    surgery_finish = format(strptime(surgery_finish, '%H:%M:%S'), '%H:%M:%S'),
    anaesthetic_finish = format(strptime(anaesthetic_finish, '%H:%M:%S'), '%H:%M:%S'),
    left_theatre = format(strptime(left_theatre, '%H:%M:%S'), '%H:%M:%S'),
    surgery_end_date = as.Date(surgery_end_date, '%d.%m.%Y')) |> 
  select(
    -surgery_start,
    -surgery_finish,
    -left_theatre
  )
 
View(clean_data_df)




# Summary Dashboard - Tier 3 --------------------------------------

summary_df <- clean_data_df |> 
  summarize(
    min_date = min(surgery_start_date),
    max_date = max(surgery_start_date),
    start_date = min_date - as.integer( format(min_date, '%w') ),
    end_date = max_date - as.integer( format(max_date, '%w') ) + 6,
    weeks_between = as.integer((end_date - start_date + 1) / 7),
    start_week = 1,
    end_week = start_week + weeks_between - 1,
    num_theatres = nlevels(theatre),
    num_specialties = nlevels(specialty)
  ) |> 
  select(start_week, end_week, everything(), -min_date, -max_date)

summary_df



