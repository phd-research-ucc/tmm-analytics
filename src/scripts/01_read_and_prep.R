# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2023-12-13
# Updated on:   2023-12-13
#
# Description:  The script reads and prepares data for 
#               further analysis and plotting
#
# Location:     scripts/01_read_and_prep.R
#



# Setup the Script -------------------------------------------------------------


# load packages
source('scripts/_load_packages.R')

# load custom functions
source('scripts/_functions.R')




# Read Data --------------------------------------------------------------------


entry_data_df <- read.csv(
  'data/raw/2023-12-08_tmm_1month_1hospital_data_entry.csv'
)

mng_theatres_df <- read.csv(
  'data/raw/2023-12-08_tmm_1month_1hospital_mng_theatres.csv'
)




# Split Manage Theaters Data Frame ---------------------------------------------


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
    theatre_open = to_unix_timestamp(theatre_open),
    theatre_close = to_unix_timestamp(theatre_close),
    
    core_time_m = as.integer( 
      difftime(theatre_close, theatre_open, units = 'mins')
    ),
    core_time_h = as.numeric( 
      difftime(theatre_close, theatre_open, units = 'hours')
    )
    
    # theatre_open = format(theatre_open, format = '%H:%M'),
    # theatre_close = format(theatre_close, format = '%H:%M')
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
      to_unix_timestamp('00:00:00'),
      to_unix_timestamp(open) ),
    close = ifelse(
      str_detect(close, '^\\s*$'), 
      to_unix_timestamp('00:00:00'),
      to_unix_timestamp(close) ),
    
    adhoc_time_m = as.integer( 
      difftime(close, open, units = 'mins') ),
    adhoc_time_h = as.integer( 
      difftime(close, open, units = 'hours') ),
    
    open = strftime(open, '%H:%M'),
    close = strftime(close, '%H:%M') ) |> 
  merge(theatre_core_time_df, by.y = 'weekday') |> 
  mutate(
    lost_time_m = ifelse(
      core_time_m - adhoc_time_m > 0,
      core_time_m - adhoc_time_m, 0 ),
    
    lost_time_h = ifelse(
      core_time_h - adhoc_time_h > 0,
      core_time_h - adhoc_time_h, 0 ),
    
    open = to_unix_timestamp(open),
    close = to_unix_timestamp(close)
  ) |> 
  
  select(
    -adhoc_time_m,
    -adhoc_time_h,
    -core_time_m,
    -core_time_h
  )
# View(theatre_adhocs_df)
theatre_adhocs_df




# Clean Entry-Data Data Frame --------------------------------------------------

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
    anaesthetic_start = to_unix_timestamp(anaesthetic_start),
    surgery_start = to_unix_timestamp(surgery_start),
    surgery_finish = to_unix_timestamp(surgery_finish),
    anaesthetic_finish = to_unix_timestamp(anaesthetic_finish),
    left_theatre = to_unix_timestamp(left_theatre),
    surgery_end_date = as.Date(surgery_end_date, '%d.%m.%Y')) |> 
  select(
    -surgery_start,
    -surgery_finish,
    -left_theatre,
    -x,
    -x_1
  )

# View(clean_data_df)




# Prepare Data -----------------------------------------------------------------

adhocs_for_merge_df <- theatre_adhocs_df |> 
  select(
    -theatre_open,
    -theatre_close
  ) |> 
  
  mutate(
    surgery_start_date = date,
    theatre = theatre_id,
    theatre_open = open,
    theatre_close = close,
    core_time_m = as.integer(
      difftime(theatre_close, theatre_open, units = 'mins')
    ),
    core_time_h = as.integer(
      difftime(theatre_close, theatre_open, units = 'hours')
    )
  ) |> 
  
  select(
    surgery_start_date,
    theatre,
    theatre_open,
    theatre_close,
    core_time_m,
    core_time_h
  )

prepared_df <- clean_data_df |> 
  mutate(
    surgery_status = str_trim( str_to_lower(surgery_status) ),
    
    weekday = format(surgery_start_date, '%a'),
    
    anaesthetic_finish = case_when(
      anaesthetic_finish >= anaesthetic_start ~ anaesthetic_finish,
      TRUE ~ anaesthetic_finish + dhours(24)),
    
    duration = difftime(
      anaesthetic_finish, 
      anaesthetic_start, 
      units = 'mins'),
    
    week = cut(
      surgery_start_date, 
      breaks = 'week', 
      labels = FALSE),
    
  ) |> 
  
  merge(theatre_core_time_df, by.y = 'weekday') |> 
  left_join(
    adhocs_for_merge_df, 
    by = c('surgery_start_date', 'theatre'),
    suffix = c('_entry', '_adhoc')
  ) |> 
  
  mutate(
    theatre_open = coalesce(theatre_open_adhoc, theatre_open_entry),
    theatre_close = coalesce(theatre_close_adhoc, theatre_close_entry),
    core_time_m = coalesce(core_time_m_adhoc, core_time_m_entry),
    core_time_h = coalesce(core_time_h_adhoc, core_time_h_entry)
  ) |> 
  
  select(
    -ends_with("_entry"), 
    -ends_with("_adhoc")
  ) |> 
  
  mutate(
    early_start = case_when(
      anaesthetic_start >= theatre_open ~ difftime(theatre_open, 
                                                   theatre_open, 
                                                   units = 'mins'),
      TRUE ~ difftime(theatre_open, anaesthetic_start, units = 'mins')
    ),
    
    over_run = case_when(
      anaesthetic_start > theatre_close ~ difftime(theatre_open, 
                                                   theatre_open, 
                                                   units = 'mins'),
      theatre_close >= anaesthetic_finish ~ difftime(theatre_open, 
                                                     theatre_open, 
                                                     units = 'mins'),
      TRUE ~ difftime(anaesthetic_finish, theatre_close, units = 'mins')
    ),
    
    non_core = case_when(
      anaesthetic_finish <= theatre_open ~ duration,
      anaesthetic_start >= theatre_close ~ duration,
      TRUE ~ difftime(theatre_open, 
                      theatre_open, 
                      units = 'mins'),
    ),
    
    incore_time = case_when(
      ( anaesthetic_start < theatre_open & 
          anaesthetic_finish > theatre_open &
          anaesthetic_finish <= theatre_close ) ~ 
        difftime(anaesthetic_finish, 
                 theatre_open, 
                 units = 'mins'),
      
      ( anaesthetic_start >= theatre_open & 
          anaesthetic_finish <= theatre_close ) ~ 
        duration,
      
      ( anaesthetic_start >= theatre_open &
          anaesthetic_start < theatre_close &
          anaesthetic_finish > theatre_close ) ~ 
        difftime(theatre_close, 
                 anaesthetic_start, 
                 units = 'mins'),
      
      ( anaesthetic_start <= theatre_open & 
          anaesthetic_finish >= theatre_close ) ~ 
        difftime(theatre_close, 
                 theatre_open, 
                 units = 'mins'),
      
      TRUE ~ difftime(theatre_open, 
                      theatre_open, 
                      units = 'mins'),
    ),
    
    is_incore_case = (
      (anaesthetic_start >= theatre_open & anaesthetic_start < theatre_close) |
        (anaesthetic_finish > theatre_open & anaesthetic_finish <= theatre_close)
    ),
    
    is_outcore_case = (
      anaesthetic_finish < theatre_open | anaesthetic_start > theatre_close
    ),
    
    is_cancalled_case = (surgery_status == 'case cancelled'),
    
    outcore_time = (case_when(
      is_outcore_case ~ duration,
      is_incore_case ~ early_start + over_run,
      TRUE ~ duration - duration
    ) )
  )

# View(prepared_df)
# View(filter(prepared_df, theatre == 'C', surgery_start_date == '2020-3-23'))
# View(filter(prepared_df, theatre == 'R', surgery_start_date == '2020-03-12'))
# View(filter(prepared_df, theatre == 'C', surgery_start_date == '2020-03-25'))



# Clean Up ---------------------------------------------------------------------

remove(
  # data frames
  adhocs_for_merge_df,
  # clean_data_df,
  entry_data_df,
  mng_theatres_df,
  theatre_adhoc_reasons,
  # theatre_adhocs_df,
  # theatre_core_time_df,
  theatre_names_df,
  
  # functions
  to_unix_timestamp
)

