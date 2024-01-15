# Meta Data --------------------------------------------------------------------
#
# Version:      1.1
# Author:       Oleksii Dovhaniuk
# Created on:   2024-01-15
# Updated on:   2024-01-15
#
# Description:  Reads the demo Manage Theatres data sheet form CSV or Excel file.
#
# Location:     services/prepareServices/prepareManageTheatresService.R
#



# Service ----------------------------------------------------------------------


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