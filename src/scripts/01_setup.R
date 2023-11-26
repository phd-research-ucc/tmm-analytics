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
  select(-comment)

View(clean_data_df)


