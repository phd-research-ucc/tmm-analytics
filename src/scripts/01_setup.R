# Meta Data ---------------------------------------------------------------
#
# TMM File Analytics
# 01_setup.R
#




# Import Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggplot2)
library(glue)




# Read Files ---------------------------------------------------------------

entry_data_df <- read.csv('src/data/raw/2023-12-08_tmm_1month_1hospital_data_entry.csv')
mng_theatres_df <- read.csv('src/data/raw/2023-11-26_tmm_1month_1hospital_mng_theatres.csv')




# Split Manage Theatres Data Frame ----------------------------------------

to_unix_timestamp <- function(
    string_time, 
    tz = 'UTC',
    format = '%H:%M',
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

View(theatre_core_time_df)


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
 
View(clean_data_df)




# Prepare Data ------------------------------------------------------------

prepared_df <- clean_data_df |> 
  mutate(
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
      breaks = "week", 
      labels = FALSE)) |> 
  merge(theatre_core_time_df, by.y = "weekday") |> 
  mutate(
    early_start = case_when(
      anaesthetic_start >= theatre_open ~ NA,
      TRUE ~ difftime(theatre_open, anaesthetic_start, units = 'mins')
    ),
    over_run = case_when(
      anaesthetic_start > theatre_close ~ NA,
      theatre_close >= anaesthetic_finish ~ NA,
      TRUE ~ difftime(anaesthetic_finish, theatre_close, units = 'mins')
    ),
    non_core = case_when(
      anaesthetic_finish <= theatre_open ~ duration,
      anaesthetic_start >= theatre_close ~ duration,
      TRUE ~ NA
    ),
    core_time = case_when(
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
      TRUE ~ NA
    )
  )
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



# On Time Start Chart -----------------------------------------------------

on_time_start_df <- prepared_df |> 
  filter( !is.na(anaesthetic_start) ) |> 
  group_by(surgery_start_date, theatre) |> 
  
  reframe(
    week = week[which.min(anaesthetic_start)],
    first_surgery_start = min(anaesthetic_start, na.rm = TRUE),
    theatre_open = theatre_open[which.min(anaesthetic_start)],
    duration = duration[which.min(anaesthetic_start)],
    early_start = sum(early_start, rm.na = TRUE),
    
    late_start = case_when(
      first_surgery_start + duration > theatre_open ~
        difftime(first_surgery_start, theatre_open, units = 'mins'),
      TRUE ~ NA),
    
    on_time = late_start <= 0 ) |> 
  
  # Grouping by weeks and theatres:
  group_by(week, theatre) |>
  reframe(
    minutes_lost = sum(late_start[ which(late_start > 0) ]),
    early_start = sum(early_start, rm.na = TRUE),
    days_on_time = sum(on_time) / 5
  ) |> 
  filter(theatre == 'I')
  

# View(on_time_start_df)
on_time_start_df


# Create a plot
on_time_start_p <- ggplot( 
    on_time_start_df, 
    aes(x = factor(week)) ) +
  
  geom_bar(
    aes(y = minutes_lost, fill = "Mins lost"), 
    stat = "identity", 
    # fill = "orange", 
    alpha = 0.7,
    position = "identity") +
  
  geom_line(
    aes(y = days_on_time * 600, color = "% Days on-time start"), 
    # color = "green", 
    group = 1) +
  
  geom_point(
    aes(y = days_on_time * 600, color = "% Days on-time start"), 
    shape = 15, 
    # color = "green",
    size = 3,
    alpha = 0.7) +  # Add square points
  
  geom_label(
    aes(y = days_on_time * 600, 
        label = scales::percent(days_on_time, scale = 100)), 
    vjust = -1,
    hjust = 0.4,
    color = "black",
    label.padding = unit(0.2, "lines")) +  # Add labels on top of the line
  
  scale_y_continuous(
    sec.axis = sec_axis(
      ~./600, 
      name = "Days On-Time Start (%)",
      breaks = seq(0, 1, by = 0.2),
      labels = scales::percent_format(scale = 100)),
    
    name = "Minutes Lost due to Late Start",
    breaks = seq(0, 600, by = 100),
    limits = c(0, 600)
  ) +
  
  scale_x_discrete(
    labels = paste0("Wk ", on_time_start_df$week)) +
  
  scale_fill_manual( 
    name = '', 
    values = c("Mins lost" = "#faab36",
               "Mins early" = '#fd5901') ) +
  
  scale_color_manual( 
    name = '', 
    values = c("% Days on-time start" = "#249ea0") ) +
  
  labs(
    title = "On Time Start (AM)", 
    x = "Weeks", 
    y = "Minutes Lost due to Late Start",
    fill = "Time" ) +
  
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),  # Remove y-axis grid lines
    legend.position = "bottom",
    legend.direction = "horizontal" )

on_time_start_p

# Inter Operational Interval Chart ----------------------------------------


# Early Finish/ Over Runs Chart -------------------------------------------


# % Utilisation and Number of Cases in Core Hours -------------------------


# % Utilisation and Number of Cases in Non-Core Hours ---------------------


# % Utilisation Pure Core with Non-Core Time  ---------------------------------------------






