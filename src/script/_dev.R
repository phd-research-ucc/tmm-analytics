# Meta Data ---------------------------------------------------------------
#
# Version:      1.1
# Author:       Oleksii Dovhaniuk
# Created on:   2023-11-22
# Updated on:   2023-12-22
#
# Description:  The script is a monolith development file for 
#               all job scripts of this project. It is mainly 
#               for the inner uses of testing and trying new ideas.
#               
#               It should not be used for knitting the reports!!!
#
# Location:     script/_dev.R
#

# Load Packages ----------------------------------------------------------------

source('script/_load_packages.R')




# Read Files ---------------------------------------------------------------

entry_data_df <- read.csv('data/raw/2023-12-08_tmm_1month_1hospital_data_entry.csv')
mng_theatres_df <- read.csv('data/raw/2023-11-26_tmm_1month_1hospital_mng_theatres.csv')




# Split Manage Theatres Data Frame ----------------------------------------

to_unix_timestamp <- function(
    string_time, 
    tz = 'UTC',
    format = '%H:%M',
    origin = '1970-01-01',
    optional = TRUE) {
  
  as.POSIXct(
    gsub(' ', '', string_time), 
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
 
# View(clean_data_df)




# Prepare Data ------------------------------------------------------------

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
    -ends_with('_entry'), 
    -ends_with('_adhoc')
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



# On Time Start Chart -----------------------------------------------------

on_time_start_df <- prepared_df |> 
  filter( !is.na(anaesthetic_start) ) |> 
  group_by(surgery_start_date, theatre) |> 
  
  reframe(
    week = week[which.min(anaesthetic_start)],
    first_surgery_start = min(anaesthetic_start, na.rm = TRUE),
    theatre_open = theatre_open[which.min(anaesthetic_start)],
    duration = duration[which.min(anaesthetic_start)],
    early_start = sum(as.integer(early_start), rm.na = TRUE),
    
    late_start = case_when(
      first_surgery_start + duration > theatre_open ~
        difftime(first_surgery_start, theatre_open, units = 'mins'),
      TRUE ~ as.difftime(0, units = 'mins')
    ),
    
    on_time = late_start <= 0
    ) |> 
  
  # Grouping by weeks and theatres:
  group_by(week, theatre) |>
  reframe(
    minutes_lost = sum(
      as.integer( late_start[ which(late_start > 0) ] ), 
      rm.na = TRUE
    ),
    early_start = sum(early_start, rm.na = TRUE),
    days_on_time = sum(on_time, rm.na = TRUE) / 5
  ) |> 
  filter(theatre == 'I')
  

# View(on_time_start_df)
on_time_start_df


# Create a plot
on_time_start_p <- ggplot( 
    on_time_start_df, 
    aes(x = factor(week)) ) +
  
  geom_bar(
    aes(y = minutes_lost, fill = 'Mins lost'), 
    stat = 'identity', 
    alpha = 0.7,
    position = 'identity',
    width = 0.5) +
  
  geom_line(
    aes(y = days_on_time * 600, color = '% Days on-time start'), 
    group = 1) +
  
  geom_point(
    aes(y = days_on_time * 600, color = '% Days on-time start'), 
    shape = 15, 
    size = 3,
    alpha = 0.7) +  # Add square points
  
  # geom_label(
  #   aes(y = days_on_time * 600, 
  #       label = scales::percent(days_on_time, scale = 100)), 
  #   vjust = -1,
  #   hjust = 0.4,
  #   color = 'black',
  #   label.padding = unit(0.2, 'lines')) +  # Add labels on top of the line
 
   geom_text(
    aes(
      y = days_on_time * 600, 
      label = scales::percent(days_on_time, scale = 100)
    ),
    vjust = -1.5,
    hjust = 0.5,
    fontface = 'bold',
    size = 4) +
  
  # geom_text(
  #   aes(y = days_on_time * 600, 
  #       label = scales::percent(days_on_time, scale = 100)), 
  #   vjust = -1,
  #   hjust = 0.4,
  #   color = 'black',
  #   label.padding = unit(0.2, 'lines')) +  # Add labels on top of the line
  
  scale_y_continuous(
    sec.axis = sec_axis(
      ~./600, 
      name = 'Days On-Time Start (%)',
      breaks = seq(0, 1, by = 0.2),
      labels = scales::percent_format(scale = 100)),
    
    name = 'Minutes Lost due to Late Start',
    breaks = seq(0, 600, by = 100),
    limits = c(0, 600)
  ) +
  
  scale_x_discrete(
    labels = paste0('Wk ', on_time_start_df$week)) +
  
  scale_fill_manual( 
    name = '', 
    values = c('Mins lost' = '#faab36',
               'Mins early' = '#fd5901') ) +
  
  scale_color_manual( 
    name = '', 
    values = c('% Days on-time start' = '#249ea0') ) +
  
  labs(
    title = 'On Time Start (AM)', 
    x = '', 
    y = 'Minutes Lost due to Late Start',
    fill = 'Time' ) +
  
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),  # Remove y-axis grid lines
    legend.position = 'bottom',
    legend.direction = 'horizontal' )

on_time_start_p




# Inter Operational Interval Chart ----------------------------------------

inter_op_interval_df <- prepared_df |> 
  filter( !is.na(anaesthetic_start), !is.na(anaesthetic_finish) ) |> 
  group_by(surgery_start_date, theatre) |> 
  arrange(surgery_start_date, theatre, anaesthetic_start) |> 
  
  select(
    theatre, 
    surgery_start_date,
    anaesthetic_start,
    anaesthetic_finish,
    surgery_end_date,
    duration ) |> 
  
  mutate(
    invert_interval = case_when(
      lag(surgery_start_date) != surgery_start_date ~ NA,
      lag(theatre) != theatre ~ NA,
      TRUE ~ difftime(
        lag(anaesthetic_finish), 
        anaesthetic_start, 
        units = 'mins') ),
    
    overlap_time = pmin(-invert_interval, 0),
    lost_time = pmax(-invert_interval, 0) ) |>

  reframe(
    trend = sum(invert_interval, na.rm = TRUE) / 2,
    total_overlap = sum(overlap_time, na.rm = TRUE),
    total_lost = sum(lost_time, na.rm = TRUE) ) 
  # |> 
  # 
  # filter(theatre == 'I')

# View(inter_op_interval_df)
inter_op_interval_df


# Create a plot
inter_op_interval_p <- ggplot( 
  filter(
    inter_op_interval_df,
    theatre == 'O'
  ),
  aes(x = factor(surgery_start_date)) ) +
  
  geom_hline(yintercept = 0, color = 'black') +
  
  geom_bar(
    aes(y = total_lost, fill = 'Mins total turnover'), 
    stat = 'identity', 
    alpha = 0.7,
    position = 'identity') +
  
  geom_bar(
    aes(y = total_overlap, fill = 'Mins total overlap'), 
    stat = 'identity', 
    alpha = 0.7,
    position = 'identity') +
  
  
  # geom_line(
  #   aes(y = trend, color = 'Trend'),
  #   group = 1,
  #   # linejoin = 'round',
  #   linetype = 'dashed') +
  
  # geom_point(
  #   aes(y = trend, color = 'Trend'),
  #   shape = 4,
  #   size = 3,
  #   alpha = 0.7) +
  
  geom_text(
    aes(
      y = total_lost,
      label = total_lost),
    vjust = -1,
    hjust = 0.5,
    fontface = 'bold',
    size = 3) +

  geom_text(
    aes(
      y = total_overlap,
      label = total_overlap),
    vjust = 1.5,
    hjust = 0.5,
    fontface = 'bold',
    size = 3,
    color = '#249ea0'
  ) +

  scale_fill_manual( 
    name = '', 
    values = c('Mins total turnover' = '#faab36',
               'Mins total overlap' = '#249ea0') ) +
  
  scale_color_manual( 
    name = '', 
    values = c('Trend' = '#fd5901') ) +
  
  scale_y_continuous(
    # breaks = seq(
    #   as.integer( -max(inter_op_interval_df$total_lost) ), 
    #   as.integer(  max(inter_op_interval_df$total_lost) ),
    #   by = 50
    # ),
    limits = c(
      -max(inter_op_interval_df$total_lost), 
       max(inter_op_interval_df$total_lost)
    )
  ) +
  
  labs(
    title = 'Inter Operation Interval', 
    x = '', 
    y = 'Minutes',
    fill = 'Time' ) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.5),
    panel.grid.major.y = element_blank(),  # Remove y-axis grid lines
    panel.grid.minor.y = element_blank(),  # Remove y-axis grid lines
    legend.position = 'bottom',
    legend.direction = 'horizontal',
    text = element_text(size = 12, family = 'sans'),
    axis.title.y = element_blank(), 
    axis.text.y = element_blank()
  )


inter_op_interval_p




# Early Finish/ Over Runs Chart -------------------------------------------

over_runs_df <- prepared_df |> 
  filter(
    !is.na(anaesthetic_start),
    !is.na(anaesthetic_finish)
  ) |> 
  group_by(week, theatre) |> 
  reframe( total_over_run = sum(over_run, na.rm = TRUE) )

View(over_runs_df)


early_finish_df <- prepared_df |> 
  filter(
    !is.na(anaesthetic_start),
    !is.na(anaesthetic_finish),
    anaesthetic_finish <= theatre_close
  ) |> 
  group_by(surgery_start_date, theatre) |> 

  reframe(
    week = week[ which.max(anaesthetic_finish) ],
    theatre_close = theatre_close[ which.max(anaesthetic_finish) ],
    duration = duration[ which.max(anaesthetic_finish) ],
    last_core_case_finish = max( anaesthetic_finish, na.rm = TRUE ),
    
    early_finish = difftime(
      last_core_case_finish,
      theatre_close, 
      units = 'mins')) |> 
  
  # Grouping by weeks and theatres:
  group_by(week, theatre) |>
  reframe( total_early_finish = sum(early_finish) )
  

View(early_finish_df)

early_finish_over_run_df <- inner_join(
    early_finish_df, 
    over_runs_df, 
    by = c('week', 'theatre')) |> 
  mutate( trend = (total_early_finish * 1.5 + total_over_run) / 2 )

summary_early_late_finish_df <- early_finish_over_run_df |> 
  summarise(
    max_ealy_finish = -min(total_early_finish),
    max_over_run = max(total_over_run),
    limit = max(max_ealy_finish, max_over_run)
  )

# View(early_finish_over_run_df)


# Create a plot
early_finish_over_run_p <- ggplot( 
  filter(
    early_finish_over_run_df, 
    theatre == 'C'
  ), 
  aes(x = factor(week)) ) +
  
  geom_hline(yintercept = 0, color = 'black') +
  
  geom_bar(
    aes(y = total_early_finish, fill = 'Mins lost due early finish'), 
    stat = 'identity', 
    alpha = 0.7,
    position = 'identity',
    width = 0.5
  ) +
  
  geom_bar(
    aes(y = total_over_run, fill = 'Mins of over run'), 
    stat = 'identity', 
    alpha = 0.7,
    position = 'identity',
    width = 0.5
  ) +
  
  # geom_line(
  #   aes(y = trend, color = 'Trend'),
  #   group = 1,
  #   # linejoin = 'round',
  #   linetype = 'dashed') +
  
  # geom_point(
  #   aes(y = trend, color = 'Trend'),
  #   shape = 4,
  #   size = 5,
  #   alpha = 0.7) +
  
  geom_text(
    aes(
      y = total_over_run, 
      label = total_over_run
    ),
    vjust = -1,
    hjust = 0.5,
    fontface = 'bold',
    size = 3) +
  
  geom_text(
    aes(
      y = total_early_finish, 
      label = total_early_finish
    ),
    vjust = 1.75,
    hjust = 0.5,
    fontface = 'bold',
    size = 3) +
  
  scale_y_continuous(
    limits = c(
      -summary_early_late_finish_df$limit, 
       summary_early_late_finish_df$limit
    )
  ) +
  
  scale_fill_manual( 
    name = '', 
    values = c('Mins lost due early finish' = '#fd5901',
               'Mins of over run' = '#faab36') ) +
  
  scale_x_discrete(
    labels = paste0('Wk ', early_finish_over_run_df$week)) +
  
  # scale_color_manual( 
  #   name = '', 
  #   values = c('Trend' = '#fd5901') ) +
  
  labs(
    title = 'Early Finish (-)/ Over Run (+) (PM)', 
    x = '', 
    y = '',
    fill = 'Time' ) +
  
  theme_minimal() +
  theme(
    # axis.text.x = element_text(angle = -90, hjust = 0),
    panel.grid.major.y = element_blank(),  # Remove y-axis grid lines
    panel.grid.minor.y = element_blank(),  # Remove y-axis grid lines
    legend.position = 'bottom',
    legend.direction = 'horizontal',
    text = element_text(size = 12, family = 'sans'),
    axis.text.y = element_blank()  
  )

early_finish_over_run_p


# % Utilisation and Number of Cases in Core Hours -------------------------

utilisation_and_cases_df <- prepared_df |> 
  group_by(surgery_start_date, theatre) |> 
  
  reframe(
    incore_cases = sum(is_incore_case, na.rm = TRUE),
    outcore_cases = sum(is_outcore_case, na.rm = TRUE),
    cancelled_cases = sum(is_cancalled_case, na.rm = TRUE),
    total_cases = incore_cases + outcore_cases + cancelled_cases,
    
    total_core_time = as.numeric( core_time_m[ which.max(core_time_m) ] ),
    total_incore_time = as.numeric( sum(incore_time, na.rm = TRUE) ),
    total_outcore_time = as.numeric( sum(outcore_time, na.rm = TRUE) ),
    # 
    # incore_time_usage = total_incore_time / total_core_time,
    # outcore_time_usage = total_outcore_time / total_core_time,
    # total_time_usage = incore_time_usage + outcore_time_usage
  ) |> 
  
  group_by(surgery_start_date) |> 
    reframe(
      incore_cases = sum(incore_cases, na.rm = TRUE),
      outcore_cases = sum(outcore_cases, na.rm = TRUE),
      cancelled_cases = sum(cancelled_cases, na.rm = TRUE),
      total_cases = incore_cases + outcore_cases + cancelled_cases,
      
      total_core_time = sum(total_core_time, na.rm = TRUE),
      total_incore_time = sum(total_incore_time, na.rm = TRUE),
      total_outcore_time = sum(total_outcore_time, na.rm = TRUE),
      
      incore_time_usage = total_incore_time / total_core_time,
      outcore_time_usage = total_outcore_time / total_core_time,
      total_time_usage = incore_time_usage + outcore_time_usage
  ) 

utilisation_and_cases_plotting_df <- utilisation_and_cases_df |> 
  
  tidyr::gather(
    key = 'case_type', 
    value = 'num_cases', 
    outcore_cases,
    cancelled_cases,
    incore_cases) |> 
  
  mutate( 
    surgery_start_date = factor(
      surgery_start_date,
      levels = unique(surgery_start_date)
    ),
    case_type = factor(
      case_type,
      levels = unique(case_type)
    )
  ) 
# |> 
#   
#   filter(theatre == 'L')


# View(utilisation_and_cases_df)


# Create a plot
utilisation_and_cases_p <- ggplot( 
  utilisation_and_cases_plotting_df, 
  aes(x = surgery_start_date) ) +
  
  geom_bar(
    aes(y = num_cases, 
        fill = case_type),
    stat = 'identity',
    alpha = 0.5,
    position = 'stack') +
  
  # geom_bar(
  #   aes(y = outcore_cases, fill = 'Num of Out-Core'), 
  #   stat = 'identity',
  #   alpha = 0.5,
  #   position = 'stack') +
  # 
  # geom_bar(
  #   aes(y = incore_cases, fill = 'Num of In-Core'), 
  #   stat = 'identity',
  #   alpha = 0.5,
  #   position = 'stack') +
  # 
  # geom_bar(
  #   aes(y = cancelled_cases, fill = 'Num of Cancelled'), 
  #   stat = 'identity',
  #   alpha = 0.5,
  #   position = 'stack') +


  # geom_ribbon(
  #   aes(
  #     ymin = total_time_usage * 5, 
  #     ymax = incore_time_usage * 5,
  #     fill = 'Out-Core Utilisaiton'), 
  #   # fill = 'blue', 
  #   alpha = 0.3
  # ) +
  
  geom_hline(
    yintercept = 0, 
    color = 'black'
  ) +
  
  geom_hline(
    yintercept = 1 * 50, 
    color = 'black',
    linetype = 'dashed'
  ) +
  
  
  geom_line(
    aes(
      y = total_time_usage * 50, 
      color = 'Total Utilisaiton'),
    # color = '#faab36',
    linetype = 'dashed',
    group = 2
  ) +
  
  geom_point(
    aes(
      y = total_time_usage * 50, 
      color = 'Total Utilisaiton'),
    shape = 16,
    # color = '#faab36',
    size = 1
  ) +
  
  geom_text(
    aes(
      y = total_time_usage * 50, 
      label = scales::percent(
        total_time_usage, 
        scale = 100,
        accuracy = 1,
      ),
      color = 'Total Utilisaiton'
    ),
    vjust = -1.2,
    hjust = 0.5,
    fontface = 'bold',
    size = 3
  ) +
  
  geom_line(
    aes(
      y = incore_time_usage * 50, 
      color = 'In-Core Utilisation'),
    group = 1
  ) +
  
  geom_point(
    aes(
      y = incore_time_usage * 50, 
      color = 'In-Core Utilisation'),
    shape = 19,
    size = 1
  ) +
  
  geom_text(
    aes(
      y = incore_time_usage * 50, 
      label = scales::percent(
        incore_time_usage, 
        scale = 100,
        accuracy = 1
      )
    ),
    vjust = 2,
    hjust = 0.5,
    fontface = 'bold',
    size = 3
  ) +
  
  geom_ribbon(
    aes(
      x = as.integer( surgery_start_date ),
      ymin = incore_time_usage * 50,
      ymax = total_time_usage * 50,
      fill = 'outcore_cases'
    ),
    # fill = 'red',
    alpha = .1
  ) +

  # geom_ribbon(
  #   aes(x = surgery_start_date,
  #       ymin = incore_time_usage * 50, 
  #       ymax = total_time_usage * 50, 
  #       fill = 'outcore_cases'), 
  #   alpha = 0.3
  # ) +

  # geom_text(
  #   aes(
  #     y = max(total_over_run), 
  #     label = total_over_run),
  #   position = position_stack(vjust = 2.3),
  #   fontface = 'bold',
  #   size = 3) +
  # 
  # geom_text(
  #   aes(
  #     y = max(total_over_run), 
  #     label = total_early_finish),
  #   position = position_stack(vjust = 2),
  #   fontface = 'bold',
  #   size = 3) +
  
  scale_fill_manual( 
    name = '', 
    values = c('incore_cases' = '#249ea0',
               'outcore_cases' = '#faab36',
               'cancelled_cases' = '#fd5901') ) +
  
  scale_color_manual( 
    name = '', 
    values = c('In-Core Utilisation' = '#249ea0',
               'Total Utilisaiton' = '#faab36') ) +
  
  labs(
    title = 'Time Utilisation and Number of Cases', 
    x = '', 
    y = '% Utilisation',
    fill = 'Num Cases' ) +
  
  scale_y_continuous(
    sec.axis = sec_axis(
      ~./50, 
      name = 'Utilisation',
      breaks = seq(0, 2.0, by = 0.2),
      labels = scales::percent_format(scale = 100)),
    
    name = 'Number of Cases',
    breaks = seq(0, 100, by = 10),
    limits = c(0, 100)
  ) +
  
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.5),
    # panel.grid.major.x = element_blank(),  # Remove x-axis grid lines
    # panel.grid.minor.y = element_blank(),
    legend.position = 'bottom',
    # legend.direction = 'horizontal',
    text = element_text(size = 12, family = 'sans') )

print(utilisation_and_cases_p)


# Arranged Plots - Tier 2 ------------------------------------------------------



arranged_plots <- (
    on_time_start_p + 
    inter_op_interval_p +
    early_finish_over_run_p 
) +
  plot_layout(ncol = 3, nrow = 1, widths = c(1, 3, 1))

arranged_plots / utilisation_and_cases_p


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

util_summary_df <- utilisation_and_cases_df |> 
  summarize(
    utilisation = sum(total_incore_time) / sum(total_core_time),
    core_cases = floor( sum(incore_cases) ),
    expected_core_cases = floor( (1.95 - utilisation) * core_cases ),
    non_core_cases = sum(outcore_cases)
  )

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
    
    utilisation_live = scales::percent(
      util_summary_df$utilisation, 
      scale = 100,
      accuracy = 1
    ),
    utilisation_goal = '95.0%',
    
    core_cases_live = util_summary_df$core_cases,
    core_cases_goal = util_summary_df$expected_core_cases,
    non_core_cases_live = util_summary_df$non_core_cases,
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
# View(summary_df)
knitr::kable(summary_df, format = 'markdown')



# Available Core Time Donut Chart ----------------------------------------------


# Create test data.
data <- data.frame(
  category = c('Potential', 'Blocked'),
  time = c(
    as.integer(summary_df$c2[8]),
    as.integer(summary_df$c2[7]) - as.integer(summary_df$c2[8])
  )
)

# Compute percentages
data$fraction <- data$time / sum(data$time)

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$category, ':\n', data$time, ' hrs')


data$persent <- paste0(
  floor( as.integer(summary_df$c1[8]) / as.integer(summary_df$c1[7]) * 100 ), 
  '%'
)

# Make the plot
available_core_time_p <-ggplot(
    data, 
    aes(
      ymax = ymax, 
      ymin = ymin, 
      xmax = 4, 
      xmin = 3, 
      fill = category
    )
  ) +
  
  geom_rect(
    alpha = 0.7
  ) +
  
  geom_label( 
    x = 4, 
    aes( 
      y = labelPosition,
      label = label
    ), 
    size = 6
  ) +
  
  geom_text(
    x = 2,
    aes(
      y = c(0),
      label = persent
    ),
    size = 16,
    fontface = 'bold',
    color = '#0B4F6C'
  ) +
  
  scale_fill_manual(
    values = c(
      'Potential' = '#2DC5C8',
      'Blocked' = '#faab36'
    )
  ) +
  
  # scale_fill_brewer(palette = 1) +
  coord_polar(theta = 'y') +
  xlim( c(2, 4) ) +
  theme_void() +
  theme(legend.position = 'none')




# Blocked Core Time Pie Chart --------------------------------------------------


# Create test data.
data <- adhoc_summary_table |> 
  mutate(
    fraction = total_lost_time_m / sum(total_lost_time_m),
    percent = paste0( floor(fraction * 1000) / 10 , '%' )
  ) |> 
  rename(
    time = total_lost_time_m,
    label_time = total_lost_time_h
  )

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head( data$ymax, n=-1) )

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$reason, ':\n', data$label_time, ' hr')

# Make the plot
blocked_core_time_p <-ggplot(
  data, 
  aes(
    ymax = ymax, 
    ymin = ymin, 
    xmax = 4, 
    xmin = 2, 
    fill = reason
  )
) +
  
  geom_rect(
    alpha = 0.7
  ) +
  
  # geom_label( 
  #   x = 4, 
  #   aes( 
  #     y = labelPosition,
  #     label = label
  #   ), 
  #   size = 6
  # ) +
  
  geom_label(
    x = 3.5,
    aes(
      y = labelPosition,
      label = percent
    ),
    size = 5,
    fontface = 'bold',
    color = 'black',
    alpha = 0.8
  ) +
  
  scale_fill_manual(
    values = c(
      'Planned Closure' = '#2DC5C8',
      'Ring-Fenced' = '#faab36',
      'Cancelled List' = '#FD5901',
      'Other Use' = '#995D81'
    )
  ) +
  
  # scale_fill_brewer(palette = 1) +
  coord_polar(theta = 'y') +
  xlim( c(2, 4) ) +
  theme_void() +
  theme(
    # legend.position = 'bottom'
  )


# Utilised Core Time Donut Chart -----------------------------------------------


# Create test data.
data <- data.frame(
  category = c('Utilised', 'Unutilised'),
  time = c(
    sum(utilisation_and_cases_df$total_incore_time),
    sum(
      utilisation_and_cases_df$total_core_time - 
      utilisation_and_cases_df$total_incore_time
    )
  )
) |> 
  mutate(
    fraction = time / sum(utilisation_and_cases_df$total_core_time),
    ymax = cumsum(fraction),
    ymin = c( 0, head(ymax, n = -1) ),
    labelPosition = (ymax + ymin) / 2,
    label = paste0(
      category, 
      ':\n', 
      floor(time / 60), 
      ' hr'
    ),
    percent = paste0(
        floor( as.integer(fraction * 1000) ) / 10, 
        '%'
    )
  )


# Make the plot
utilised_core_time_p <- ggplot(
  data, 
  aes(
    ymax = ymax, 
    ymin = ymin, 
    xmax = 4, 
    xmin = 2, 
    fill = category
  )
) +
  
  geom_rect(
    alpha = 0.7
  ) +
  
  # geom_label( 
  #   x = 4, 
  #   aes( 
  #     y = labelPosition,
  #     label = label
  #   ), 
  #   size = 4
  # ) +
  
  geom_label(
    x = 3.5,
    aes(
      y = labelPosition,
      label = percent
    ),
    size = 5,
    fontface = 'bold',
    color = 'black',
    alpha = 0.8
  ) +
  
  scale_fill_manual(
    values = c(
      'Unutilised' = '#FD5901',
      'Utilised' = '#2DC5C8'
    )
  ) +
  
  # scale_fill_brewer(palette = 1) +
  coord_polar(theta = 'y') +
  xlim( c(2, 4) ) +
  theme_void() +
  theme(
    # legend.position = 'none'
  )






# Arrange Plots - Tier 3 -------------------------------------------------------


arranged_summary_plots <- (
  available_core_time_p +
  (blocked_core_time_p / utilised_core_time_p)
)

arranged_summary_plots

