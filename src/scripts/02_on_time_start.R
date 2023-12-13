# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2023-12-13
# Updated on:   2023-12-13
#
# Description:  The script uses prepared data 
#               from 01_read_and_prep.R script to
#               render On Time Start Chart, which
#               shows percentage of days when surgery
#               cases started on time and the minutes
#               lost due to late starts. 
#
#               Time span - 4 weeks.
#               X axis stands for weeks.
#
# Location:     scripts/02_on_time_start.R
#



# Setup the Script -------------------------------------------------------------

source('scripts/01_read_and_prep.R')





# Mine Needed Data -------------------------------------------------------------


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
      TRUE ~ difftime(theatre_open, theatre_open, units = 'mins')
    ),
    
    on_time = late_start <= 0 ) |> 
  
  # Grouping by weeks and theatres:
  group_by(week, theatre) |>
  reframe(
    minutes_lost = sum(
      late_start[ which(late_start > 0) ], 
      rm.na = TRUE
    ),
    early_start = sum(early_start, rm.na = TRUE),
    days_on_time = sum(on_time, rm.na = TRUE) / 5
  ) |> 
  filter(theatre == 'I')


# View(on_time_start_df)
on_time_start_df




# Render On Time Start Chart ---------------------------------------------------


on_time_start_p <- ggplot( 
  on_time_start_df, 
  aes(x = factor(week)) ) +
  
  geom_bar(
    aes(y = minutes_lost, fill = 'Mins lost'), 
    stat = 'identity', 
    alpha = 0.7,
    position = 'identity') +
  
  geom_line(
    aes(y = days_on_time * 600, color = '% Days on-time start'), 
    group = 1) +
  
  geom_point(
    aes(y = days_on_time * 600, color = '% Days on-time start'), 
    shape = 15, 
    size = 3,
    alpha = 0.7) +  # Add square points
  
  geom_label(
    aes(y = days_on_time * 600, 
        label = scales::percent(days_on_time, scale = 100)), 
    vjust = -1,
    hjust = 0.4,
    color = 'black',
    label.padding = unit(0.2, 'lines')) +  # Add labels on top of the line
  
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
    x = 'Weeks', 
    y = 'Minutes Lost due to Late Start',
    fill = 'Time' ) +
  
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),  # Remove y-axis grid lines
    legend.position = 'bottom',
    legend.direction = 'horizontal' )

on_time_start_p



# Clean Up ---------------------------------------------------------------------

remover(on_time_start_df)


