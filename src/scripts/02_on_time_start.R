# Meta Data --------------------------------------------------------------------
#
# Version:      1.1
# Author:       Oleksii Dovhaniuk
# Created on:   2023-12-13
# Updated on:   2024-01-01
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
    start_delta = difftime(first_surgery_start, theatre_open, units = 'mins'),
    type = case_when(
      first_surgery_start + duration < theatre_open ~ 'out-core', 
      start_delta <= 0 ~ 'on-time',
      start_delta > 0 ~ 'late'
    ),
  ) |> 
  
  # Grouping by weeks and theatres:
  group_by(week) |>
  reframe(
    days_on_time = sum(type == 'on-time') / n(),
    late_minutes = sum(
      as.integer( start_delta[ which(type == 'late') ] ), 
      rm.na = TRUE
    ),
    early_minutes = sum(
      as.integer( start_delta[ which(type == 'on-time') ] ), 
      rm.na = TRUE
    )
  ) 


# View(on_time_start_df)
on_time_start_df




# Render On Time Start Chart ---------------------------------------------------

upper_boundary = max(on_time_start_df$late_minutes) + 200
lower_boundary = -upper_boundary
percent_pos_multiplier = on_time_start_df$days_on_time * upper_boundary

on_time_start_p <- ggplot( 
    on_time_start_df, 
    aes(x = factor(week))
  ) +
  
  geom_bar(
    aes(
      y = late_minutes, 
      fill = 'Mins Start Late'
    ), 
    stat = 'identity', 
    alpha = 0.7,
    position = 'identity',
    width = 0.8
  ) +
  
  geom_bar(
    aes(
      y = early_minutes, 
      fill = 'Mins Start Early'
    ), 
    stat = 'identity', 
    alpha = 0.7,
    position = 'identity',
    width = 0.8
  ) +
  
  geom_line(
    aes(
      y = percent_pos_multiplier, 
      color = '% Days on-time start'
    ), 
    group = 1
  ) +
  
  geom_point(
    aes(
      y = percent_pos_multiplier,
      color = '% Days on-time start'
    ), 
    shape = 16, 
    size = 3,
    alpha = 0.7
  ) +
  
  geom_text(
    aes(
      y = percent_pos_multiplier, 
      label = scales::percent(days_on_time, scale = 100)
    ),
    vjust = -1.5,
    hjust = 0.5,
    fontface = 'bold',
    size = 4
  ) +
  
  geom_text(
    aes(
      y = 0, 
      label = late_minutes
    ),
    vjust = -1,
    hjust = 0.5,
    alpha = 0.7,
    fontface = 'bold',
    size = 5,
    colour = '#521B00'
  ) +
  
  geom_text(
    aes(
      y = early_minutes, 
      label = early_minutes,
    ),
    vjust = 1.75,
    hjust = 0.5,
    fontface = 'bold',
    size = 5,
    colour = '#249ea0'
  ) +
  
  geom_hline(yintercept = 0, color = 'black') +
  
  scale_y_continuous(
    limits = c(lower_boundary, upper_boundary)
  ) +
  
  scale_x_discrete(
    labels = paste0('Wk ', on_time_start_df$week)
  ) +
  
  scale_fill_manual( 
    name = '', 
    values = c(
      'Mins Start Late' = '#faab36',
      'Mins Start Early' = '#249ea0',
      ''
    )
  ) +
  
  scale_color_manual( 
    name = '', 
    values = c('% Days on-time start' = '#249ea0')
  ) +
  
  labs(
    title = 'On Time Start (AM)', 
    x = '', 
    y = '',
    fill = 'Time'
  ) +
  
  theme_minimal() +
  
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = 'bottom',
    legend.direction = 'horizontal',
    axis.text.y = element_blank()  
  )




# Clean Up ---------------------------------------------------------------------

# remove(
#   on_time_start_df,
#   prepared_df
# )




# Display the plot -------------------------------------------------------------

on_time_start_p





