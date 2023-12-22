# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2023-12-13
# Updated on:   2023-12-22
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
    early_start = sum(as.integer(early_start), rm.na = TRUE),
    
    late_start = case_when(
      first_surgery_start + duration > theatre_open ~
        difftime(first_surgery_start, theatre_open, units = 'mins'),
      TRUE ~ as.difftime(0, units = 'mins')
    ),
    
    on_time = late_start <= 0
  ) |> 
  
  # Grouping by weeks and theatres:
  group_by(week) |>
  reframe(
    minutes_lost = sum(
      as.integer( late_start[ which(late_start > 0) ] ), 
      rm.na = TRUE
    ),
    early_start = sum(early_start, rm.na = TRUE),
    days_on_time = sum(on_time, rm.na = TRUE) / 30
  ) 
# |> 
  # filter(theatre == 'I')


# View(on_time_start_df)
on_time_start_df




# Render On Time Start Chart ---------------------------------------------------


# Create a plot
on_time_start_p <- ggplot( 
    on_time_start_df, 
    aes(x = factor(week))
  ) +
  
  geom_hline(yintercept = 0, color = 'black') +
  
  geom_bar(
    aes(y = minutes_lost, fill = 'Mins lost due late start'), 
    stat = 'identity', 
    alpha = 0.7,
    position = 'identity',
    width = 0.5
  ) +
  
  geom_line(
    aes(y = days_on_time * 1000, color = '% Days on-time start'), 
    group = 1
  ) +
  
  geom_point(
    aes(y = days_on_time * 1000, color = '% Days on-time start'), 
    shape = 15, 
    size = 3,
    alpha = 0.7
  ) +
  
  geom_text(
    aes(
      y = days_on_time * 1000, 
      label = scales::percent(days_on_time, scale = 100)
    ),
    vjust = -1.5,
    hjust = 0.5,
    fontface = 'bold',
    size = 4
  ) +
  
  geom_text(
    aes(y = minutes_lost, label = minutes_lost),
    vjust = -1.5,
    hjust = 0.5,
    fontface = 'bold',
    size = 3
  ) +
  
  scale_y_continuous(
    # sec.axis = sec_axis(
    #   ~./600,
    #   name = 'Days On-Time Start (%)',
    #   breaks = seq(0, 1, by = 0.2),
    #   labels = scales::percent_format(scale = 100)
    # ),
    # name = 'Minutes Lost due to Late Start',
    # breaks = seq(0, 600, by = 100),
    limits = c(0, 1600)
  ) +
  
  scale_x_discrete(
    labels = paste0('Wk ', on_time_start_df$week)
  ) +
  
  scale_fill_manual( 
    name = '', 
    values = c(
      'Mins lost due late start' = '#faab36',
      'Mins early' = '#fd5901'
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

remove(
  on_time_start_df,
  prepared_df
)




# Display the plot -------------------------------------------------------------

on_time_start_p





