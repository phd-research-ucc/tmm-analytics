# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2023-12-13
# Updated on:   2023-12-22
#
# Description:  The script uses prepared data 
#               from 01_read_and_prep.R script to
#               render Inter Operation Interval Chart,
#               which shows minutes lost due to turnover time
#               between two surgeries, and minutes saved
#               due to surgery overlaps.
#
#               Time span - 4 weeks.
#               X axis stands for days.
#
# Location:     scripts/03_inter_operational_interval.R
#



# Setup the Script -------------------------------------------------------------

source('scripts/01_read_and_prep.R')





# Mine Needed Data -------------------------------------------------------------


inter_op_interval_df <- prepared_df |> 
  filter(
    !is.na(anaesthetic_start), 
    !is.na(anaesthetic_finish)
  ) |> 
  group_by(surgery_start_date, theatre) |> 
  arrange(surgery_start_date, theatre, anaesthetic_start) |> 
  
  select(
    theatre, 
    surgery_start_date,
    anaesthetic_start,
    anaesthetic_finish,
    surgery_end_date,
    duration
  ) |> 
  
  mutate(
    invert_interval = case_when(
      lag(surgery_start_date) != surgery_start_date ~ NA,
      lag(theatre) != theatre ~ NA,
      TRUE ~ difftime(
        lag(anaesthetic_finish), 
        anaesthetic_start, 
        units = 'mins'
      )
    ),
    overlap_time = pmin(-invert_interval, 0),
    lost_time = pmax(-invert_interval, 0)
  ) |>
  
  reframe(
    trend = sum(invert_interval, na.rm = TRUE) / 2,
    total_overlap = sum(overlap_time, na.rm = TRUE),
    total_lost = sum(lost_time, na.rm = TRUE)
  ) |> 
  
  group_by(surgery_start_date) |> 
  reframe(
    total_overlap = sum(total_overlap, na.rm = TRUE),
    total_lost = sum(total_lost, na.rm = TRUE)
  )

inter_op_interval_df




# Render Inter Operation Interval Chart ----------------------------------------


inter_op_interval_p <- ggplot( 
  inter_op_interval_df,
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
  
  geom_text(
    aes(
      y = total_lost,
      label = total_lost
    ),
    vjust = -1,
    hjust = 0.5,
    fontface = 'bold',
    size = 3
    ) +
  
  geom_text(
    aes(
      y = total_overlap,
      label = total_overlap
    ),
    vjust = 2,
    hjust = 0.5,
    fontface = 'bold',
    size = 3,
    color = '#249ea0'
  ) +
  
  scale_fill_manual( 
    name = '', 
    values = c(
      'Mins total turnover' = '#faab36',
      'Mins total overlap' = '#249ea0')
    ) +
  
  scale_color_manual( 
    name = '', 
    values = c('Trend' = '#fd5901')
  ) +
  
  scale_y_continuous(
    limits = c(
      -max(inter_op_interval_df$total_lost) - 40, 
      max(inter_op_interval_df$total_lost) + 40
    )
  ) +
  
  labs(
    title = 'Inter Operation Interval', 
    x = '', 
    y = 'Minutes',
    fill = 'Time'
  ) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.5),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = 'bottom',
    legend.direction = 'horizontal',
    text = element_text(size = 12, family = 'sans'),
    axis.title.y = element_blank(), 
    axis.text.y = element_blank()
  )




# Clean Up ---------------------------------------------------------------------

remove(
  prepared_df,
  inter_op_interval_df
)




# Display the Plot -------------------------------------------------------------

inter_op_interval_p


