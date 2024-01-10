# Meta Data --------------------------------------------------------------------
#
# Version:      1.1
# Author:       Oleksii Dovhaniuk
# Created on:   2023-12-13
# Updated on:   2024-01-02
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
    is_incore_case,
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
    total_lost = sum(lost_time, na.rm = TRUE),
    max_lost = max(lost_time, na.rm = TRUE),
    max_overlap = min(total_overlap, na.rm = TRUE),
    num_cases = n()
  ) |> 
  
  group_by(surgery_start_date) |> 
  reframe(
    total_overlap = sum(total_overlap, na.rm = TRUE),
    total_lost = sum(total_lost, na.rm = TRUE),
    max_turnover = max(max_lost),
    theatre_with_max_turnover = theatre[ which.max(max_lost) ],
    avg_turnover = round(
      as.numeric( total_lost / sum(num_cases) ),
      1
    ),
    max_overlap = min(max_overlap)
  ) |> 
  
  mutate(
    # interval_avg = total_overlap / total_cases,
    weekday_abbr = format(surgery_start_date, '%a'),
    month_abbr = format(surgery_start_date, '%b'),
    day_date = format(surgery_start_date, '%d'),
    x_date_label = glue('{weekday_abbr}\n{day_date}\n{month_abbr}')
  ) |> 
  
  arrange(surgery_start_date)

inter_op_interval_df




# Render Inter Operation Interval Chart ----------------------------------------

upper_boundary = max(inter_op_interval_df$total_lost)
lower_boundary = min(inter_op_interval_df$total_overlap)
max_avg_turnover = max(inter_op_interval_df$avg_turnover)


inter_op_interval_p <- ggplot( 
    inter_op_interval_df,
    aes(x = reorder(x_date_label, surgery_start_date))
  ) +
  
  geom_bar(
    aes(
      y = total_lost, 
      fill = 'Total IoI'
    ), 
    stat = 'identity', 
    alpha = 0.7,
    position = 'identity'
  ) +
  
  geom_bar(
    aes(
      y = total_overlap, 
      fill = 'Total overlap'
    ), 
    stat = 'identity', 
    alpha = 0.7,
    position = 'identity'
  ) +
  
  geom_bar(
    aes(
      y = max_turnover,
      fill = 'Max IoI',
    ), 
    alpha = 0.5,
    stat = 'identity'
  ) +
  
  geom_bar(
    aes(
      y = max_overlap,
      fill = 'Max overlap',
    ), 
    alpha = 0.7,
    stat = 'identity'
  ) +
  
  geom_text(
    aes(
      y = max_turnover,
      label = theatre_with_max_turnover,
    ),
    vjust = 1.5,
    hjust = 0.5,
    fontface = 'bold',
    size = 5,
    colour = '#FDE6C3'
  ) +
  
  geom_line(
    aes(
      y = avg_turnover, 
      color = 'Avg IoI'
    ), 
    group = 1
  ) +
  
  geom_point(
    aes(
      y = avg_turnover,
      color = 'Avg IoI'
    ), 
    shape = 15, 
    size = 2,
    alpha = 0.7
  ) +
  
  geom_text(
    aes(
      y = avg_turnover,
      label = avg_turnover,
      color = 'Avg IoI'
    ),
    vjust = -0.75,
    hjust = 0.5,
    fontface = 'bold',
    size = 3
  ) +
  
  geom_text(
    aes(
      y = total_lost,
      label = total_lost
    ),
    vjust = -1,
    hjust = 0.5,
    fontface = 'bold',
    size = 4
  ) +
  
  geom_text(
    aes(
      y = max_turnover,
      label = max_turnover,
    ),
    vjust = -0.25,
    hjust = 0.5,
    fontface = 'bold',
    size = 4,
    colour = '#fd5901'
  ) +

  
  geom_text(
    aes(
      y = total_overlap,
      label = total_overlap
    ),
    vjust = 2,
    hjust = 0.5,
    fontface = 'bold',
    size = 4,
    color = '#249ea0'
  ) +
  
  geom_hline(yintercept = 0, color = 'black') +
  
  scale_fill_manual( 
    name = '', 
    values = c(
        'Total IoI' = '#faab36',
        'Total overlap' = '#249ea0',
        'Max IoI' = '#fd5901',
        'Max overlap' = '#7ADFE1'
      )
  ) +
  
  scale_color_manual( 
    name = '', 
    values = c(
      'Avg IoI' = '#521B00',
      'Max IoI' = '#fd5901'
     )
  ) +
  
  scale_y_continuous(
    limits = c(lower_boundary, upper_boundary)
  ) +
  
  labs(
    title = 'Inter Operation Interval, minutes', 
    x = '', 
    y = 'Minutes',
    fill = 'Time'
  ) +
  
  theme_minimal() +

  theme(
    axis.text.x = element_text(hjust = 0.5, vjust = 0.5),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = 'bottom',
    legend.direction = 'horizontal',
    text = element_text(size = 12, family = 'sans'),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  )




# Clean Up ---------------------------------------------------------------------

# remove(
#   prepared_df,
#   inter_op_interval_df
# )




# Display the Plot -------------------------------------------------------------

inter_op_interval_p


