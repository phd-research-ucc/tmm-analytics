# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2023-12-22
# Updated on:   2023-12-22
#
# Description:  Render Available Core Time Donut Chart
#
# Location:     scripts/08_available_core_time.R
#



# Setup the Script -------------------------------------------------------------

source('scripts/07_summary_table.R')



# Mine Needed Data -------------------------------------------------------------


data <- data.frame(
    category = c('Potential Time', 'Blocked'),
    time = c(
      as.integer(summary_df$c2[8]),
      as.integer(summary_df$c2[7]) - as.integer(summary_df$c2[8])
    )
  ) |> 
  
  mutate(
    fraction = time / sum(time),
    ymax = cumsum(fraction),
    ymin = c( 0, head(ymax, n = -1) ),
    label_position = (ymax + ymin) / 2,
    label = paste0(category, ':\n', time, ' hrs'),
    percent = paste0(
      floor(
        as.integer(summary_df$c1[8]) / as.integer(summary_df$c1[7]) * 100
      ), 
      '%'
    )
  )




# Render Available Core Time Donut Chart ---------------------------------------


available_core_time_p <- ggplot(
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
      y = label_position,
      label = label
    ), 
    size = 3
  ) +
  
  geom_text(
    x = 2,
    aes(
      y = c(0),
      label = percent
    ),
    size = 12,
    fontface = 'bold',
    color = '#0B4F6C'
  ) +
  
  scale_fill_manual(
    values = c(
      'Potential Time' = '#2DC5C8',
      'Blocked' = '#faab36'
    )
  ) +
  
  labs(
    title = 'Core Time Availability', 
    x = '', 
    y = '',
    fill = 'Rate'
  ) +
  
  coord_polar(theta = 'y') +
  xlim( c(2, 4) ) +
  theme_void() +
  theme(legend.position = 'none')



# Clean Up ---------------------------------------------------------------------

remove(
  data,
  summary_df
)



# Display the Plot -------------------------------------------------------------

available_core_time_p



