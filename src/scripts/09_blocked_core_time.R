# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2023-12-22
# Updated on:   2023-12-22
#
# Description:  Render Distribution Chart of Blocked Time
#
# Location:     scripts/09_blocked_core_time.R
#



# Setup the Script -------------------------------------------------------------

source('scripts/07_summary_table.R')




# Mine Needed Data -------------------------------------------------------------


data <- adhoc_summary_table |> 
  mutate(
    fraction = total_lost_time_m / sum(total_lost_time_m),
    percent = paste0( floor(fraction * 1000) / 10 , '%' ),
    ymax = cumsum(fraction),
    ymin = c( 0, head(ymax, n = -1) ),
    label_position = (ymax + ymin) / 2,
    label = paste0(reason, ':\n', total_lost_time_h, ' hr')
  ) |> 
  rename(
    time = total_lost_time_m,
    label_time = total_lost_time_h
  )




# Render Blocked Time Distribution Chart --------------------------------------


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
  
  geom_label(
    x = 3.5,
    aes(
      y = label_position,
      label = percent
    ),
    size = 3,
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
  
  labs(
    title = 'Blocked Core Time Distribution', 
    x = '', 
    y = '',
    fill = ''
  ) +
  
  coord_polar(theta = 'y') +
  xlim( c(2, 4) ) +
  theme_void()




# Clean Up ---------------------------------------------------------------------

remove(
  utilised_core_time_p,
  adhoc_summary_table,
  summary_df,
  data
)



# Display the Plot -------------------------------------------------------------

blocked_core_time_p



