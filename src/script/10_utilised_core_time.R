# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2023-12-22
# Updated on:   2023-12-22
#
# Description:  Render Chart for Core Time Utilisation Rate
#
# Location:     script/10_utilised_core_time.R
#



# Setup the Script -------------------------------------------------------------

source('script/07_summary_table.R')
source('script/05_utilisation_and_cases.R')




# Mine Needed Data -------------------------------------------------------------


data <- data.frame(
  category = c('Utilised Core Time', 'Unused Core Time'),
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
    label = paste0(category, ':\n', floor(time / 60), ' hr'),
    percent = paste0(
      floor( as.integer(fraction * 1000) ) / 10, 
      '%'
    )
  )




# Render Chart for Core Time Utilisation Rate ---------------------------------------


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
  
  geom_label(
    x = 3.5,
    aes(
      y = labelPosition,
      label = percent
    ),
    size = 3,
    fontface = 'bold',
    color = 'black',
    alpha = 0.8
  ) +
  
  scale_fill_manual(
    values = c(
      'Unused Core Time' = '#FD5901',
      'Utilised Core Time' = '#2DC5C8'
    )
  ) +
  
  labs(
    title = 'Core Time Utilisation', 
    x = '', 
    y = '',
    fill = ''
  ) +
  
  coord_polar(theta = 'y') +
  xlim( c(2, 4) ) +
  theme_void()




# Clean Up ---------------------------------------------------------------------

remove(
  adhoc_summary_table,
  clean_data_df,
  data,
  summary_df,
  theatre_adhocs_df,
  theatre_core_time_df,
  utilisation_and_cases_df,
  utilisation_and_cases_p
)



# Display the Plot -------------------------------------------------------------

utilised_core_time_p



