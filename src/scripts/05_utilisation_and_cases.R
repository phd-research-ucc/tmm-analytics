# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2023-12-13
# Updated on:   2023-12-22
#
# Description:  The script uses prepared data 
#               from 01_read_and_prep.R script to
#               render Utilisation and Cases Chart,
#               which shows utilisation rates, number 
#               of in-core cases, umber of out-of-core
#               cases, and number cancelled cases.
#
#               Time span - 4 weeks.
#               X axis stands for days.
#
# Location:     scripts/05_utilisation_and_cases.R
#



# Setup the Script -------------------------------------------------------------

source('scripts/01_read_and_prep.R')




# Mine Needed Data -------------------------------------------------------------


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



# Render Utilisation and Cases Chart ---------------------------------------


utilisation_and_cases_p <- ggplot( 
      utilisation_and_cases_plotting_df, 
      aes(x = surgery_start_date)
    ) +
  
  geom_bar(
    aes(
      y = num_cases, 
      fill = case_type
    ),
    stat = 'identity',
    alpha = 0.7,
    position = 'stack'
  ) +

  geom_hline(
    yintercept = 0, 
    color = 'black'
  ) +
  
  geom_hline(
    yintercept = 1 * 30 + 20, 
    color = 'black',
    linetype = 'dashed'
  ) +
  
  
  geom_line(
    aes(
      y = total_time_usage * 30 + 20, 
      color = '% Total Utilisaiton'),
    linetype = 'dashed',
    group = 2
  ) +
  
  geom_point(
    aes(
      y = total_time_usage * 30 + 20, 
      color = '% Total Utilisaiton'),
    shape = 16,
    size = 1
  ) +
  
  geom_text(
    aes(
      y = total_time_usage * 30 + 20, 
      label = scales::percent(
        total_time_usage, 
        scale = 100,
        accuracy = 1,
      ),
      color = '% Total Utilisaiton'
    ),
    vjust = -1.2,
    hjust = 0.5,
    fontface = 'bold',
    size = 3
  ) +
  
  geom_line(
    aes(
      y = incore_time_usage * 30 + 20, 
      color = '% In-Core Utilisation'),
    group = 1
  ) +
  
  geom_point(
    aes(
      y = incore_time_usage * 30 + 20, 
      color = '% In-Core Utilisation'),
    shape = 19,
    size = 1
  ) +
  
  geom_text(
    aes(
      y = incore_time_usage * 30 + 20, 
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
      ymin = incore_time_usage * 30 + 20,
      ymax = total_time_usage * 30 + 20,
      fill = 'outcore_cases'
    ),
    alpha = .2
  ) +

  scale_fill_manual( 
    name = '', 
    values = c(
      'incore_cases' = '#249ea0',
      'outcore_cases' = '#faab36',
      'cancelled_cases' = '#fd5901'
    )
  ) +
  
  scale_color_manual( 
    name = '', 
    values = c(
      '% In-Core Utilisation' = '#249ea0',
      '% Total Utilisaiton' = '#faab36'
    )
  ) +
  
  labs(
    title = 'Time Utilisation and Number of Cases', 
    x = '', 
    y = '% Utilisation',
    fill = 'Num Cases'
  ) +
  
  scale_y_continuous(
    # sec.axis = sec_axis(
    #   ~./50, 
    #   name = 'Utilisation',
    #   breaks = seq(0, 2.0, by = 0.2),
    #   labels = scales::percent_format(scale = 100)),
    
    name = 'Number of Cases',
    breaks = seq(0, 60, by = 10),
    limits = c(0, 60)
  ) +
  
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.5),
    legend.position = 'bottom',

    text = element_text(size = 12, family = 'sans'),
    
  )




# Clean Up ---------------------------------------------------------------------

remove(
  prepared_df,
  # utilisation_and_cases_df,
  utilisation_and_cases_plotting_df
)



# Display the Plot -------------------------------------------------------------

utilisation_and_cases_p



