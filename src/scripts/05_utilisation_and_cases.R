# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2023-12-13
# Updated on:   2023-12-13
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
  group_by(surgery_start_date) |> 
  
  reframe(
    incore_cases = sum(is_incore_case, na.rm = TRUE),
    outcore_cases = sum(is_outcore_case, na.rm = TRUE),
    cancelled_cases = sum(is_cancalled_case, na.rm = TRUE),
    total_cases = incore_cases + outcore_cases + cancelled_cases,
    
    total_core_time = as.numeric( core_time_m[ which.max(core_time_m) ] ),
    total_incore_time = as.numeric( sum(incore_time, na.rm = TRUE) ),
    total_outcore_time = as.numeric( sum(outcore_time, na.rm = TRUE) ),
    
    incore_time_usage = total_incore_time / total_core_time,
    outcore_time_usage = total_outcore_time / total_core_time,
    total_time_usage = incore_time_usage + outcore_time_usage
  ) |> 
  
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



# Render Utilisation and Cases Chart ---------------------------------------


utilisation_and_cases_p <- ggplot( 
  utilisation_and_cases_df, 
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
#   # fill = "blue", 
#   alpha = 0.3
# ) +


geom_line(
  aes(
    y = total_time_usage * 5, 
    color = 'Out-Core Utilisaiton'),
  # color = '#faab36',
  linetype = 'dashed',
  group = 2
) +
  
  geom_point(
    aes(
      y = total_time_usage * 5, 
      color = 'Out-Core Utilisaiton'),
    shape = 16,
    # color = '#faab36',
    size = 1
  ) +
  
  geom_line(
    aes(
      y = incore_time_usage * 5, 
      color = 'In-Core Utilisation'),
    group = 1
  ) +
  
  
  geom_point(
    aes(
      y = incore_time_usage * 5, 
      color = 'In-Core Utilisation'),
    shape = 19,
    size = 1
  ) +
  
  geom_ribbon(
    aes(x = surgery_start_date,
        ymin = incore_time_usage * 5, 
        ymax = total_time_usage * 5, 
        fill = 'outcore_cases'), 
    alpha = 0.3
  ) +
  
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
               'Out-Core Utilisaiton' = '#faab36') ) +
  
  labs(
    title = 'Time Utilisation and Number of Cases', 
    x = 'Days', 
    y = '% Utilisation',
    fill = 'Num Cases' ) +
  
  scale_y_continuous(
    sec.axis = sec_axis(
      ~./5, 
      name = '% Utilisation',
      breaks = seq(0, 6, by = 0.4),
      labels = scales::percent_format(scale = 100)),
    
    name = 'Number of Cases',
    breaks = seq(0, 30, by = 2),
    limits = c(0, 30)
  ) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = -90, hjust = 0),
    panel.grid.major.x = element_blank(),  # Remove x-axis grid lines
    # panel.grid.minor.y = element_blank(),
    legend.position = 'bottom',
    # legend.direction = 'horizontal',
    text = element_text(size = 12, family = 'sans') )

print(utilisation_and_cases_p)




# Clean Up ---------------------------------------------------------------------

remove(utilisation_and_cases_df)
