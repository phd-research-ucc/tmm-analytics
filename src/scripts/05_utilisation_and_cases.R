# Meta Data --------------------------------------------------------------------
#
# Version:      1.1
# Author:       Oleksii Dovhaniuk
# Created on:   2023-12-13
# Updated on:   2024-01-04
#
# Description:  The script uses prepared data 
#               from 01_read_and_prep.R script to
#               render Utilisation and Cases Chart,
#               which shows utilisation rates, number 
#               of In-Core Cases cases, umber of Out-of-Core Cases
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

# one_day <- prepared_df |>
#   filter(surgery_start_date == '2020-03-02') |>
#   select(
#     -surgery_status,
#     -surgery_end_date,
#     -week,
#     -core_time_m,
#     -core_time_h
#   )
# 
# one_day_summary <- one_day |> 
#   group_by(surgery_start_date) |>
#   summarise(
#     total_used_time = sum(duration),
#     total_incore_time = sum(incore_time),
#     total_available_time = 6 * 480,
#     incore_utilisation = total_incore_time / total_available_time,
#     total_utilisation = total_used_time / total_available_time,
#     total_incore_cases = sum(is_incore_case),
#     total_outcore_cases = sum(is_outcore_case),
#     total_cancelled_cases = sum(is_cancelled_case),
#     total_has_noncore_time_case = sum(has_noncore_time_case),
#     total_noncore_cases = sum(is_noncore_case)
#   )

utilisation_and_cases_df <- prepared_df |> 
  group_by(surgery_start_date, theatre) |> 
  
  reframe(
    incore_cases = sum(is_incore_case, na.rm = TRUE),
    outcore_cases = sum(is_outcore_case, na.rm = TRUE),
    noncore_cases = sum(is_noncore_case, na.rm = TRUE),
    cancelled_cases = sum(is_cancelled_case, na.rm = TRUE),
    total_cases = incore_cases + outcore_cases + cancelled_cases,
    
    total_core_time = as.numeric( core_time_m[ which.max(core_time_m) ] ),
    total_incore_time = as.numeric( sum(incore_time, na.rm = TRUE) ),
    total_outcore_time = as.numeric( sum(outcore_time, na.rm = TRUE) )
  ) |> 
  
  group_by(surgery_start_date) |> 
  reframe(
    outcore_cases = sum(outcore_cases, na.rm = TRUE),
    'Out-of-Core Cases' = outcore_cases,
    noncore_cases = sum(noncore_cases, na.rm = TRUE),
    'Non-Core Cases' = noncore_cases,
    cancelled_cases = -sum(cancelled_cases, na.rm = TRUE),
    'Cancelled Cases' = cancelled_cases,
    incore_cases = sum(incore_cases, na.rm = TRUE),
    'In-Core Cases' = incore_cases - noncore_cases,
    total_cases = incore_cases + outcore_cases,
    
    total_core_time = sum(total_core_time, na.rm = TRUE),
    total_incore_time = sum(total_incore_time, na.rm = TRUE),
    total_outcore_time = sum(total_outcore_time, na.rm = TRUE),
    
    incore_time_usage = total_incore_time / total_core_time,
    outcore_time_usage = total_outcore_time / total_core_time,
    total_time_usage = incore_time_usage + outcore_time_usage,
    
  ) 

utilisation_and_cases_plotting_df <- utilisation_and_cases_df |> 
  
  tidyr::gather(
    key = 'case_type', 
    value = 'num_cases',
    'Non-Core Cases',
    'In-Core Cases',
    'Out-of-Core Cases',
    'Cancelled Cases'
  ) |> 
  
  mutate( 
    weekday_abbr = format(surgery_start_date, '%a'),
    month_abbr = format(surgery_start_date, '%b'),
    day_date = format(surgery_start_date, '%d'),
    x_date_label = glue('{weekday_abbr}\n{day_date}\n{month_abbr}'),
    
    # surgery_start_date = factor(
    #   surgery_start_date,
    #   levels = unique(surgery_start_date)
    # ),
    case_type = factor(
      case_type,
      levels = c(
        'Out-of-Core Cases',
        'In-Core Cases',
        'Non-Core Cases',
        'Cancelled Cases'
      )
    )
  ) 



# Render Utilisation and Cases Chart ---------------------------------------


utilisation_and_cases_p <- ggplot( 
      utilisation_and_cases_plotting_df, 
      aes(x = reorder(x_date_label, surgery_start_date))
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
  
  annotate(
    geom = "text",
    x = 21, 
    y = 1 * 30 + 20,
    label = "100%",
    hjust = 1,
    vjust = -0.5,
    fontface = 'bold',
    size = 4
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
      color = '% In-Core Cases Utilisation'),
    group = 1
  ) +
  
  geom_point(
    aes(
      y = incore_time_usage * 30 + 20, 
      color = '% In-Core Cases Utilisation'),
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
      x = as.integer( reorder(x_date_label, surgery_start_date) ),
      ymin = incore_time_usage * 30 + 20,
      ymax = total_time_usage * 30 + 20,
      fill = 'Out-of-Core Cases'
    ),
    alpha = .2
  ) +
  
  geom_text(
    aes(
      y = total_cases,
      label = ifelse(outcore_cases > 0, outcore_cases, '')
    ),
    colour = '#faab36',
    vjust = -0.5,
    hjust = 0.5,
    fontface = 'bold',
    size = 4
  ) +
  
  geom_text(
    aes(
      y = incore_cases,
      label = incore_cases - noncore_cases
    ),
    colour = '#264854',
    vjust = 1.8,
    hjust = 0.5,
    fontface = 'bold',
    size = 4
  ) +
  
  geom_text(
    aes(
      y = noncore_cases,
      label = ifelse(noncore_cases > 0, noncore_cases, '')
    ),
    colour = '#1A7B85',
    vjust = 1.8,
    hjust = 0.5,
    fontface = 'bold',
    size = 4
  ) +
  
  geom_text(
    aes(
      y = cancelled_cases,
      label = ifelse(cancelled_cases < 0, cancelled_cases, '')
    ),
    colour = '#fd5901',
    vjust = 1.5,
    hjust = 0.5,
    fontface = 'bold',
    size = 4
  ) +

  scale_fill_manual( 
    name = '', 
    values = c(
      'In-Core Cases' = '#249ea0',
      'Out-of-Core Cases' = '#faab36',
      'Non-Core Cases' = '#94EBCF',
      'Cancelled Cases' = '#fd5901'
    )
  ) +
  
  scale_color_manual( 
    name = '', 
    values = c(
      '% In-Core Cases Utilisation' = '#249ea0',
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
    
    name = '',
    breaks = seq(-5, 60, 5),
    # minor_breaks = seq(
    #   min(utilisation_and_cases_df$cancelled_cases, na.rm = TRUE), 
    #   max(utilisation_and_cases_df$total_cases, na.rm = TRUE), 
    #   1
    # ),
    limits = c(-5, 60)
  ) +
  
  
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(hjust = 0.5, vjust = 0.5),
    legend.position = 'bottom',
    # panel.grid.minor = element_line(
    #   color = "lightgray", 
    #   size = 0.2, 
    #   linetype = "dashed"
    # ),
    text = element_text(size = 12, family = 'sans'),
    axis.text.y = element_blank()
  )




# Clean Up ---------------------------------------------------------------------

# remove(
#   prepared_df,
#   # utilisation_and_cases_df,
#   utilisation_and_cases_plotting_df
# )



# Display the Plot -------------------------------------------------------------

utilisation_and_cases_p



