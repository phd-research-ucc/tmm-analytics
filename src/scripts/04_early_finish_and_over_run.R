# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2023-12-13
# Updated on:   2023-12-13
#
# Description:  The script uses prepared data 
#               from 01_read_and_prep.R script to
#               render Early Finish/ Over Run Chart,
#               which shows minutes lost due to early 
#               workday finishes and out of core time
#               of surgery over runs.
#
#               Time span - 4 weeks.
#               X axis stands for weeks
#
# Location:     scripts/04_early_finish_and_over_run.R
#



# Setup the Script -------------------------------------------------------------

source('scripts/01_read_and_prep.R')





# Mine Needed Data -------------------------------------------------------------


over_runs_df <- prepared_df |> 
  filter(
    !is.na(anaesthetic_start),
    !is.na(anaesthetic_finish)
  ) |> 
  group_by(week, theatre) |> 
  reframe( total_over_run = sum(over_run, na.rm = TRUE) )

# View(over_runs_df)


early_finish_df <- prepared_df |> 
  filter(
    !is.na(anaesthetic_start),
    !is.na(anaesthetic_finish),
    anaesthetic_finish <= theatre_close
  ) |> 
  group_by(surgery_start_date, theatre) |> 
  
  reframe(
    week = week[ which.max(anaesthetic_finish) ],
    theatre_close = theatre_close[ which.max(anaesthetic_finish) ],
    duration = duration[ which.max(anaesthetic_finish) ],
    last_core_case_finish = max( anaesthetic_finish, na.rm = TRUE ),
    
    early_finish = difftime(
      last_core_case_finish,
      theatre_close, 
      units = 'mins')) |> 
  
  # Grouping by weeks and theatres:
  group_by(week, theatre) |>
  reframe( total_early_finish = sum(early_finish) )


# View(early_finish_df)


early_finish_over_run_df <- inner_join(
  early_finish_df, 
  over_runs_df, 
  by = c('week', 'theatre')) |> 
  mutate( trend = (total_early_finish * 1.5 + total_over_run) / 2 ) |> 
  filter(theatre == 'F')

# View(early_finish_over_run_df)




# Render Early Finish and Over Run Chart ---------------------------------------


early_finish_over_run_p <- ggplot( 
  early_finish_over_run_df, 
  aes(x = factor(week)) ) +
  
  geom_bar(
    aes(y = total_early_finish, fill = 'Early finish'), 
    stat = 'identity', 
    alpha = 0.7,
    position = 'identity') +
  
  geom_bar(
    aes(y = total_over_run, fill = 'Over run'), 
    stat = 'identity', 
    alpha = 0.7,
    position = 'identity') +
  
  # geom_line(
  #   aes(y = trend, color = 'Trend'),
  #   group = 1,
  #   # linejoin = 'round',
  #   linetype = 'dashed') +
  
  geom_point(
    aes(y = trend, color = 'Trend'),
    shape = 4,
    size = 5,
    alpha = 0.7) +
  
  geom_text(
    aes(
      y = max(total_over_run), 
      label = total_over_run),
    position = position_stack(vjust = 2.3),
    fontface = 'bold',
    size = 3) +
  
  geom_text(
    aes(
      y = max(total_over_run), 
      label = total_early_finish),
    position = position_stack(vjust = 2),
    fontface = 'bold',
    size = 3) +
  
  scale_fill_manual( 
    name = '', 
    values = c('Early finish' = '#faab36',
               'Over run' = '#249ea0') ) +
  
  scale_color_manual( 
    name = '', 
    values = c('Trend' = '#fd5901') ) +
  
  labs(
    title = 'Early Finish (-)/ Over Run (+) (PM)', 
    x = 'Weeks', 
    y = 'Minutes',
    fill = 'Time' ) +
  
  theme_minimal() +
  theme(
    # axis.text.x = element_text(angle = -90, hjust = 0),
    panel.grid.major.x = element_blank(),  # Remove y-axis grid lines
    legend.position = 'bottom',
    legend.direction = 'horizontal',
    text = element_text(size = 12, family = 'sans') )

early_finish_over_run_p




# Clean Up ---------------------------------------------------------------------

remove(
  early_finish_over_run_df,
  over_runs_df
)

