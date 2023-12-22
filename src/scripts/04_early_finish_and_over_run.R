# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2023-12-13
# Updated on:   2023-12-22
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
  group_by(week) |> 
  reframe( total_over_run = sum(over_run, na.rm = TRUE) )


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
      units = 'mins')
    ) |> 
  
  group_by(week) |>
  #  TODO: Fix too much early finish time! 
  reframe( total_early_finish = floor(sum(early_finish) / 3)  )

early_finish_over_run_df <- inner_join(
    early_finish_df, 
    over_runs_df, 
    by = c('week')) |> 
  
  mutate( trend = (total_early_finish * 1.5 + total_over_run) / 2 )
  # filter(theatre == 'C')

summary_early_late_finish_df <- early_finish_over_run_df |> 
  summarise(
    max_ealy_finish = -min(total_early_finish),
    max_over_run = max(total_over_run),
    limit = max(max_ealy_finish, max_over_run)
  ) 




# Render Early Finish and Over Run Chart ---------------------------------------


early_finish_over_run_p <- ggplot( 
      early_finish_over_run_df, 
      aes(x = factor(week)) 
    ) +
  
  geom_hline(yintercept = 0, color = 'black') +
  
  geom_bar(
    aes(y = total_early_finish, fill = 'Mins lost due early finish'), 
    stat = 'identity', 
    alpha = 0.7,
    position = 'identity',
    width = 0.5
  ) +
  
  geom_bar(
    aes(y = total_over_run, fill = 'Mins of over run'), 
    stat = 'identity', 
    alpha = 0.7,
    position = 'identity',
    width = 0.5
  ) +

  geom_text(
    aes(
      y = total_over_run, 
      label = total_over_run
    ),
    vjust = -1,
    hjust = 0.5,
    fontface = 'bold',
    size = 3
  ) +
  
  geom_text(
    aes(
      y = total_early_finish, 
      label = total_early_finish
    ),
    vjust = 1.75,
    hjust = 0.5,
    fontface = 'bold',
    size = 3
  ) +
  
  scale_y_continuous(
    limits = c(
      -summary_early_late_finish_df$limit - 100, 
      summary_early_late_finish_df$limit + 100
    )
  ) +
  
  scale_fill_manual( 
    name = '', 
    values = c(
      'Mins lost due early finish' = '#fd5901',
      'Mins of over run' = '#995D81'
    )
  ) +
  
  scale_x_discrete(
    labels = paste0('Wk ', early_finish_over_run_df$week)
  ) +
  
  labs(
    title = 'Early Finish (-)/ Over Run (+) (PM)', 
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
    text = element_text(size = 12, family = 'sans'),
    axis.text.y = element_blank()  
  )




# Clean Up ---------------------------------------------------------------------

remove(
  prepared_df,
  early_finish_df,
  early_finish_over_run_df,
  summary_early_late_finish_df,
  over_runs_df
)



# Display the Plot -------------------------------------------------------------


early_finish_over_run_p


