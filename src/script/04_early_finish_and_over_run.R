# Meta Data --------------------------------------------------------------------
#
# Version:      1.1
# Author:       Oleksii Dovhaniuk
# Created on:   2023-12-13
# Updated on:   2024-01-04
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
# Location:     script/04_early_finish_and_over_run.R
#



# Setup the Script -------------------------------------------------------------

source('script/01_read_and_prep.R')





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
    !is.na(anaesthetic_finish)
  ) |> 
  group_by(surgery_start_date, theatre) |> 
  
  reframe(
    week = week[ which.max(anaesthetic_finish) ],
    theatre_close = theatre_close[ which.max(anaesthetic_finish) ],
    duration = duration[ which.max(anaesthetic_finish) ],
    early_finish = early_finish[ which.max(anaesthetic_finish) ]
  ) |> 
  
  group_by(week) |>
  #  TODO: Fix too much early finish time! 
  reframe( total_early_finish = sum(early_finish) )

early_finish_over_run_df <- inner_join(
    early_finish_df, 
    over_runs_df, 
    by = c('week')) |> 
  
  mutate( 
    time_balance = (total_early_finish + total_over_run) / 2,
    balance_sign = ifelse( 
      sign(time_balance) == 0, 
      1, 
      -sign(time_balance)
    )
  )
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
  
  geom_bar(
    aes(y = total_early_finish, fill = 'Mins lost due early finish'), 
    stat = 'identity', 
    alpha = 0.7,
    position = 'identity',
    width = 0.8
  ) +
  
  geom_bar(
    aes(y = total_over_run, fill = 'Mins of over run'), 
    stat = 'identity', 
    alpha = 0.7,
    position = 'identity',
    width = 0.8
  ) +
  
  geom_line(
    aes(
      y = time_balance, 
      color = 'Time balance'
    ), 
    group = 1
  ) +
  
  geom_point(
    aes(
      y = time_balance,
      color = 'Time balance'
    ), 
    shape = 19, 
    size = 3,
    alpha = 0.7
  ) +

  geom_text(
    aes(
      y = total_over_run, 
      label = total_over_run
    ),
    vjust = -0.75,
    hjust = 0.5,
    fontface = 'bold',
    size = 5,
    colour = '#B26D06'
  ) +
  
  geom_text(
    aes(
      y = total_early_finish, 
      label = total_early_finish
    ),
    vjust = 1.75,
    hjust = 0.5,
    fontface = 'bold',
    size = 5,
    colour = '#fd5901'
  ) +
  
  geom_text(
    aes(
      y = time_balance, 
      label = time_balance
    ),
    vjust = 1.5 * early_finish_over_run_df$balance_sign + 0.4,
    hjust = 0.5,
    fontface = 'bold',
    size = 5,
    colour = '#249ea0'
  ) +
  
  geom_hline(yintercept = 0, color = 'black') +
  
  scale_y_continuous(
    limits = c(
      -summary_early_late_finish_df$limit - 200, 
      summary_early_late_finish_df$limit + 200
    )
  ) +
  
  scale_fill_manual( 
    name = '', 
    values = c(
      'Mins lost due early finish' = '#fd5901',
      'Mins of over run' = '#faab36'
    )
  ) +
  
  scale_color_manual( 
    name = '', 
    values = c('Time balance' = '#249ea0')
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

# remove(
#   prepared_df,
#   early_finish_df,
#   early_finish_over_run_df,
#   summary_early_late_finish_df,
#   over_runs_df
# )



# Display the Plot -------------------------------------------------------------


early_finish_over_run_p


