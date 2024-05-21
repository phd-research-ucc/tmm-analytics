# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2024-05-19
# Updated on:   2024-05-20
#
# Description:  Analyses utilisation time and number of cases 
#               in relation to recovery bays.
#
# Location:     ~/src/script/recovery_wards/05_utilisation_by_bay.R
#



# Setup the Script -------------------------------------------------------------

library(ggplot2)




# Build Scatter Plots ----------------------------------------------------------

calc_difftime <- function(time_finish, time_start){
  
  minutes = as.numeric(
    make_difftime(
      time_finish - time_start,
      units = "minutes"
    )
  );
  
  
  case_when(
    minutes < 0 ~ 0,
    TRUE ~ round(minutes)
  )
}

get_cases_by_bay_pl <- function(data_entry) {
  
  current_bay_levels <- levels(data_entry$bay)
  new_bay_levels <- current_bay_levels[
    order( nchar(current_bay_levels) )
  ]
  # print(new_bay_levels)
  
  plot_data <- data_entry |> 
    group_by(bay) |> 
    summarise(cases = n()) |> 
    select(bay, cases) |> 
    mutate(
      bay = factor(bay, levels=new_bay_levels)
    )
  
  # View(plot_data)
  
  
  plot <- ggplot(
      plot_data, 
      aes(x = bay, y = cases)
    ) + 
    geom_bar(stat = "identity")
  
  print(plot)
  
  return(plot)
}


get_utilisation_by_bay_pl <- function(df) {
  
  current_bay_levels <- levels(df$data_entry$bay)
  new_bay_levels <- current_bay_levels[
    order( nchar(current_bay_levels) )
  ]
  
  plot_data <- df$data_entry |>
    mutate(
      bay_open = as_hms("09:00:00"),
      bay_closed = as_hms("21:00:00"),
      is_out_of_core_case = ifelse(
        out_of_recov <= bay_open | bay_closed <= ward_called,
        TRUE,
        FALSE
      ),
      earlystart_recovery_time = case_when(
        is_out_of_core_case ~ 0,
        TRUE ~ calc_difftime(bay_open, ward_called)
      ),
      overrun_recovery_time = case_when(
        is_out_of_core_case ~ 0,
        TRUE ~ calc_difftime(out_of_recov, bay_closed)
      ),
      noncore_recovery_time =  earlystart_recovery_time + overrun_recovery_time,
      incore_recovery_time = case_when(
        is_out_of_core_case ~ 0,
        TRUE ~ recovery_time - noncore_recovery_time
      )
    ) |> 
    group_by(surgery_start_date, bay) |> 
    summarise(
      incore = sum(incore_recovery_time),
      noncore = sum(noncore_recovery_time),
      total = sum(recovery_time)
    ) |> 
    mutate(
      core = calc_difftime(as_hms("21:00:00"), as_hms("09:00:00"))
    ) |> 
    group_by(bay) |> 
    summarise(
      incore_utilisation = sum(incore) / sum(core),
      noncore_utilisation = sum(noncore) / sum(core),
      total_utilisation = sum(total) / sum(core)
    ) |> 
    mutate(
      bay = factor(bay, levels=new_bay_levels)
    )
  
  View(plot_data)
  
  plot <- ggplot( 
      plot_data, 
      aes(x = bay)
    ) +
    
    geom_hline(
      yintercept = 1, 
      color = 'black',
      linetype = 'dashed'
    ) +
    
    # geom_ribbon(
    #   aes(
    #     x = bay,
    #     ymin = incore_utilisation + noncore_utilisation,
    #     ymax = total_utilisation,
    #   ),
    #   alpha = .5
    # ) +
    # 
    # geom_ribbon(
    #   aes(
    #     x = bay,
    #     ymin = incore_utilisation,
    #     ymax = incore_utilisation + noncore_utilisation
    #   ),
    #   alpha = .5
    # ) +
    
    geom_line(
      aes(
        y = total_utilisation, 
        color = '% Utilisation of Time by Non-Core Cases'
      ),
      linetype = 'dashed',
      group = 4
    ) +
    
    geom_line(
      aes(
        y = incore_utilisation + noncore_utilisation, 
        color = '% Utilisation with Out-of-Core Time'
      ),
      linetype = 'dashed',
      group = 3
    ) +
    
    geom_line(
      aes(
        y = incore_utilisation, 
        color = '% Utilisation In-Core Time'
      ),
      group = 2
    ) +
    
    theme(
      legend.position = "bottom"
    )
  
  print(plot)
  
  return(plot)
  
}


