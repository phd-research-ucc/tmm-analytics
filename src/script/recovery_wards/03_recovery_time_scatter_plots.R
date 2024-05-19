# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2024-05-11
# Updated on:   2024-05-11
#
# Description:  Analyse the recovery time in relation to other surgery
#               case parameters.
#
# Location:     ~/src/script/recovery_wards/03_recovery_time_scatter_plots.R
#



# Setup the Script -------------------------------------------------------------

library(ggplot2)




# Build Scatter Plots ----------------------------------------------------------

get_recovery_time_pl <- function(data_entry_df, grouped_by) {
  plot_data <- data_entry_df |> 
    select(
      grouped_by,
      recovery_time
    ) |> 
    group_by(.data[[grouped_by]]) |> 
    mutate(index = n())
    
    View(plot_data)
    
    plot <- ggplot(
      plot_data,
      aes(
        x = index, 
        y = recovery_time, 
        color = .data[[grouped_by]]
      )
    ) +
      labs( 
        x = "Case Index", 
        y = "Recovery Time", 
        color = grouped_by
      ) +
      # geom_smooth(method=lm) +
      geom_point()
    
    print(plot)
    
}



get_recovery_time_with_dependency_pl <- function(
    data_entry_df, 
    relative_metric, 
    grouped_by
  ){
  
  plot_data <- data_entry_df |> 
    select(
      grouped_by,
      relative_metric,
      recovery_time
    ) 
  
  
  View(plot_data)
  
  plot <- ggplot(
    plot_data,
    aes(
      x = .data[[relative_metric]], 
      y = recovery_time, 
      color = .data[[grouped_by]]
    )
  ) +
    labs( 
      x = relative_metric, 
      y = "Recovery Time", 
      color = grouped_by
    ) +
    # geom_smooth(method=lm) +
    geom_point()
  
  print(plot)
}





