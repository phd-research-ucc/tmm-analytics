# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2024-05-19
# Updated on:   2024-05-19
#
# Description:  Analyse the durations such as induction-to-incision time,
#               anaesthetic time, anaesthetic recovery time, 
#               knife-to-skin time, ward wairing time, and recovery time 
#               in relation to recovery bays.
#
# Location:     ~/src/script/recovery_wards/04_durations_by_bay.R
#



# Setup the Script -------------------------------------------------------------

library(ggplot2)
library(ggpubr)




# Build Scatter Plots ----------------------------------------------------------

get_durations_pl <- function(data_entry_df) {
  
  plot_data <- data_entry_df |> 
    select(
      bay,
      induction_to_incision_time,
      knife_to_skin_time,
      anaesthetic_recovery_time,
      until_left_theatre_time,
      ward_waiting_time,
      recovery_time
    )
  
  # View(plot_data)
  
  surgery_data <- plot_data |> 
    select(
      -until_left_theatre_time,
      -ward_waiting_time,
      -recovery_time
    ) |> 
    pivot_longer(
      cols = -bay,
      names_to = "duration",
      values_to = "minutes"
    ) |> 
    group_by(bay, duration) |> 
    summarise(minutes = sum(minutes)) |> 
    mutate(char_length = nchar(as.character(bay))) |> 
    arrange(char_length, bay) |> 
    select(-char_length)
  
  surgery_plot <- ggplot(
    surgery_data, 
    aes(x = bay, y = minutes, fill = duration)) + 
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_y_reverse() +
    labs(x = NULL, y = NULL) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom"
    )
  
  bay_plot <- plot_data |> 
    select(
      -induction_to_incision_time,
      -knife_to_skin_time,
      -anaesthetic_recovery_time
    ) |> 
    pivot_longer(
      cols = -bay,
      names_to = "duration",
      values_to = "minutes"
    ) |> 
    group_by(bay, duration) |> 
    summarise(minutes = sum(minutes)) |> 
    mutate(char_length = nchar(as.character(bay))) |> 
    arrange(char_length, bay) |> 
    select(-char_length) 
  
  bay_plot <- ggplot(
    bay_plot, 
    aes(x = bay, y = minutes, fill = duration)) + 
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(x = NULL, fill = NULL) +
    theme(
      axis.title.x = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom"
    )
  
  plot <- ggarrange(
    surgery_plot,
    bay_plot,
    ncol = 2,
    legend = "bottom"
  )
  
  print(plot)
  
}




