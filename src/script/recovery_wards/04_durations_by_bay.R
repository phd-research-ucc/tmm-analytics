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
library(glue)
library(hrbrthemes)
library(viridis)




# Build Scatter Plots ----------------------------------------------------------

get_durations_df <- function(file) {
  df <- readRDS(file)
 
  plot_data <- df$data_entry |> 
    mutate(
      until_left_theatre = as.numeric(left_theatre - anaesthetic_finish) %/% 360 / 10,
      ward_waiting = as.numeric(ward_called - left_theatre) %/% 360 / 10,
      recovery_duration = as.numeric(out_of_recov - ward_called) %/% 360 / 10
    ) |> 
    select(
      bay,
      until_left_theatre,
      ward_waiting,
      recovery_duration
    ) |> 
    pivot_longer(
      cols = -bay,
      names_to = "type",
      values_to = "duration"
    ) |> 
    group_by(bay, type) |> 
    summarise(total = sum(duration)) |> 
    mutate(
      type = factor(
        type, 
        levels=c(
          "recovery_duration",
          "ward_waiting", 
          "until_left_theatre"
        )
      )
    )
  
  return(plot_data)
}

# saveRDS(
#   get_durations_df("~/src/data/clean/2024-05-21/Annonmysed_37-40_v2_8_anon.rds"),
#   file = "~/src/data/final/2024-05-21/recovery/durations_37-40_v2_8.rds"
# )

get_duration_pl <- function(file) {
  
  df <- readRDS(file);
  
  ggplot(
      df, 
      aes(x = bay, y = total, fill = type)
    ) + 
    geom_bar(stat = "identity") +
    # geom_text(
    #   aes(
    #     y = df$recovery_duration,
    #     label = df$recovery_duration
    #   ),
    #   vjust = -1.5,
    #   hjust = 0.5,
    #   fontface = 'bold',
    #   size = 4
    # ) +

    theme(legend.position = "bottom") +
    theme_minimal() +
    scale_fill_viridis(discrete = TRUE, name = "") 
  
}



# get_durations_pl <- function(data_entry_df) {
#   
#   plot_data <- data_entry_df |> 
#     select(
#       bay,
#       induction_to_incision_time,
#       knife_to_skin_time,
#       anaesthetic_recovery_time,
#       until_left_theatre_time,
#       ward_waiting_time,
#       recovery_time
#     ) |> 
#     pivot_longer(
#       cols = -bay,
#       names_to = "duration",
#       values_to = "minutes"
#     )
#   
#   # View(plot_data)
#   
#   surgery_data <- plot_data |> 
#     select(
#       -until_left_theatre_time,
#       -ward_waiting_time,
#       -recovery_time
#     ) |> 
#     pivot_longer(
#       cols = -bay,
#       names_to = "duration",
#       values_to = "minutes"
#     ) |> 
#     group_by(bay, duration) |> 
#     summarise(minutes = sum(minutes)) |> 
#     mutate(char_length = nchar(as.character(bay))) |> 
#     arrange(char_length, bay) |> 
#     select(-char_length)
#   
#   surgery_plot <- ggplot(
#     surgery_data, 
#     aes(x = bay, y = minutes, fill = duration)) + 
#     geom_bar(stat = "identity") +
#     coord_flip() +
#     scale_y_reverse() +
#     labs(x = NULL, y = NULL) +
#     theme(
#       axis.title.y = element_blank(),
#       axis.text.y = element_blank(),
#       axis.ticks = element_blank(),
#       legend.position = "bottom"
#     )
#   
#   bay_plot <- plot_data |> 
#     select(
#       -induction_to_incision_time,
#       -knife_to_skin_time,
#       -anaesthetic_recovery_time
#     ) |> 
#     pivot_longer(
#       cols = -bay,
#       names_to = "duration",
#       values_to = "minutes"
#     ) |> 
#     group_by(bay, duration) |> 
#     summarise(minutes = sum(minutes)) |> 
#     mutate(char_length = nchar(as.character(bay))) |> 
#     arrange(char_length, bay) |> 
#     select(-char_length) 
#   
#   bay_plot <- ggplot(
#     bay_plot, 
#     aes(x = bay, y = minutes, fill = duration)) + 
#     geom_bar(stat = "identity") +
#     coord_flip() +
#     labs(x = NULL, fill = NULL) +
#     theme(
#       axis.title.x = element_blank(),
#       axis.ticks = element_blank(),
#       legend.position = "bottom"
#     )
#   
#   plot <- ggarrange(
#     surgery_plot,
#     bay_plot,
#     ncol = 2,
#     legend = "bottom"
#   )
#   
#   print(plot)
#   
# }
# 
# 
# 
# 
