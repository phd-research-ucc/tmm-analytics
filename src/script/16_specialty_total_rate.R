# Meta Data --------------------------------------------------------------------
#
# Version:      1.1
# Author:       Oleksii Dovhaniuk
# Created on:   2024-04-17
# Updated on:   2024-04-17
#
# Description:  Script which returns functions get_specialty_total_rate_pl()
#               and get_specialty_total_rate_df() with plot for total spent 
#               time in minutes in regards to different specialties
#               for the inputted data set.
#
# Location:     script/16_specialty_total_rate.R
#



# Setup the Script -------------------------------------------------------------

source('script/01_read_and_prep.R')
library(ggrepel)

# Total Spent Time Rate of Specialties -----------------------------------------

get_specialty_total_rate_df <- function(){
  
  df <- prepared_df |> 
    select(
      specialty,
      duration,
      incore_time,
      early_start,
      over_run
    ) |> 
    
    filter(
      !is.na(duration)
    ) |> 
    
    group_by(
      specialty
    ) |> 
    
    summarize(
      total = as.numeric( sum(duration) )
    )
  
  grand_total <- sum(df$total)
  
  df <- mutate(
    df, 
    rate = round(total / grand_total * 100, digits = 2)
  )
  
  return (df)
}


get_specialty_total_rate_pl <- function(){
  
  df <- get_specialty_total_rate_df()
  
  # Get the positions
  df2 <- df |> 
    mutate(csum = rev(cumsum(rev(rate))), 
           pos = rate/2 + lead(csum, 1),
           pos = if_else(is.na(pos), rate/2, pos))
  
  pl <- ggplot(df, aes(x = "" , y = rate, fill = fct_inorder(specialty))) +
    geom_col(width = 1, color = "black") +
    coord_polar(theta = "y") +
    # scale_fill_brewer(palette = "Pastel1") +
    geom_label_repel(data = df2,
                     aes(y = pos, label = paste0(rate, "%")),
                     size = 4.5, nudge_x = 1, show.legend = FALSE) +
    guides(fill = guide_legend(title = "Specialty")) +
    theme_void()
  
  return(pl)
}






