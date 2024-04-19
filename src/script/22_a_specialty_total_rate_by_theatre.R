# Meta Data --------------------------------------------------------------------
#
# Version:      1.1
# Author:       Oleksii Dovhaniuk
# Created on:   2024-04-18
# Updated on:   2024-04-18
#
# Description:  Script which returns functions get_a_specialty_total_rate_by_theatre_pl()
#               and get_a_specialty_total_rate_by_theatre_df() with plot for minutes spent 
#               of cases in regards to a single specialty by theatre
#               for the inputted data set.
#
# Location:     script/22_a_specialty_total_rate_by_theatre.R
#



# Setup the Script -------------------------------------------------------------

source('script/01_read_and_prep.R')




# Minutes of Cases for a Specialty ---------------------------------------------

get_a_specialty_total_rate_by_theatre_df <- function(target_specialty) {
  
  df <- prepared_df |> 
    select(
      specialty,
      duration,
      incore_time,
      early_start,
      over_run,
      theatre
    ) |> 
    
    filter(
      !is.na(duration),
      specialty == target_specialty
    ) |> 
    
    group_by(
      specialty,
      theatre
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
  
 
get_a_specialty_total_rate_by_theatre_pl <- function(target_specialty) {
  
  df <- get_a_specialty_total_rate_by_theatre_df(target_specialty)
  
  # Get the positions
  df2 <- df |> 
    mutate(csum = rev(cumsum(rev(rate))), 
           pos = rate/2 + lead(csum, 1),
           pos = if_else(is.na(pos), rate/2, pos))
  
  pl <- ggplot(df, aes(x = "" , y = rate, fill = fct_inorder(theatre))) +
    geom_col(width = 1, color = "black") +
    coord_polar(theta = "y") +
    # scale_fill_brewer(palette = "Pastel1") +
    geom_label_repel(data = df2,
                     aes(y = pos, label = paste0(rate, "%")),
                     size = 4.5, nudge_x = 1, show.legend = FALSE) +
    guides(fill = guide_legend(title = "theatre")) +
    theme_void()
  
  return(pl)
}




