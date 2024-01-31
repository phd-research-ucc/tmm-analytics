# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2024-01-31
# Updated on:   2024-01-31
#
# Description:  Exstracts and clears theatre names data frame 
#               from the mng_theatre_df.
#
# Location:     src/services/dataServices/getTheatreNamesService.R
#


# Setup ------------------------------------------------------------------------

library(tidyverse)
library(janitor)


# Service ----------------------------------------------------------------------


get_theatre_names <- function(mng_theatres_df){
  
  mng_theatres_df <- janitor::clean_names(mng_theatres_df)
  
  theatre_names_df <- mng_theatres_df |> 
    select(
      theatre_1,
      th_name,
    )  |>
    
    filter( !is.na(theatre_1) ) |> 
    
    rename(
      theatre_id = theatre_1,
      theatre_name = th_name
    ) |> 
    
    mutate(
      theatre_id = as.factor(theatre_id),
      theatre_name = as.factor(theatre_name)
    )

  return(theatre_names_df)
}




