# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2024-01-31
# Updated on:   2024-01-31
#
# Description:  Transforms time of character type into hms type.
#
# Location:     src/services/dataServices/characterToHMSService.R
#


# Service ----------------------------------------------------------------------

character_to_hms <- function(character_hms){

  library(lubridate)
  library(hms)

  character_hms <- case_when(
    is.na(character_hms) ~ '00:00:00',
    TRUE ~ character_hms
  )
  
  time <- lubridate::hms(character_hms) |> 
    round()
  
  hms::hms(time)
}




