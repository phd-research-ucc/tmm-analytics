# Meta Data ---------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2023-12-13
# Updated on:   2023-12-13
#
# Description:  The file contains custom functions for this project
#
# Location:     scripts/_dev.R
#


to_unix_timestamp <- function(
    string_time, 
    tz = 'UTC',
    format = '%H:%M',
    origin = '1970-01-01',
    optional = TRUE) {
  
  as.POSIXct(
    gsub(' ', '', string_time), 
    tz = tz, 
    format = format,
    optional = optional)
}