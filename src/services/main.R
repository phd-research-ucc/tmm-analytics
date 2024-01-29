# Meta Data --------------------------------------------------------------------
#
# Version:      1.1
# Author:       Oleksii Dovhaniuk
# Created on:   2024-01-11
# Updated on:   2024-01-18
#
# Description:  Combines all services of the project
#               into one API collection.
#
# Location:     services/main.R
#




# Setup Plumber --------------------------------------------------------


options('plumber.port' = 8080)




# Connecting to Plumber --------------------------------------------------------

#* Health Check - Is the API running
#* @get /health-check
statue <- function(){
  list(
    status = 'All Good',
    time = Sys.time()
  )
} 


#* Reads demo Data Entry sheet from file end returns data frame
#* @param type 'csv', 'xls', or 'xlsx' (by default - 'xlsx')
#* @get /file/read-demo-data-entry
function(type='xlsx') {
  source('fileServices/readDemoDataEntryService.R')
  readDemoDataEntryService(type)
}


#* Reads demo Mng Theatre sheet from file end returns data frame
#* @param type 'csv', 'xls', or 'xlsx' (by default - 'xlsx')
#* @get /file/read-demo-mng-theatre
function(type='xlsx') {
  source('fileServices/readDemoManageTheatresService.R')
  readDemoManageTheatresService(type)
}


#* Prepares Data Entry and Mng Theatre data for further analysis
#* @param data_entry_file path to the data entry file
#* @param mng_theatres_file path to the manage theatres file
#* @get /prepare/prep-data
function(data_entry_file = NA, mng_theatres_file = NA) {
  library(jsonlite)
  
  if ( is.na(mng_theatres_file) ){
    source('fileServices/readDemoManageTheatresService.R')
    mng_theatres_json <- readDemoManageTheatresService()
  }

  if ( is.na(data_entry_file) ){
    source('fileServices/readDemoDataEntryService.R')
    data_entry_json <- readDemoDataEntryService()
  }

  source('prepareServices/prepareDataService.R')
  prepareDataService(
    fromJSON(data_entry_json),
    fromJSON(mng_theatres_json)
  )
}


#* @get /apiA
function(){
    response <- httr::GET("http://localhost:8080/apiB")
    content <- httr::content(response, "text")
    return(list(result = content, message = "API A Response"))
}


#* @get /apiB
function(){
    return(list(message = "API B Response"))
}


# Cleanup ----------------------------------------------------------------------

# rm(list = ls())