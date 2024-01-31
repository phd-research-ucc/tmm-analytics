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

library(jsonlite)

options('plumber.port' = 8080)
DEMO_DATA_ENTRY_PATH <- 'fileServices/readDemoDataEntryService.R'
DEMO_MANAGE_THEATRE_PATH <- 'fileServices/readDemoManageTheatresService.R'




# Connecting to Plumber --------------------------------------------------------


#* Health Check - Is the API running
#* @get /health-check
statue <- function(){
  list(
    status = 'All Good',
    time = Sys.time()
  )
} 




# File Services ----------------------------------------------------------------


#* Reads demo Data Entry sheet from file end returns data frame
#* @param type 'csv', 'xls', or 'xlsx' (by default - 'xlsx')
#* @get /file/read-demo-data-entry
function(type='xlsx') {
  source(DEMO_DATA_ENTRY_PATH)
  readDemoDataEntryService(type)
}


#* Reads demo Mng Theatre sheet from file end returns data frame
#* @param type 'csv', 'xls', or 'xlsx' (by default - 'xlsx')
#* @get /file/read-demo-mng-theatre
function(type='xlsx') {
  source(DEMO_MANAGE_THEATRE_PATH)
  readDemoManageTheatresService(type)
}




# Data Services ----------------------------------------------------------------


#* Exstracts and clears theatre names data frame from the mng_theatre_df
#* @param file path to the excel file
#* @get /data/get-theatre-names
function(file = NA) {

  if ( is.na(file) ) {
    source(DEMO_MANAGE_THEATRE_PATH)
    mng_theatres_df <- readDemoManageTheatresService()
  }
  
  source('dataServices/getTheatreNamesService.R')
  get_theatre_names(mng_theatres_df)
}


#* Exstracts and clears theatre timetables data frame from the mng_theatre_df
#* @param file path to the excel file
#* @get /data/get-theatre-timetables
function(file = NA) {

  if ( is.na(file) ) {
    source(DEMO_MANAGE_THEATRE_PATH)
    mng_theatres_df <- readDemoManageTheatresService()
  }

  source('dataServices/getTheatreTimetablesService.R')
  get_theatre_timetables(mng_theatres_df)
}


#* Exstracts and clears theatre ad-hocs data frame from the mng_theatre_df
#* @param file path to the excel file
#* @get /data/get-theatre-adhocs
function(file = NA) {

  if ( is.na(file) ) {
    source(DEMO_MANAGE_THEATRE_PATH)
    mng_theatres_df <- readDemoManageTheatresService()
  }

  source('dataServices/getTheatreAdhocsService.R')
  get_theatre_adhocs(mng_theatres_df)
}


#* Exstracts and clears the data entry data frame 
#* from the data_entry_df.
#* @param file path to the excel file
#* @get /data/get-data-entry
function(file = NA) {

  if ( is.na(file) ) {
    source(DEMO_DATA_ENTRY_PATH)
    data_entry_df <- readDemoDataEntryService()
  }

  source('dataServices/getDataEntryService.R')
  get_data_entry(data_entry_df)
}



#* Prepare data for On Time Start Plot.
#* 
#* @param file path to the excel file
#* @get /data/get-data-entry
function(file = NA) {

  if ( is.na(file) ) {
    source(DEMO_DATA_ENTRY_PATH)
    data_entry_df <- readDemoDataEntryService()
  }

  source('dataServices/getDataEntryService.R')
  get_data_entry(data_entry_df)
}





# Cleanup ----------------------------------------------------------------------

# rm(list = ls())