# Meta Data --------------------------------------------------------------------
#
# Version:      1.1
# Author:       Oleksii Dovhaniuk
# Created on:   2024-01-11
# Updated on:   2024-01-12
#
# Description:  Combines all services of the project
#               into one API collection.
#
# Location:     services/main.R
#




# Setup ------------------------------------------------------------------------


# #* Sets up the global environment for the API services.
# #* @get /setup
# function(){
#   # Load required packages.
#   source('../script/_load_packages.R') 
#   
#   # List paths of all services.
#   service_file_paths = list.files(
#     'services', 
#     pattern = 'Service\\.R$', 
#     all.files = TRUE, 
#     full.names = TRUE, 
#     recursive = TRUE
#   )
#   
#   # Source all services.
#   for (service_path in service_file_paths) {
#     prepared_path <- file.path( basename(service_path) )
#     source(service_path)
#     cat('Service sourced:', service_path, '\n')
#   }
# }




# Connecting to Plumber --------------------------------------------------------


#* Reads data entry sheet from file end returns data frame
#* @param type 'csv', 'xls', or 'xlsx' (by default - 'xlsx')
#* @get /file/read-demo-data-entry
function(type='xlsx') {
  source('fileServices/readDemoDataentryService.R')
  readDemoDataEntryService(type)
}




# Cleanup ----------------------------------------------------------------------

# rm(list = ls())