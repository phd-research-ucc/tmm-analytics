# Meta Data --------------------------------------------------------------------
#
# Version:      1.1
# Author:       Oleksii Dovhaniuk
# Created on:   2024-01-10
# Updated on:   2024-01-11
#
# Description:  Reads the Data Entry data sheet form CSV or Excel file.
#
# Location:     services/fileServices/readDataEntryExampleService.R
#


# Load libraries ---------------------------------------------------------------

library(plumber)
library(readxl)




# Service ----------------------------------------------------------------------


createReadDataEntryExampleService <- function() {
  pr <- plumber::plumber()

  pr$handle(
    'POST', 
    '/file', 
    function(req, res) {
      file <- '2023-12-08_tmm_1month_1hospital_sample_data.xlsx'
      result <- readxl::read_xlsx(file)
      return(result)
    }
  )

  return(pr)
}

readDataEntryExampleService <- createReadDataEntryExampleService()

