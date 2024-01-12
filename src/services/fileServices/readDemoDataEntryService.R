# Meta Data --------------------------------------------------------------------
#
# Version:      1.1
# Author:       Oleksii Dovhaniuk
# Created on:   2024-01-10
# Updated on:   2024-01-12
#
# Description:  Reads the Data Entry data sheet form CSV or Excel file.
#
# Location:     services/fileServices/readDemoDataEntryService.R
#




# Service ----------------------------------------------------------------------


readDemoDataEntryService <- function(type='xlsx'){
  library(readxl)

  base_path <- '../data/demo/'

  if (type == 'csv'){
    file <- paste0(base_path, 'demo_data_entry.csv')
    result <- read.csv(file)
    return(result)
  }
  
  if (type == 'xls'){
    file <- paste0(base_path, 'demo_tmm.xls')
  } else {
    file <- paste0(base_path, 'demo_tmm.xlsx')
  }
  result <- read_excel(file, sheet = 'Data Entry')
  return(result)
}
