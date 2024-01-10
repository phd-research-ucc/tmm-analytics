# Meta Data --------------------------------------------------------------------
#
# Version:      1.1
# Author:       Oleksii Dovhaniuk
# Created on:   2024-01-10
# Updated on:   2024-01-10
#
# Description:  Reads the Data Entry data sheet form CSV or Excel file.
#
# Location:     services/fileServices/readDataEntryService.R
#



# Preparation ------------------------------------------------------------------

# dataframe <- read.csv(
#   'data/raw/2023-12-08_tmm_1month_1hospital_data_entry.xlsx'
# )




# Service ----------------------------------------------------------------------


#* @get /excel
function(req, res){
  filename <- file.path(tempdir(), 'data/raw/2023-12-08_tmm_1month_1hospital_data_entry.xlsx')
  on.exit(unlink(filename))
  filename
}
