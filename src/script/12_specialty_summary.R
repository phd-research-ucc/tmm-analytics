# Meta Data --------------------------------------------------------------------
#
# Version:      1.1
# Author:       Oleksii Dovhaniuk
# Created on:   2024-03-19
# Updated on:   2024-04-17
#
# Description:  The summary analysis of the specialties of the dataset.
#
# Location:     script/12_specialty_summary.R
#



# Setup the Script -------------------------------------------------------------

source('script/01_read_and_prep.R')
library(ggpubr)
source('script/13_specialty_cases.R')
source('script/14_specialty_durations.R')
source('script/15_specialty_demand_rates.R')
source('script/16_specialty_total_rate.R')


# Combined Plot of Specialty Summary -------------------------------------------

specialty_cases_pl <- get_specialty_cases_pl()
specialty_durations_pl <- get_specialty_durations_pl()
specialty_demand_rates_pl <- get_specialty_demand_rates_pl()
specialty_total_rate_pl <- get_specialty_total_rate_pl()

specialty_summary_pl <- ggarrange(
  specialty_cases_pl, 
  specialty_durations_pl, 
  specialty_total_rate_pl,
  specialty_demand_rates_pl, 
  labels = c("A", "B", "C", "D"),
  ncol = 2, nrow = 2)

specialty_summary_pl


