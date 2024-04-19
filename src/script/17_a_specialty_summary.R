# Meta Data --------------------------------------------------------------------
#
# Version:      1.1
# Author:       Oleksii Dovhaniuk
# Created on:   2024-04-17
# Updated on:   2024-04-17
#
# Description:  The summary analysis of a single specialty from the data set.
#
# Location:     script/17_a_specialty_summary.R
#



# Setup the Script -------------------------------------------------------------

source('script/01_read_and_prep.R')
library(ggpubr)
source('script/18_a_specialty_cases.R')
source('script/19_a_specialty_case_min.R')
source('script/20_a_specialty_cases_by_theatre.R')
source('script/21_a_specialty_case_min_by_theatre.R')
source('script/22_a_specialty_total_rate_by_theatre.R')
source('script/23_a_specialty_demand_rates_by_theatre.R')


get_a_specialty_summary_pl <- function(target_specialty){
  
  a_specialty_cases_pl <- get_a_specialty_cases_pl(target_specialty)
  a_specialty_case_min_pl <- get_a_specialty_case_min_pl(target_specialty)
  a_specialty_cases_by_theatre_pl <- get_a_specialty_cases_by_theatre_pl(target_specialty)
  a_specialty_case_min_by_theatre_pl <- get_a_specialty_case_min_by_theatre_pl(target_specialty)
  a_specialty_total_rate_by_theatre_pl <- get_a_specialty_total_rate_by_theatre_pl(target_specialty)
  a_specialty_demand_rates_by_theatre_pl <- get_a_specialty_demand_rates_by_theatre_pl(target_specialty)
  
  
  a_specialty_summary_by_theatre_pl <- ggarrange(
    a_specialty_cases_by_theatre_pl,
    a_specialty_case_min_by_theatre_pl,
    labels = c("C", "D"),
    ncol = 2, 
    nrow = 1
  )
  
  a_specialty_summary_rates_pl <- ggarrange(
    a_specialty_total_rate_by_theatre_pl,
    a_specialty_demand_rates_by_theatre_pl,
    labels = c("E", "F"),
    ncol = 2, 
    nrow = 1
  )
  
  a_specialty_summary_pl <- ggarrange(
    a_specialty_cases_pl, 
    a_specialty_case_min_pl,
    a_specialty_summary_by_theatre_pl,
    a_specialty_summary_rates_pl,
    labels = c("A", "B"),
    ncol = 1, 
    nrow = 4
  )
  
  return(a_specialty_summary_pl)
}

get_a_specialty_summary_pl("427")



