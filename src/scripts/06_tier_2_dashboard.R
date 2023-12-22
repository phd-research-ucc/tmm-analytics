# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2023-12-22
# Updated on:   2023-12-22
#
# Description:  Arrange 4 plots intow a Tier - 3 dashboard.
#               Plots:
#                 - On Time Start;
#                 - Inter Operational Interval;
#                 - Early Finish/ Over Run
#                 - Utilisation Rate and Number of Surgery Cases
#
# Location:     scripts/06_tier_2_dashboard.R
#



# Setup the Script -------------------------------------------------------------

source('scripts/02_on_time_start.R')
source('scripts/03_inter_operational_interval.R')
source('scripts/04_early_finish_and_over_run.R')
source('scripts/05_utilisation_and_cases.R')




# Arrange the Tier 2 Dashboard -------------------------------------------------


tier_2_dashboard <- (
  on_time_start_p + 
    inter_op_interval_p +
    early_finish_over_run_p 
) +
  plot_layout(ncol = 3, nrow = 1, widths = c(1, 3, 1))

tier_2_dashboard <- tier_2_dashboard / utilisation_and_cases_p




# Clean Up ---------------------------------------------------------------------

remove(
  # on_time_start_p,
  # inter_op_interval_p,
  # early_finish_over_run_p,
  # utilisation_and_cases_p
)



# Display the Plot -------------------------------------------------------------

tier_2_dashboard



