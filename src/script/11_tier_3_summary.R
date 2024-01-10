# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2023-12-22
# Updated on:   2023-12-22
#
# Description:  Arrange 3 Core Time Utilisation Plots:
#                 - Core Time Availability;
#                 - Blocked Core Time Distribution;
#                 - Core Time Utilisation.
#
# Location:     script/11_tier_3_summary.R
#



# Setup the Script -------------------------------------------------------------

source('script/08_available_core_time.R')
source('script/09_blocked_core_time.R')
source('script/10_utilised_core_time.R')




# Arrange Plotes into Tier 3 Summary ---------------------------------------


tier_3_summary <- (
  available_core_time_p +
  (blocked_core_time_p / utilised_core_time_p)
)




# Clean Up ---------------------------------------------------------------------

remove(
  available_core_time_p,
  blocked_core_time_p,
  utilised_core_time_p
)



# Display the Plot -------------------------------------------------------------

tier_3_summary



