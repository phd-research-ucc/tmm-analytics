# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2023-12-22
# Updated on:   2023-12-22
#
# Description:  Summarise the data
#
# Location:     scripts/07_summary_table.R
#



# Setup the Script -------------------------------------------------------------

source('scripts/01_read_and_prep.R')
source('scripts/05_utilisation_and_cases.R')




# Mine Needed Data -------------------------------------------------------------


adhoc_summary_table <- theatre_adhocs_df |> 
  group_by(reason) |>
  summarise(
    total_lost_time_m = sum(lost_time_m),
    total_lost_time_h = sum(lost_time_h)
  ) |> 
  complete(
    reason, 
    fill = list(
      total_lost_time_m = 0, 
      total_lost_time_h = 0)
  )

util_summary_df <- utilisation_and_cases_df |> 
  summarize(
    utilisation = sum(total_incore_time) / sum(total_core_time),
    core_cases = floor( sum(incore_cases) ),
    expected_core_cases = floor( (1.95 - utilisation) * core_cases ),
    non_core_cases = sum(outcore_cases)
  )

summary_df <- clean_data_df |> 
  summarize(
    min_date = min(surgery_start_date),
    max_date = max(surgery_start_date),
    start_date = min_date - as.integer( format(min_date, '%w') ),
    end_date = max_date - as.integer( format(max_date, '%w') ) + 6,
    
    num_weeks = as.integer((end_date - start_date + 1) / 7),
    start_week = 1,
    end_week = start_week + num_weeks - 1,
    
    num_theatres = nlevels(theatre),
    num_specialties = nlevels(specialty),
    
    capacity = num_theatres * num_weeks,
    possible_core_time_h = sum(theatre_core_time_df$core_time_h) * capacity,
    possible_core_time_m = sum(theatre_core_time_df$core_time_m) * capacity,
    
    potential_core_time_h = 
      possible_core_time_h - sum(adhoc_summary_table$total_lost_time_h),
    potential_core_time_m = 
      possible_core_time_m - sum(adhoc_summary_table$total_lost_time_m),
    
    unstaffed_time_h = '-',
    unstaffed_time_m = '-',
    staffed_time_h = potential_core_time_h,
    staffed_time_m = potential_core_time_m,
    
    utilisation_live = scales::percent(
      util_summary_df$utilisation, 
      scale = 100,
      accuracy = 1
    ),
    utilisation_goal = '95.0%',
    
    core_cases_live = util_summary_df$core_cases,
    core_cases_goal = util_summary_df$expected_core_cases,
    non_core_cases_live = util_summary_df$non_core_cases,
    non_core_cases_goal = 0
  ) |> 
  select(start_week, end_week, everything(), -min_date, -max_date) |> 
  add_row() |> 
  mutate(
    start_week = c(start_week[1], end_week[1]),
    start_date = c(start_date[1], end_date[1]),
    possible_core_time_m = c(possible_core_time_m[1], possible_core_time_h[1]),
    potential_core_time_m = c(potential_core_time_m[1], potential_core_time_h[1]),
    unstaffed_time_m = c(unstaffed_time_m[1], unstaffed_time_h[1]),
    staffed_time_m = c(staffed_time_m[1], staffed_time_h[1]),
    utilisation_live = c(utilisation_live[1], utilisation_goal[1]),
    core_cases_live = c(core_cases_live[1], core_cases_goal[1]),
    non_core_cases_live = c(non_core_cases_live[1], non_core_cases_goal[1])
  ) |>
  
  rename(
    dates = start_date,
    weeks = start_week,
    possible_core_time = possible_core_time_m,
    potential_core_time = potential_core_time_m,
    unstaffed_time = unstaffed_time_m,
    staffed_time = staffed_time_m,
    utilisation = utilisation_live,
    core_cases = core_cases_live,
    non_core_cases = non_core_cases_live
  ) |> 
  
  select(
    -end_date,
    -end_week,
    -possible_core_time_h,
    -potential_core_time_h,
    -unstaffed_time_h,
    -staffed_time_h,
    -utilisation_goal,
    -core_cases_goal,
    -non_core_cases_goal
  ) |>
  
  t() |>
  as.data.frame() |> 
  rename(c1 = V1, c2=V2)




# Clean Up ---------------------------------------------------------------------

remove(
  # adhoc_summary_table,
  clean_data_df,
  theatre_adhocs_df,
  util_summary_df,
  utilisation_and_cases_df,
  utilisation_and_cases_p,
  theatre_core_time_df
)



# Display the Plot -------------------------------------------------------------

knitr::kable(summary_df, format = 'markdown')



