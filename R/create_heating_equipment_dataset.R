# function to create the dataset used to generate a stacked bar plot showing population weighted use of heating equipment

create_heating_equipment_dataset = function(fusionACS_data_20250723_01_ACS_H,
                                            fusionACS_data_20250723_02_RECS_2015_H,
                                            grouping_detail = "equipm" # select either equipm (for all values available) or equipm_summarized (for heat pump, no space heating, all other)
){
  
  table_equipm <- fusionACS_data_20250723_01_ACS_H %>%
    dplyr::select(year, hid, weight, puma10, hincp) %>%
    mutate(state_fips = substr(puma10,1,2)) %>%
    left_join(fusionACS_data_20250723_02_RECS_2015_H, by = c("year", "hid"))
  
  if(grouping_detail == "equipm"){
    
    table_equipm <- table_equipm %>%
      group_by(year, state_fips, equipm) %>% #need to use survey object so that the weights are properly used to calculate the average expenditure shares
      summarize(mean_shrbtusph = weighted.mean(shrbtusph, w=weight, na.rm = TRUE),
                mean_shrdolsph = weighted.mean(shrdolsph, w=weight),
                mean_shrelcol = weighted.mean(shrelcol, w=weight),
                pop = sum(weight)) %>%
      ungroup() %>%
      group_by(year, state_fips) %>%
      mutate(total_pop = sum(pop),
             population_proportion = pop/total_pop) %>%
      ungroup()
    
  } else {
    
    grouping_vars = c("year", "state_fips", "equipm_summarized")
    
    table_equipm <- table_equipm %>%
      mutate(equipm_summarized = case_when(equipm == "Heat pump" ~ "Heat pump", # Heat pump
                                           equipm == "No space heating" ~ "No space heating", # No space heating
                                           TRUE ~ "All other")) %>%
      group_by(year, state_fips, equipm_summarized) %>% #need to use survey object so that the weights are properly used to calculate the average expenditure shares
      summarize(mean_shrbtusph = weighted.mean(shrbtusph, w=weight, na.rm = TRUE),
                mean_shrdolsph = weighted.mean(shrdolsph, w=weight),
                mean_shrelcol = weighted.mean(shrelcol, w=weight),
                pop = sum(weight)) %>%
      ungroup() %>%
      group_by(year, state_fips) %>%
      mutate(total_pop = sum(pop),
             population_proportion = pop/total_pop) %>%
      ungroup()
  }
  
  
  return(table_equipm)
  
}