# create heating_equipment_dataset

create_heating_equipment_dataset = function(fusionACS_data_20250723_01_ACS_H,
                                            fusionACS_data_20250723_02_RECS_2015_H){
  
  table_equipm <- fusionACS_data_20250723_01_ACS_H %>%
    dplyr::select(year, hid, weight, puma10, hincp) %>%
    mutate(state_fips = substr(puma10,1,2)) %>%
    left_join(fusionACS_data_20250723_02_RECS_2015_H, by = c("year", "hid"))
  
  return(table_equipm)
  
}