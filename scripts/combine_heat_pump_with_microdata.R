# combine_heat_pump_with_microdata
# combine heat pump data with micro household data

microdata <- read.csv("../energy_affordability_paper/output/final_df_clean.csv")
sum(microdata$weight) # 178935615 (outliers removed)
# double check the version
microdata_updated <- read.csv("../energy_affordability_paper/output/final_df_clean_updated.csv") %>%
  dplyr::select(-X)
sum(microdata_updated$weight) # 182646473 (complete dataset)

#heat pump data full
heat_pump_cost_savings_and_density_data <- read.csv("../energy_affordability_paper/data/Heat_Pump_Data/heat_pump_cost_savings_and_density_data.csv")

# complete dataset
microdata_updated_with_heat_pump <- microdata_updated %>%
  left_join(heat_pump_cost_savings_and_density_data, by = c("state" = "STUSPS"))

write.csv(microdata_updated_with_heat_pump, file = "../energy_affordability_paper/data/Heat_Pump_Data/microdata_updated_with_heat_pump.csv", row.names = FALSE)

# outliers removed
microdata_with_heat_pump <- microdata %>%
  left_join(heat_pump_cost_savings_and_density_data, by = c("state" = "STUSPS"))

write.csv(microdata_with_heat_pump, file = "../energy_affordability_paper/data/Heat_Pump_Data/microdata_with_heat_pump.csv", row.names = FALSE)



#heat pump data small
heat_pump_cost_savings_and_density_data_small <- read.csv("../energy_affordability_paper/data/Heat_Pump_Data/heat_pump_cost_savings_and_density_data_small.csv")

# complete dataset with small
microdata_updated_with_heat_pump_small <- microdata_updated %>%
  left_join(heat_pump_cost_savings_and_density_data_small, by = c("state" = "STUSPS"))

write.csv(microdata_updated_with_heat_pump_small, file = "../energy_affordability_paper/data/Heat_Pump_Data/microdata_updated_with_heat_pump_small.csv", row.names = FALSE)

# outliers removed with small
microdata_with_heat_pump_small <- microdata %>%
  left_join(heat_pump_cost_savings_and_density_data_small, by = c("state" = "STUSPS"))

write.csv(microdata_with_heat_pump_small, file = "../energy_affordability_paper/data/Heat_Pump_Data/microdata_with_heat_pump_small.csv", row.names = FALSE)
