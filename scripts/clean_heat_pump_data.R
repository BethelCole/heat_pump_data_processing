# clean heat pump data

library(tidycensus)
library(readxl)
library(tidyverse)

heat_pump_data <- read.csv("data/ashp_savings_Joule24.csv")

# clean up variables for example transform numeric GEOID variable into 5-digit state-fip
heat_pump_data_clean <- heat_pump_data %>%
  mutate(county_fip_5_digit = str_pad(as.character(GEOID), width = 5, side = "left", pad = "0"),
         state_fp_2_digit = str_pad(as.character(STATEFP), width = 2, side = "left", pad = "0"))



# pull in county population
# pull in county density (urban/rural)
county_density <- read_excel("data/2020_UA_COUNTY.xlsx")

county_density_clean <- county_density %>%
  mutate(county_fip_5_digit = paste0(STATE, COUNTY))


# join heat pump data and county density data

heat_pump_with_density <- heat_pump_data_clean %>%
  dplyr::select(-STATE_NAME) %>%
  left_join(county_density_clean, by = "county_fip_5_digit")

# share of each state that uses each main heating fuel
fuel_allocation_within_state <- heat_pump_with_density %>%
  group_by(STATEFP, STUSPS, STATE_NAME, Main.heating.fuel) %>%
  summarize(pop = sum(POP_COU)) %>%
  pivot_wider(names_from = Main.heating.fuel,
              values_from = pop) %>%
  rowwise() %>%
  mutate(row_total = sum(c(Electricity, `Utility gas`, Wood, `Bottled, tank, or LP gas`, `Fuel oil, kerosene, etc`), na.rm = TRUE),
         electricity_pop_pct = (Electricity / row_total) * 100,
         utilitygas_pop_pct = (`Utility gas` / row_total) * 100,
         wood_pop_pct = (Wood / row_total) * 100,
         bottledgas_pop_pct = (`Bottled, tank, or LP gas` / row_total) * 100,
         fueloil_pop_pct = (`Fuel oil, kerosene, etc` / row_total) * 100) %>%
  ungroup()

heat_pump_pledge <- fuel_allocation_within_state %>%
  mutate(heat_pump_pledge = case_when(STATE_NAME %in% c("California", "Colorado", "Maine", "Maryland", "Massachusetts", "New Jersey", "New York", "Oregon", "Rhode Island", "District of Columbia", "Washington") ~ 1,
                                       TRUE ~ 0))


county_density_clean_grouped <- county_density_clean %>%
  group_by(STATE, STATE_NAME) %>%
  summarise(state_pop = sum(POP_COU),
            state_pop_urban = sum(POP_URB),
            state_pop_rural = sum(POP_RUR)) %>% #,
  mutate(pop_pct_urban = 100 * (state_pop_urban/state_pop),
         pop_pct_rural = 100 * (state_pop_rural/state_pop))



combined_data <- heat_pump_pledge %>%
  left_join(county_density_clean_grouped, by = "STATE_NAME")



# doing the same but using state and density

# share of each state that uses each main heating fuel
fuel_allocation_within_state_using_density <- heat_pump_with_density %>%
  mutate(county_pop_mainly_urban = case_when(POPPCT_URB > 0.5 ~ 1,
                                      TRUE ~ 0)) %>%
  group_by(STATEFP, STUSPS, STATE_NAME, Main.heating.fuel, county_pop_mainly_urban) %>%
  summarize(pop = sum(POP_COU)) %>%
  pivot_wider(names_from = Main.heating.fuel,
              values_from = pop) %>%
  rowwise() %>%
  mutate(row_total = sum(c(Electricity, `Utility gas`, Wood, `Bottled, tank, or LP gas`, `Fuel oil, kerosene, etc`), na.rm = TRUE),
         electricity_pop_pct = (Electricity / row_total) * 100,
         utilitygas_pop_pct = (`Utility gas` / row_total) * 100,
         wood_pop_pct = (Wood / row_total) * 100,
         bottledgas_pop_pct = (`Bottled, tank, or LP gas` / row_total) * 100,
         fueloil_pop_pct = (`Fuel oil, kerosene, etc` / row_total) * 100) %>%
  ungroup()

heat_pump_pledge_using_density <- fuel_allocation_within_state_using_density %>%
  mutate(heat_pump_pledge = case_when(STATE_NAME %in% c("California", "Colorado", "Maine", "Maryland", "Massachusetts", "New Jersey", "New York", "Oregon", "Rhode Island", "District of Columbia", "Washington") ~ 1,
                                      TRUE ~ 0))


heat_pump_pledge_using_density_clean <- heat_pump_pledge_using_density %>%
  dplyr::select(STATEFP, STUSPS, STATE_NAME, county_pop_mainly_urban, electricity_pop_pct, utilitygas_pop_pct, wood_pop_pct, bottledgas_pop_pct, fueloil_pop_pct, heat_pump_pledge) %>%
  mutate(pop_density = case_when(county_pop_mainly_urban == 1 ~ "urban",
                                 TRUE ~ "rural")) %>%
  dplyr::select(-county_pop_mainly_urban)

heat_pump_pledge_using_density_clean_wide <- heat_pump_pledge_using_density_clean %>%
  dplyr::select(-heat_pump_pledge) %>%
  pivot_wider(names_from = pop_density,
              values_from = c(electricity_pop_pct, utilitygas_pop_pct, wood_pop_pct, bottledgas_pop_pct, fueloil_pop_pct))

# county_density_clean_grouped_using_density <- county_density_clean %>%
#   group_by(STATE, STATE_NAME) %>%
#   summarise(pop_state = sum(POP_COU),
#             #aland_mi2_state = sum(ALAND_Mi²_COU),
#             pop_urb_state = sum(POP_URB)) %>% #,
#   #aland_mi2_urb_state = sum(ALAND_Mi²_URB))
#   mutate(urban_pct = 100 * (pop_urb_state/pop_state))

# county_density_clean_urban_rural <- county_density_clean_grouped %>%
#   pivot_longer(cols = c(state_pop_urban, state_pop_rural, pop_pct_urban, pop_pct_rural) ,
#                names_to = "variables",
#                values_to = "values")


combined_data_full <- county_density_clean_grouped %>%
  dplyr::select(-c(pop_pct_urban, pop_pct_rural)) %>%
  left_join(heat_pump_pledge_using_density_clean_wide, by = "STATE_NAME") %>%
  dplyr::filter(! STATE_NAME %in% c("American Samoa", "Guam", "Commonwealth of the Northern Marianas", "Puerto Rico", "US Virgin Islands"))

cleaned_heat_pump_data <- combined_data_full

write.csv(cleaned_heat_pump_data, file = "output/cleaned_heat_pump_data.csv", row.names = FALSE)
write.csv(cleaned_heat_pump_data, file = "../energy_affordability_paper/data/Heat_Pump_Data/cleaned_heat_pump_data.csv", row.names = FALSE)

# cat("CSV file 'combined_data_full.csv' created successfully.\n")  
# The cat function is a versatile tool in R for concatenating and printing objects. It allows for customization of the output format and can direct the output to files, making it useful for generating reports and logs.


# cost
# our understanding is that cost savings are per household (based on info in the supplementary files of the ashp paper)
# We calculate a weighted average at the state level and at the state-density level using housing count as a proxy for households

cost_df <- heat_pump_with_density %>%
  dplyr::select(STATEFP, state_fp_2_digit, county_fip_5_digit, Main.heating.fuel, Main.heating.fuel.cost, Cost.ASHP, Cost.savings, HOU_COU, HOU_URB, HOU_RUR)

# Main heating fuel cost
cost_df_extended <- cost_df %>%
  mutate(county_total_heating_cost = Main.heating.fuel.cost * HOU_COU,
         county_urban_heating_cost = Main.heating.fuel.cost * HOU_URB,
         county_rural_heating_cost = Main.heating.fuel.cost * HOU_RUR) %>%
  group_by(STATEFP, state_fp_2_digit, Main.heating.fuel) %>%
  summarise(state_fuel_total_heating_cost = sum(county_total_heating_cost),
            state_fuel_urban_heating_cost = sum(county_urban_heating_cost),
            state_fuel_rural_heating_cost = sum(county_rural_heating_cost),
            state_housing = sum(HOU_COU),
            state_urban_housing = sum(HOU_URB),
            state_rural_housing = sum(HOU_RUR)) %>%
  ungroup() %>%
  mutate(hh_heating_cost_by_fuel = state_fuel_total_heating_cost/state_housing,
         hh_heating_cost_by_fuel_urban = state_fuel_urban_heating_cost/state_urban_housing,
         hh_heating_cost_by_fuel_rural = state_fuel_rural_heating_cost/state_rural_housing)


cost_clean <- cost_df_extended %>%
  dplyr::select(STATEFP, state_fp_2_digit, Main.heating.fuel, hh_heating_cost_by_fuel, hh_heating_cost_by_fuel_urban, hh_heating_cost_by_fuel_rural) %>%
  pivot_wider(names_from = Main.heating.fuel,
              values_from = c(hh_heating_cost_by_fuel, hh_heating_cost_by_fuel_urban, hh_heating_cost_by_fuel_rural))


# Per household cost savings from ASHP by main heating fuel

# generate county level (by main heating fuel) cost savings in preparation for aggregation to state level (by main heating fuel)
cost_savings <- cost_df %>%
  mutate(county_total_heating_cost_savings = Cost.savings * HOU_COU,
         county_urban_heating_cost_savings = Cost.savings * HOU_URB,
         county_rural_heating_cost_savings = Cost.savings * HOU_RUR) %>%
  group_by(STATEFP, state_fp_2_digit, Main.heating.fuel) %>%
  summarise(state_fuel_total_heating_cost_savings = sum(county_total_heating_cost_savings),
            state_fuel_urban_heating_cost_savings = sum(county_urban_heating_cost_savings),
            state_fuel_rural_heating_cost_savings = sum(county_rural_heating_cost_savings),
            state_housing = sum(HOU_COU),
            state_urban_housing = sum(HOU_URB),
            state_rural_housing = sum(HOU_RUR)) %>%
  ungroup() %>%
  mutate(hh_heating_cost_savings_by_main_heating_fuel = state_fuel_total_heating_cost_savings/state_housing,
         hh_heating_cost_savings_by_main_heating_fuel_urban = state_fuel_urban_heating_cost_savings/state_urban_housing,
         hh_heating_cost_savings_by_main_heating_fuel_rural = state_fuel_rural_heating_cost_savings/state_rural_housing)


cost_savings_clean <- cost_savings %>%
  dplyr::select(STATEFP, state_fp_2_digit, Main.heating.fuel, hh_heating_cost_savings_by_main_heating_fuel, hh_heating_cost_savings_by_main_heating_fuel_urban, hh_heating_cost_savings_by_main_heating_fuel_rural) %>%
  pivot_wider(names_from = Main.heating.fuel,
              values_from = c(hh_heating_cost_savings_by_main_heating_fuel, hh_heating_cost_savings_by_main_heating_fuel_urban, hh_heating_cost_savings_by_main_heating_fuel_rural))



heat_pump_cost_savings_data <- cost_savings_clean %>%
  rename(hh_hp_savings_electricity = hh_heating_cost_savings_by_main_heating_fuel_Electricity,
         hh_hp_savings_utilitygas = `hh_heating_cost_savings_by_main_heating_fuel_Utility gas`,
         hh_hp_savings_wood = hh_heating_cost_savings_by_main_heating_fuel_Wood,
         hh_hp_savings_lpgas = `hh_heating_cost_savings_by_main_heating_fuel_Bottled, tank, or LP gas`,
         hh_hp_savings_fueloil = `hh_heating_cost_savings_by_main_heating_fuel_Fuel oil, kerosene, etc`,
         # urban breakout
         hh_hp_savings_electricity_urban = hh_heating_cost_savings_by_main_heating_fuel_urban_Electricity,
         hh_hp_savings_utilitygas_urban = `hh_heating_cost_savings_by_main_heating_fuel_urban_Utility gas`,
         hh_hp_savings_wood_urban = hh_heating_cost_savings_by_main_heating_fuel_urban_Wood,
         hh_hp_savings_lpgas_urban = `hh_heating_cost_savings_by_main_heating_fuel_urban_Bottled, tank, or LP gas`,
         hh_hp_savings_fueloil_urban = `hh_heating_cost_savings_by_main_heating_fuel_urban_Fuel oil, kerosene, etc`,
         # rural breakout
         hh_hp_savings_electricity_rural = hh_heating_cost_savings_by_main_heating_fuel_rural_Electricity,
         hh_hp_savings_utilitygas_rural = `hh_heating_cost_savings_by_main_heating_fuel_rural_Utility gas`,
         hh_hp_savings_wood_rural = hh_heating_cost_savings_by_main_heating_fuel_rural_Wood,
         hh_hp_savings_lpgas_rural = `hh_heating_cost_savings_by_main_heating_fuel_rural_Bottled, tank, or LP gas`,
         hh_hp_savings_fueloil_rural = `hh_heating_cost_savings_by_main_heating_fuel_rural_Fuel oil, kerosene, etc`)

write.csv(heat_pump_cost_savings_data, file = "output/heat_pump_cost_savings_data.csv", row.names = FALSE)
write.csv(heat_pump_cost_savings_data, file = "../energy_affordability_paper/data/Heat_Pump_Data/heat_pump_cost_savings_data.csv", row.names = FALSE)

heat_pump_cost_savings_data_small <- cost_savings_clean %>%
  dplyr::select(STATEFP, 
                state_fp_2_digit, 
                hh_heating_cost_savings_by_main_heating_fuel_Electricity,
                `hh_heating_cost_savings_by_main_heating_fuel_Utility gas`, 
                `hh_heating_cost_savings_by_main_heating_fuel_Wood`, 
                `hh_heating_cost_savings_by_main_heating_fuel_Bottled, tank, or LP gas`,
                `hh_heating_cost_savings_by_main_heating_fuel_Fuel oil, kerosene, etc`) %>%
  rename(hh_hp_savings_electricity = hh_heating_cost_savings_by_main_heating_fuel_Electricity,
         hh_hp_savings_utilitygas = `hh_heating_cost_savings_by_main_heating_fuel_Utility gas`,
         hh_hp_savings_wood = hh_heating_cost_savings_by_main_heating_fuel_Wood,
         hh_hp_savings_lpgas = `hh_heating_cost_savings_by_main_heating_fuel_Bottled, tank, or LP gas`,
         hh_hp_savings_fueloil = `hh_heating_cost_savings_by_main_heating_fuel_Fuel oil, kerosene, etc`)

write.csv(heat_pump_cost_savings_data_small, file = "output/heat_pump_cost_savings_data_small.csv", row.names = FALSE)
write.csv(heat_pump_cost_savings_data_small, file = "../energy_affordability_paper/data/Heat_Pump_Data/heat_pump_cost_savings_data_small.csv", row.names = FALSE)

# combine heat pump datasets and select subset

heat_pump_cost_savings_and_density_data <- cleaned_heat_pump_data %>%
  right_join(heat_pump_cost_savings_data, by = "STATEFP")

write.csv(heat_pump_cost_savings_and_density_data, file = "output/heat_pump_cost_savings_and_density_data.csv", row.names = FALSE)
write.csv(heat_pump_cost_savings_and_density_data, file = "../energy_affordability_paper/data/Heat_Pump_Data/heat_pump_cost_savings_and_density_data.csv", row.names = FALSE)

# small version

heat_pump_cost_savings_and_density_data_small <- cleaned_heat_pump_data %>%
  right_join(heat_pump_cost_savings_data_small, by = "STATEFP")

write.csv(heat_pump_cost_savings_and_density_data_small, file = "output/heat_pump_cost_savings_and_density_data_small.csv", row.names = FALSE)
write.csv(heat_pump_cost_savings_and_density_data_small, file = "../energy_affordability_paper/data/Heat_Pump_Data/heat_pump_cost_savings_data_small.csv", row.names = FALSE)
  