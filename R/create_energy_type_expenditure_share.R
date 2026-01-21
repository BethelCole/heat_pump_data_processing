# calculate share of energy expenditures on each energy type

create_energy_type_expenditure_share = function(heating_equipment_dataset){
  
  energy_type_expenditure_share <- heating_equipment_dataset %>%
    mutate(energy_expenditure_dol = dollarng + dollarel + dollarfo+ dollarlp,
           dollarng_share_energy_expenditures = dollarng / energy_expenditure_dol,
           dollarel_share_energy_expenditures = dollarel / energy_expenditure_dol,
           dollarfo_share_energy_expenditures = dollarfo / energy_expenditure_dol,
           dollarlp_share_energy_expenditures = dollarlp / energy_expenditure_dol,
           check_shares = dollarng_share_energy_expenditures + dollarel_share_energy_expenditures + dollarfo_share_energy_expenditures + dollarlp_share_energy_expenditures) %>%
    mutate(equipm_summarized = case_when(equipm == "Heat pump" ~ "Heat pump", # Heat pump
                                         equipm == "No space heating" ~ "No space heating", # No space heating
                                         TRUE ~ "All other"))
  
  return(energy_type_expenditure_share)
}