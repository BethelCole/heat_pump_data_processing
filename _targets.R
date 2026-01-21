library(targets)
library(tarchetypes) # Load other packages as needed.
source("packages.R") # Packages that your targets need for their tasks.

tar_source()# Run the R scripts in the R/ folder with your custom functions


# Replace the target list below with your own:
list(
  
  # track and read in the dictionary, ACS household, and RECS household datasets downloaded from https://github.com/ummel/fusionACS/releases
  tar_target(fusionACS_data_20250723_01_dictionary_parquet, "data/Fusion_Output/fusionACS_data_2025-07-23_01/dictionary.parquet", format = "file"),
  tar_target(fusionACS_data_20250723_01_dictionary, read_parquet(fusionACS_data_20250723_01_dictionary_parquet)),
  
  tar_target(fusionACS_data_20250723_01_ACS_H_parquet, "data/Fusion_Output/fusionACS_data_2025-07-23_01/ACS_H.parquet", format = "file"),
  tar_target(fusionACS_data_20250723_01_ACS_H, read_parquet(fusionACS_data_20250723_01_ACS_H_parquet)),
  
  tar_target(fusionACS_data_20250723_02_RECS_2015_H_parquet, "data/Fusion_Output/fusionACS_data_2025-07-23_02/RECS_2015_H/M=1/part-0.parquet", format = "file"),
  tar_target(fusionACS_data_20250723_02_RECS_2015_H, read_parquet(fusionACS_data_20250723_02_RECS_2015_H_parquet)),
  
  # create equipment_use_dataset
  tar_target(heating_equipment_dataset, create_heating_equipment_dataset(fusionACS_data_20250723_01_ACS_H,
                                                                      fusionACS_data_20250723_02_RECS_2015_H)),
  
  tar_target(year_state_equipm_summary, create_year_state_equipm_summary(heating_equipment_dataset,
                                                                         grouping_detail = "equipm")),
  
  tar_target(energy_type_expenditure_share, create_energy_type_expenditure_share(heating_equipment_dataset)),
  
  # Outputs
  tar_render(heat_pump_data_viz,
             "docs/heat_pump_data_viz.Rmd",
             output_dir = 'output/',
             output_file = paste0("output/heat_pump_data_viz_",Sys.Date(),".html"),
             params = list(mode = "targets"))
  
)
