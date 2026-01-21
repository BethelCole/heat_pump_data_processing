create_density_plot = function(energy_type_expenditure_share,
                               analysis_var = "dollarel_share_energy_expenditures",
                               yearSelect = 2017,
                               facetSelect = TRUE,
                               facet_var = "euipment_summary"){
  
  
  # if(facet_var == "usrep_region") {
  #   CEI_pic <- CEI_pic %>%
  #     filter(is.na(state) == FALSE)
  # }
  
  
  
  energy_type_expenditure_share <- energy_type_expenditure_share %>%
    dplyr::filter(year == yearSelect)
  
  if(analysis_var %in% c("dollarng_share_energy_expenditures", 
                         "dollarel_share_energy_expenditures",
                         "dollarfo_share_energy_expenditures",
                         "dollarlp_share_energy_expenditures")){
    
    p1 <- ggplot() +
      geom_density(data = energy_type_expenditure_share,
                   aes(x = .data[[analysis_var]],
                       weight = weight,
                       #color = "CEI"
                       ),
                   linewidth = 1)
  }

  # p1 <- ggplot() +
  #   geom_density(data = CEI_pic,
  #                aes(x = .data[[analysis_var]],
  #                    weight = weight,
  #                    color = "CEI"),
  #                linewidth = 1) +
  #   geom_density(data = SM_pic,
  #                aes(x = .data[[analysis_var]],
  #                    weight = weight,
  #                    color = "StatMatch_prior"),
  #                linewidth = 1) +
  #   
  #   geom_density(data = fusioncei_pic,
  #                aes(x = .data[[analysis_var]],
  #                    weight = weight,
  #                    color = "fusionCEI"),
  #                linewidth = 1) +
  #   
  #   geom_density(data = SM_using_CEI_processed_df,
  #                aes(x = .data[[analysis_var]],
  #                    weight = weight,
  #                    color = "StatMatch_new"),
  #                linewidth = 1) +
  #   
  #   xlim(-0.1, 0.2)
  
  
  if(facet_var == "equipment_all"){
    
  } else if (facet_var == "euipment_summary"){
    
  }
  
if(facetSelect == TRUE) {
  
    p2 <- p1 + facet_wrap(~.data[[facet_var]],
                          scales = "free")
  
} else if (facetSelect == FALSE) {
  
  p2 <- p1
  
}
  
  p_final <- p2 +
    scale_x_continuous(labels = scales::percent_format()) +
  
    #facet_wrap(~income_updated) +
    # scale_color_manual(name = "Datasets",
    #                    values = c("CEI" = "red",
    #                               "StatMatch" = "darkgreen",
    #                               "fusionCEI" = "blue",
    #                               "StatMatch_new" = "purple")) +
    scale_color_viridis_d() +
    labs(title = paste0("Expenditure Share of Income - "),
         color = "Survey") +
    theme_bw()
    
  
  name = paste0("output/expenditureDensityByIncome_",analysis_var,"_",facet_var,"_",Sys.Date(),".png")
  
  # ggsave(p_final,
  #        filename = name,
  #        create.dir = TRUE)
  
  #paste0("output/survey_comparison_figs_",Sys.Date(),".html")
  
 
  return(p_final)
    
}

#ggbarcrosstabs_svy(recs_object,income_group_extended,energy_dollars) 
# list_of_analysis_vars <- c("energy_pct_income", "elec_pct_income", "ngas_pct_income", "ofuel_pct_income", "trn_pct_income", "srv_pct_income", "oth_sec_pct_income", "trspt_gas_pct_income", "bld_pct_income", "eis_pct_income", "agr_pct_income", "che_pct_income", "nmp_pct_income", "pmt_pct_income")
# 
# list_of_analysis_vars <- c("energy_pct_income")
# 
# for (i in list_of_analysis_vars) {
#   
#   create_density_plot(facetSelect = TRUE,
#                       facet_var = "income_updated",
#                       i)
# }
