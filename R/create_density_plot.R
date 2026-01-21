create_density_plot = function(energy_type_expenditure_share,
                               analysis_var = "dollarel_share_energy_expenditures",
                               yearSelect = 2017,
                               facetSelect = TRUE,
                               facet_var = "equipm_summarized"){
  
  
  # if(facet_var == "usrep_region") {
  #   CEI_pic <- CEI_pic %>%
  #     filter(is.na(state) == FALSE)
  # }
  
  
  if(yearSelect == 0){
    
    energy_type_expenditure_share <- energy_type_expenditure_share
    
  } else if(yearSelect %in% c(2015, 2016, 2017, 2018, 2019)) {
    
    energy_type_expenditure_share <- energy_type_expenditure_share %>%
      dplyr::filter(year == yearSelect)
  }
  
  
  
  
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
  
  
  # if(facet_var == "equipment_all"){
  #   
  #   facetVariable <- "equipm"
  #     
  # } else if (facet_var == "equipment_summary"){
  #   
  #   facetVariable <- "equipm_summarized"
  # }
  
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
    # labs(title = paste0("Share of Energy Expenditure"),
    #      color = "Survey") +
    theme_bw()
    
  
  #name = paste0("output/expenditureDensityByIncome_",analysis_var,"_",facet_var,"_",Sys.Date(),".png")
  
  # ggsave(p_final,
  #        filename = name,
  #        create.dir = TRUE)
  
  #paste0("output/survey_comparison_figs_",Sys.Date(),".html")
  
 
  return(p_final)
    
}


# create_density_plot(energy_type_expenditure_share,
#                     analysis_var = "dollarel_share_energy_expenditures",
#                     yearSelect = 2017,
#                     facetSelect = TRUE,
#                     facet_var = "equipm_summarized")
