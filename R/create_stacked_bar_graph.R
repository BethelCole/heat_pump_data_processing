# function to create stacked bar graph showing population weighted use of space heating equipment

create_stacked_bar_graph = function(table_equipm,
                                    yearSelect = 2015,
                                    fill_options = "equipm"){
  
  # if("equipm" %in% names(table_equipm)){
  #   fill_options = equipm
  # } else {
  #   fill_options = equipm_summarized
  # }
  
  
  space_heating_stacked_bar_chart <- table_equipm %>%
    dplyr::filter(year == yearSelect) %>%
    ggplot(aes(x = state_fips, y = population_proportion, fill = .data[[fill_options]])) +
    geom_bar(stat = "identity")
  #geom_bar(stat = "identity", position = "stack")
  
  return(space_heating_stacked_bar_chart)
}