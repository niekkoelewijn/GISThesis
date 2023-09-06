# Course: GRS-80436 / MSc Thesis
# Project: AHN canopy gap detection
# Author: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: L.C. Vreugdenhil MSc.
# Year: 2022

### Description ###
# Script to get the number of each class, both canopy gap level and forest plot
# level


## Basics

# Canopy gap level
new_gaps_complete %>% 
  sf::st_drop_geometry() %>% 
  summarise(
    n_gaps = n(),
    area = sum(shape_area)
  )

# Forest plot level
forest_metrics_raster_metrics %>% 
  sf::st_drop_geometry() %>%
  summarise(
    n_gaps = n(),
    area = sum(ForestPlot_area)
  ) 


## Managed class

# Canopy gap level
n_gaps_management <- new_gaps_beech %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    n_gaps = n(),
    area = sum(shape_area)
  ) %>% 
  mutate(share_n = round(n_gaps / sum(n_gaps),2)  ,
         share_area = round(area / sum(area),2)  )

n_gaps_management <- new_gaps_complete %>% 
  sf::st_drop_geometry() %>% 
  group_by(ntree_class) %>% 
  summarise(
    n_gaps = n(),
    area = sum(shape_area)
  ) %>% 
  mutate(share_n = round(n_gaps / sum(n_gaps),2)  ,
         share_area = round(area / sum(area),2)  )

# Forest plot level
n_plots_management <- forest_metrics_beech %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    n_plots = n(),
    area = sum(ForestPlot_area)
  ) %>% 
  mutate(share_n = round(n_plots / sum(n_plots) ,2),
         share_area = round(area / sum(area) ,2))


## ntree class

# Canopy gap level
n_gaps_ntree <- new_gaps_complete %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class,
           ntree_class) %>% 
  summarise(
    n_gaps = n(),
    area = sum(shape_area)
  ) %>% 
  group_by(managed_class) %>%
  mutate(n_relative = round(n_gaps / sum(n_gaps),2),
         area_relative = round(area / sum(area),2))


## Tree species

# Canopy gap level
n_gaps_tree_species <- new_gaps_important_species %>% 
  sf::st_drop_geometry() %>% 
  group_by(ForestPlot_tree_species) %>% 
  summarise(
    n_gaps = n(),
    area = sum(shape_area)
  ) %>% 
  dplyr::mutate(gap_share_n = n_gaps / sum(n_gaps) * 100,
                gap_share_area = area / sum(area) * 100)

# Forest plot level
forest_plots_important_species %>% 
  sf::st_drop_geometry() %>%
  summarise(
    n_plots = n(),
    area = sum(ForestPlot_area)
  ) 

n_plots_tree_species <- forest_plots_important_species %>% 
  sf::st_drop_geometry() %>% 
  group_by(ForestPlot_tree_species) %>% 
  summarise(
    n_plots = n(),
    area = sum(ForestPlot_area)
  ) %>% 
  dplyr::mutate(plot_share_n = n_plots / sum(n_plots) * 100,
                plot_share_area = area / sum(area) * 100)


## Age class

# Canopy gap level
n_gaps_age_class <- new_gaps_influence_age %>% 
  sf::st_drop_geometry() %>% 
  group_by(age_class) %>% 
  summarise(
    n_gaps = n(),
    area = sum(shape_area)
  ) %>% 
  dplyr::mutate(gap_share_n = n_gaps / sum(n_gaps) * 100,
                gap_share_area = area / sum(area) * 100)

# Forest plot level
n_plots_age_class <- forest_metrics_influence_age %>% 
  sf::st_drop_geometry() %>% 
  group_by(age_class) %>% 
  summarise(
    n_plots = n(),
    area = sum(ForestPlot_area)
  ) %>% 
  dplyr::mutate(plot_share_n = n_plots / sum(n_plots) * 100,
                plot_share_area = area / sum(area) * 100)


## Check fractions of gap classes

# Canopy gap level
fractions_gap <- new_gaps_complete %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    average_frac_NGBM = mean(fraq_NGBM),
    average_frac_NGCM = mean(fraq_NGCM),
    average_frac_NGSM = mean(fraq_NGSM),
    average_frac_RGCM = mean(fraq_RGCM)
  ) %>% 
  dplyr::mutate(total = average_frac_NGBM + average_frac_NGCM + average_frac_NGSM + average_frac_RGCM)

# Forest plot level
fractions_plot <- forest_metrics_raster_metrics %>% 
  sf::st_drop_geometry() %>%
  group_by(managed_class) %>% 
  summarise(
    average_frac_NoG = mean(ForestPlot_fraq_NoG),
    average_frac_RG = mean(ForestPlot_fraq_RG),
    average_frac_DG = mean(ForestPlot_fraq_DG),
    average_frac_NGBM = mean(ForestPlot_fraq_NGBM),
    average_frac_NGCM = mean(ForestPlot_fraq_NGCM),
    average_frac_NGSM = mean(ForestPlot_fraq_NGSM),
    average_frac_RGCM = mean(ForestPlot_fraq_RGCM)
  ) %>% 
  dplyr::mutate(total = average_frac_NoG + average_frac_RG + average_frac_DG +
                  average_frac_NGBM + average_frac_NGCM + average_frac_NGSM + average_frac_RGCM)

new_gaps_complete %>% 
  sf::st_drop_geometry() %>% 
  summarise(
    ave_frac_NGBM = mean(fraq_NGBM),
    ave_frac_NGLM = mean(fraq_NGCM),
    ave_frac_NGSM = mean(fraq_NGSM),
    ave_frac_RGLM = mean(fraq_RGCM)
  ) 
