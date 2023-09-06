# Course: GRS-80436 / MSc Thesis
# Project: AHN canopy gap detection
# Author: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: L.C. Vreugdenhil MSc.
# Year: 2022

### Description ###
# Script to randomly select 4 managed and 4 unmanaged forest plots to prepare
# field visit for gap validation


## Load forest plots in study area

# Load all forest plots
path_plots <- "ahncanopygaps/InputData/Speulderbos/boslaag nieuw Veluwe Achterhoek met objectnaam/vak afdeling veluwe achterhoek.shp"
forest_plots_all <- st_read(path_plots) %>% 
  st_transform(7415)

# Load all forest statistics
path_plots_stats <- "ahncanopygaps/InputData/Speulderbos/boslaag nieuw Veluwe Achterhoek met objectnaam/boslaag nieuw Veluwe Achterhoek met objectnaam.shp"
forest_plots_stats_all <- st_read(path_plots_stats) %>% 
  st_transform(7415)

# Clip forest_plots to study area extent
forest_plots_study <- sf::st_intersection(forest_plots_all, study_area_speulderbos)
forest_plots_stats <- sf::st_intersection(forest_plots_stats_all, study_area_speulderbos)

# Join the datasets together
forest_plots_data <- sf::st_join(forest_plots_study, forest_plots_stats, st_intersects, largest = T)


## Add managed/unmanaged attribute to the forest plots

# Select plots using pipes
forest_plots_study_classes <- forest_plots_data %>% 
  dplyr::mutate(managed_class = case_when(str_detect(COMBI, "^10") ~ "unmanaged", 
                                          str_detect(BOOMSOORT, "BU") & KIEMJAAR < 1926 ~ "unmanaged",
                                          .default = "managed"))

# Write forest_plots_study_classes to file
st_write(forest_plots_study_classes["managed_class"], "~/ahncanopygaps/InputData/forest_plots_study_classes.gpkg")

# Select plots using pipes
forest_plots_three_management_classes <- forest_plots_data %>% 
  dplyr::mutate(managed_class = case_when(str_detect(COMBI, "^10") ~ "unmanaged", 
                                          KIEMJAAR == 1835 ~ "pseudo_unmanaged",
                                          .default = "managed"))

# Write forest_plots_three_management_classes to file
st_write(forest_plots_three_management_classes["managed_class"], "~/ahncanopygaps/InputData/forest_plots_three_management_classes.gpkg")

## Randomly select rows from managed and unmagaged datasets

# Select managed rows
forest_plots_managed <- forest_plots_study_classes %>% 
  dplyr::filter(managed_class == "managed")

# Select unmanaged rows
forest_plots_unmanaged <- forest_plots_study_classes %>% 
  dplyr::filter(managed_class == "unmanaged")

# Randomly select 5 managed and unmagaged rows
managed_selection <- forest_plots_managed[sample(nrow(forest_plots_managed), 5),] %>% 
  dplyr::select(ID)
unmanaged_selection <- forest_plots_unmanaged[sample(nrow(forest_plots_unmanaged), 5),] %>% 
  dplyr::select(ID)

# I will visit managed plots 25 A2, 17 A3, 11 F2 9 D, and 17 E3 and unmanaged plots
# 10 J, 15 N, 17 M, 15 X and 11 B

# Get gaps that are situated in the to visit plots
to_visit_gaps_managed <- new_gaps_polygons[st_intersects(new_gaps_polygons, st_union(managed_selection)) %>% lengths > 0,]
to_visit_gaps_unmanaged <- new_gaps_polygons[st_intersects(new_gaps_polygons, st_union(unmanaged_selection)) %>% lengths > 0,]

# View plots to visit
plot(st_geometry(study_area_speulderbos))
plot(st_geometry(managed_selection["COMBI"]), col = "green",add = T)
plot(st_geometry(unmanaged_selection["COMBI"]), col = "red", add = T)
plot(st_geometry(to_visit_gaps_managed), col = "red", add = T)
plot(st_geometry(to_visit_gaps_unmanaged), col = "green", add = T)


## Write ouput to files

# Create directory
dir_path <- "~/ahncanopygaps/InputData/FieldVisit"
if(!dir.exists(dir_path)){
  dir.create(dir_path)
}

# Write gaps to directory
st_write(to_visit_gaps_managed, paste0(dir_path, "/to_visit_gaps_managed.gpkg"))
st_write(to_visit_gaps_unmanaged, paste0(dir_path, "/to_visit_gaps_unmanaged.gpkg"))

# Write plots to directory
st_write(dplyr::select(managed_selection, COMBI), paste0(dir_path, "/managed_selection.gpkg"))
st_write(dplyr::select(unmanaged_selection, COMBI), paste0(dir_path, "/unmanaged_selection.gpkg"))


## Add forest metrics to forest plots

# Investigate the variables of the two forest plot data sets
colnames(forest_plots_study_classes)
colnames(forest_metrics_raster_metrics)

# Prepare forest_plots_study_classes for join
forest_plots_characteristics <- forest_plots_study_classes %>% 
  sf::st_drop_geometry() %>% 
  tibble() %>% 
  dplyr::select(COMBI, BOOMSOORT, KIEMJAAR, managed_class)
forest_plots_metrics_and_class$managed_class <- as.factor(forest_plots_metrics_and_class$managed_class)
forest_plots_managed_metrics <- forest_plots_metrics_and_class[which(forest_plots_metrics_and_class$managed_class == "managed"),]
forest_plots_unmanaged_metrics <- forest_plots_metrics_and_class[which(forest_plots_metrics_and_class$managed_class == "unmanaged"),]

# Join data sets
forest_plots_metrics_and_class <- forest_metrics_raster_metrics %>% 
  dplyr::left_join(forest_plots_characteristics)

# Write forest_plots_metrics_and_class to file
st_write(forest_plots_metrics_and_class, "~/ahncanopygaps/InputData/forest_plots_metrics_and_class.gpkg")




