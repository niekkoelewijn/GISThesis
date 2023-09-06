# Course: GRS-80436 / MSc Thesis
# Project: AHN canopy gap detection
# Author: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: L.C. Vreugdenhil MSc.
# Year: 2022

### Description ###
# Script to create histograms to check characteristics of all canopy gaps, and
# split the canopy gaps in managed and unmanaged to compare the differences.


## Prepare data

# Add managed class to forest plots
forest_metrics_raster_metrics <- st_read("~/ahncanopygaps/InputData/forest_metrics_raster_metrics.gpkg") 
forest_plots_class <- st_read("~/ahncanopygaps/InputData/forest_plots_study_classes.gpkg") 
new_gaps_all_metrics <- st_read("~/ahncanopygaps/InputData/new_gaps_all_metrics.gpkg")
forest_plot_metrics_class <- sf::st_join(forest_metrics_raster_metrics, forest_plots_class, st_intersects, largest = T)
forest_plot_metrics_class$managed_class <- as.factor(forest_plot_metrics_class$managed_class.x)

forest_plots_three_management_classes <- st_read("~/ahncanopygaps/InputData/forest_plots_three_management_classes.gpkg") 
forest_plot_metrics_three_management_classes <- sf::st_join(forest_metrics_raster_metrics, forest_plots_three_management_classes, st_intersects, largest = T)
forest_plot_metrics_three_management_classes$managed_class <- as.factor(forest_plot_metrics_three_management_classes$managed_class.x)

# Add managed class as attribute to canopy gaps 
new_gaps_plot_code_class <- new_gaps_plot_code %>% 
  dplyr::left_join(sf::st_drop_geometry(forest_plot_metrics_class), by = "COMBI") %>% 
  dplyr::select(ID, managed_class) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::right_join(new_gaps_all_metrics, by = "ID")
managed_gaps <- new_gaps_plot_code_class[which(new_gaps_plot_code_class$managed_class == "managed"),]
unmanaged_gaps <- new_gaps_plot_code_class[which(new_gaps_plot_code_class$managed_class == "unmanaged"),]

# Add new managed class as attribute to canopy gaps 
new_gaps_plot_code_three_management_classes <- new_gaps_plot_code %>% 
  dplyr::left_join(sf::st_drop_geometry(forest_plot_metrics_three_management_classes), by = "COMBI") %>% 
  dplyr::select(ID, managed_class) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::right_join(new_gaps_all_metrics, by = "ID")
managed_gaps_three_management_classes <- new_gaps_plot_code_three_management_classes[which(new_gaps_plot_code_three_management_classes$managed_class == "managed"),]
unmanaged_gaps_three_management_classes <- new_gaps_plot_code_three_management_classes[which(new_gaps_plot_code_three_management_classes$managed_class == "unmanaged"),]
pseudo_unmanaged_gaps_three_management_classes <- new_gaps_plot_code_three_management_classes[which(new_gaps_plot_code_three_management_classes$managed_class == "pseudo_unmanaged"),]
classified_canopy_gaps <- classified_canopy_gaps %>% 
  dplyr::rename(ntree_class = class)
new_gaps_plot_code_three_management_classes <- new_gaps_plot_code_three_management_classes %>% 
  dplyr::left_join(classified_canopy_gaps, by = "ID")

forest_metrics_raster_metrics_new <- forest_metrics_raster_metrics %>% 
  dplyr::mutate(ForestPlot_germination_year = as.numeric(forest_plots_metrics_and_class$KIEMJAAR),
                ForestPlot_age = ifelse(2023 - as.numeric(forest_plots_metrics_and_class$KIEMJAAR) != 2023,
                                        2023 - as.numeric(forest_plots_metrics_and_class$KIEMJAAR),
                                        0),
                ForestPlot_tree_species = forest_plots_metrics_and_class$BOOMSOORT,
                managed_class = forest_plot_metrics_class$managed_class) %>% 
  dplyr::select(COMBI, ForestPlot_germination_year, ForestPlot_age, ForestPlot_tree_species, forest_plot_area)

forest_plot_metrics_three_management_classes_new <- forest_plot_metrics_three_management_classes %>%
  dplyr::mutate(ForestPlot_germination_year = as.numeric(forest_plots_metrics_and_class$KIEMJAAR),
                ForestPlot_age = ifelse(2023 - as.numeric(forest_plots_metrics_and_class$KIEMJAAR) != 2023,
                                        2023 - as.numeric(forest_plots_metrics_and_class$KIEMJAAR),
                                        0),
                ForestPlot_tree_species = forest_plots_metrics_and_class$BOOMSOORT) %>% 
  dplyr::select(-managed_class.x, -managed_class.y) %>% 
  dplyr::left_join(sf::st_drop_geometry(forest_metrics_raster_metrics_new)) %>% 
  dplyr::rename(ForestPlot_area = forest_plot_area)

# IMPORTANT FOR VISUALIZATIONS
forest_metrics_raster_metrics <- forest_plot_metrics_three_management_classes_new

new_gaps_forestplotmetrics_new <- new_gaps_plot_code %>% 
  dplyr::select(ID, COMBI, forest_plot_area) %>% 
  dplyr::left_join(sf::st_drop_geometry(forest_metrics_raster_metrics_new)) %>% 
  dplyr::rename(ForestPlot_ID = COMBI,
                ForestPlot_area = forest_plot_area)

new_gaps_forestplotmetrics_area <- new_gaps_forestplotmetrics_new %>% 
  dplyr::select(ForestPlot_ID, ForestPlot_area)

new_gaps_complete <- new_gaps_plot_code_three_management_classes %>% 
  dplyr::left_join(sf::st_drop_geometry(classified_canopy_gaps), by = "ID") %>% 
  dplyr::select(ID, managed_class, class) %>% 
  dplyr::rename(ntree_class = class) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::right_join(new_gaps_all_metrics, by = "ID") %>% 
  sf::st_drop_geometry() %>% 
  dplyr::left_join(sf::st_drop_geometry(new_gaps_forestplotmetrics_new)) %>% 
  dplyr::select(-geom)

new_gaps_complete_sf <- new_gaps_plot_code_three_management_classes %>% 
  dplyr::left_join(sf::st_drop_geometry(classified_canopy_gaps), by = "ID") %>% 
  dplyr::select(ID, managed_class, class) %>% 
  dplyr::rename(ntree_class = class) %>% 
  dplyr::right_join(new_gaps_all_metrics, by = "ID") %>% 
  dplyr::left_join(sf::st_drop_geometry(new_gaps_forestplotmetrics_new)) %>% 
  st_as_sf()

# Write forest_metrics_raster_metrics tibble to csv
write_csv(sf::st_drop_geometry(forest_metrics_raster_metrics), "~/ahncanopygaps/InputData/forest_metrics_raster_metrics.csv")

# Write forest_metrics_raster_metrics sf to gpkg
st_write(forest_metrics_raster_metrics, "~/ahncanopygaps/InputData/forest_metrics_raster_metrics.gpkg", append=FALSE)

# Write new_gaps_complete tibble to csv
write_csv(new_gaps_complete, "~/ahncanopygaps/InputData/new_gaps_complete.csv")

# Write new_gaps_complete_sf sf to gpkg
st_write(new_gaps_complete_sf, "~/ahncanopygaps/InputData/new_gaps_complete.gpkg", append=FALSE)


## Create histograms for the 4 most important metrics to distinguish the classes
## managed, unmanaged and pseudo-unmanaged)

# Frequencies of canopy gap ForestPlot_gini_CHM4
mu <- new_gaps_plot_code_three_management_classes_na_replaced %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_gini_CHM4, na.rm=TRUE))
ggplot(new_gaps_plot_code_three_management_classes_na_replaced, aes(x=ForestPlot_gini_CHM4, fill = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 alpha = 0.5, 
                 position="identity",
                 binwidth = 0.01) +
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("Gini coefficient")+
  ggtitle("Canopy gap ForestPlot_gini_CHM4")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap ForestPlot_fraq_NoG
mu <- new_gaps_plot_code_three_management_classes_na_replaced %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_fraq_NoG, na.rm=TRUE))
ggplot(new_gaps_plot_code_three_management_classes_na_replaced, aes(x=ForestPlot_fraq_NoG, fill = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 alpha = 0.5, 
                 position="identity",
                 binwidth = 0.01) +
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("fraction")+
  ggtitle("Canopy gap ForestPlot_fraq_NoG")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap ForestPlot_fraq_NGCM
mu <- new_gaps_plot_code_three_management_classes_na_replaced %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_fraq_NGCM, na.rm=TRUE))
ggplot(new_gaps_plot_code_three_management_classes_na_replaced, aes(x=ForestPlot_fraq_NGCM, fill = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 alpha = 0.5, 
                 position="identity",
                 binwidth = 0.005) +
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("fraction")+
  ggtitle("Canopy gap ForestPlot_fraq_NGCM")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap ForestPlot_CE
mu <- new_gaps_plot_code_three_management_classes_na_replaced %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_CE, na.rm=TRUE))
ggplot(new_gaps_plot_code_three_management_classes_na_replaced, aes(x=ForestPlot_CE, fill = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 alpha = 0.5, 
                 position="identity",
                 binwidth = 0.05) +
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("canopy edge")+
  ggtitle("Canopy gap ForestPlot_CE")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap shape_area
mu <- new_gaps_plot_code_three_management_classes_na_replaced %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(shape_area, na.rm=TRUE))
ggplot(new_gaps_plot_code_three_management_classes_na_replaced, aes(x=shape_area, fill = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 alpha = 0.5, 
                 position="identity",
                 binwidth = 5) +
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlim(0,500)+
  xlab("area [m2]")+
  ggtitle("Canopy gap shape_area")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap shape_perimeter
mu <- new_gaps_plot_code_three_management_classes_na_replaced %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(shape_perimeter, na.rm=TRUE))
ggplot(new_gaps_plot_code_three_management_classes_na_replaced, aes(x=shape_perimeter, fill = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 alpha = 0.5, 
                 position="identity",
                 binwidth = 5) +
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlim(0,500)+
  xlab("perimeter [m]")+
  ggtitle("Canopy gap shape_perimeter")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap dist_nn
mu <- new_gaps_plot_code_three_management_classes_na_replaced %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(dist_nn, na.rm=TRUE))
ggplot(new_gaps_plot_code_three_management_classes_na_replaced, aes(x=dist_nn, fill = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 alpha = 0.5, 
                 position="identity",
                 binwidth = 1) +
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("distance [m]")+
  ggtitle("Canopy gap dist_nn")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())


## Create histograms for the 4 most important metrics to distinguish the classes
## part of tree, one tree and group of trees)

# Frequencies of canopy gap shape_AHN3_planarity
mu <- metrics_canopy_gaps_na_replaced %>% 
  group_by(class) %>%
  dplyr::summarize(mu = mean(shape_AHN3_planarity, na.rm=TRUE))
ggplot(metrics_canopy_gaps_na_replaced, aes(x=shape_AHN3_planarity, fill = class)) +
  geom_histogram(aes(y = after_stat(density)),
                 alpha = 0.5, 
                 position="identity",
                 binwidth = 0.01) +
  geom_vline(data = mu, aes(xintercept = mu,
                            col = class),
             linetype="dashed")+
  xlab("planarity")+
  ggtitle("Canopy gap shape_AHN3_planarity")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap shape_AHN3_linearity
mu <- metrics_canopy_gaps_na_replaced %>% 
  group_by(class) %>%
  dplyr::summarize(mu = mean(shape_AHN3_linearity, na.rm=TRUE))
ggplot(metrics_canopy_gaps_na_replaced, aes(x=shape_AHN3_linearity, fill = class)) +
  geom_histogram(aes(y = after_stat(density)),
                 alpha = 0.5, 
                 position="identity",
                 binwidth = 0.01) +
  geom_vline(data = mu, aes(xintercept = mu,
                            col = class),
             linetype="dashed")+
  xlab("linearity")+
  ggtitle("Canopy gap shape_AHN3_linearity")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap ForestPlot_fraq_NGCM
mu <- metrics_canopy_gaps_na_replaced %>% 
  group_by(class) %>%
  dplyr::summarize(mu = mean(ForestPlot_fraq_NGCM, na.rm=TRUE))
ggplot(metrics_canopy_gaps_na_replaced, aes(x=ForestPlot_fraq_NGCM, fill = class)) +
  geom_histogram(aes(y = after_stat(density)),
                 alpha = 0.5, 
                 position="identity",
                 binwidth = 0.005) +
  geom_vline(data = mu, aes(xintercept = mu,
                            col = class),
             linetype="dashed")+
  xlab("fraction")+
  ggtitle("Canopy gap ForestPlot_fraq_NGCM")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap ForestPlot_GD
mu <- metrics_canopy_gaps_na_replaced %>% 
  group_by(class) %>%
  dplyr::summarize(mu = mean(ForestPlot_GD, na.rm=TRUE))
ggplot(metrics_canopy_gaps_na_replaced, aes(x=ForestPlot_GD, fill = class)) +
  geom_histogram(aes(y = after_stat(density)),
                 alpha = 0.5, 
                 position="identity",
                 binwidth = 0.0001) +
  geom_vline(data = mu, aes(xintercept = mu,
                            col = class),
             linetype="dashed")+
  xlab("fraction")+
  ggtitle("Canopy gap ForestPlot_GD")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap shape_area
mu <- metrics_canopy_gaps_na_replaced %>% 
  group_by(class) %>%
  dplyr::summarize(mu = mean(shape_area, na.rm=TRUE))
ggplot(metrics_canopy_gaps_na_replaced, aes(x=shape_area, fill = class)) +
  geom_histogram(aes(y = after_stat(density)),
                 alpha = 0.5, 
                 position="identity",
                 binwidth = 5) +
  geom_vline(data = mu, aes(xintercept = mu,
                            col = class),
             linetype="dashed")+
  xlim(0,500)+
  xlab("area [m2]")+
  ggtitle("Canopy gap shape_area")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap shape_perimeter
mu <- metrics_canopy_gaps_na_replaced %>% 
  group_by(class) %>%
  dplyr::summarize(mu = mean(shape_perimeter, na.rm=TRUE))
ggplot(metrics_canopy_gaps_na_replaced, aes(x=shape_perimeter, fill = class)) +
  geom_histogram(aes(y = after_stat(density)),
                 alpha = 0.5, 
                 position="identity",
                 binwidth = 5) +
  geom_vline(data = mu, aes(xintercept = mu,
                            col = class),
             linetype="dashed")+
  xlim(0,500)+
  xlab("perimeter [m]")+
  ggtitle("Canopy gap shape_perimeter")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap dist_nn
mu <- metrics_canopy_gaps_na_replaced %>% 
  group_by(class) %>%
  dplyr::summarize(mu = mean(dist_nn, na.rm=TRUE))
ggplot(metrics_canopy_gaps_na_replaced, aes(x=dist_nn, fill = class)) +
  geom_histogram(aes(y = after_stat(density)),
                 alpha = 0.5, 
                 position="identity",
                 binwidth = 1) +
  geom_vline(data = mu, aes(xintercept = mu,
                            col = class),
             linetype="dashed")+
  xlab("distance [m]")+
  ggtitle("Canopy gap dist_nn")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

## Old histograms

# Frequencies of canopy gap Buffer10m_ipcumzq50_AHN4
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(Buffer10m_ipcumzq50_AHN4, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=Buffer10m_ipcumzq50_AHN4, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 1) +
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("unit")+
  ggtitle("Canopy gap Buffer10m_ipcumzq50_AHN4")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap Buffer10m_ipcumzq30_AHN4
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(Buffer10m_ipcumzq30_AHN4, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=Buffer10m_ipcumzq30_AHN4, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 1) +
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("unit")+
  ggtitle("Canopy gap Buffer10m_ipcumzq30_AHN4")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap Buffer10m_ipcumzq70_AHN3
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(Buffer10m_ipcumzq70_AHN3, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=Buffer10m_ipcumzq70_AHN3, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.5) +
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("unit")+
  ggtitle("Canopy gap Buffer10m_ipcumzq70_AHN3")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap Buffer5m_isd_AHN4
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(Buffer5m_isd_AHN4, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=Buffer5m_isd_AHN4, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 5) +
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("unit")+
  ggtitle("Canopy gap Buffer5m_isd_AHN4")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap Buffer5m_ipcumzq70_AHN4
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(Buffer5m_ipcumzq70_AHN4, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=Buffer5m_ipcumzq70_AHN4, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.5) +
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("unit")+
  ggtitle("Canopy gap Buffer5m_ipcumzq70_AHN4")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap Buffer10m_isd_AHN3
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(Buffer10m_isd_AHN3, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=Buffer10m_isd_AHN3, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 1) +
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("unit")+
  ggtitle("Canopy gap Buffer10m_isd_AHN3")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap Buffer10m_isd_AHN4
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(Buffer10m_isd_AHN4, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=Buffer10m_isd_AHN4, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 5) +
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("unit")+
  ggtitle("Canopy gap Buffer10m_isd_AHN4")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap Buffer10m_zpcum7_AHN3
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(Buffer10m_zpcum7_AHN3, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=Buffer10m_zpcum7_AHN3, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 2) +
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlim(0,100)+
  xlab("unit")+
  ggtitle("Canopy gap Buffer10m_zpcum7_AHN3")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap Buffer10m_ipcumzq10_AHN4
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(Buffer10m_ipcumzq10_AHN4, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=Buffer10m_ipcumzq10_AHN4, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.1) +
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("unit")+
  ggtitle("Canopy gap Buffer10m_ipcumzq10_AHN4")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of forest plot fraction new gap both methods (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_fraq_NGBM, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=ForestPlot_fraq_NGBM, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.005)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("fraction")+
  ggtitle("Canopy gap ForestPlot_fraq_NGBM ")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of forest plot fraction new gap CHM-difference method (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_fraq_NGCM, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=ForestPlot_fraq_NGCM, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.005)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("fraction")+
  ggtitle("Canopy gap ForestPlot_fraq_NGCM ")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of forest plot fraction disappeared gap (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_fraq_DG, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=ForestPlot_fraq_DG, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.0025)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("fraction")+
  ggtitle("Canopy gap ForestPlot_fraq_DG ")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of forest plot fraction remaining gap (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_fraq_RG, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=ForestPlot_fraq_RG, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.01)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("fraction")+
  ggtitle("Canopy gap ForestPlot_fraq_RG ")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of forest plot fraction disappeared gap vertical closure (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_fraq_VC, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=ForestPlot_fraq_VC, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.01)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("percentage [%]")+
  ggtitle("Canopy gap ForestPlot_fraq_VC ")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of ForestPlot_GD (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_GD, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=ForestPlot_GD, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.0001)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlim(-0.0002,0.005)+
  xlab("Forest plot gap density")+
  ggtitle("Canopy gap ForestPlot_GD ")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of ForestPlot_gini_CHM4 (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_gini_CHM4, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=ForestPlot_gini_CHM4, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.01)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("Gini coef")+
  ggtitle("Canopy gap ForestPlot_gini_CHM4 ")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of ForestPlot_mean_CHMdiv (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_mean_CHMdiv, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=ForestPlot_mean_CHMdiv, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.5)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("difference [m]")+
  ggtitle("Canopy gap ForestPlot_mean_CHMdiv ")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of ForestPlot_PIG (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_PIG, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=ForestPlot_PIG, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.01)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("forest plot percentage in gap [%]")+
  ggtitle("Canopy gap ForestPlot_PIG ")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of ForestPlot_mean_CHM3 (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_mean_CHM3, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=ForestPlot_mean_CHM3, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 1)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("height [m]")+
  ggtitle("Canopy gap ForestPlot_mean_CHM3 ")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of ForestPlot_fraq_NoG (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_fraq_NoG, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=ForestPlot_fraq_NoG, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.01)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("fraction")+
  ggtitle("Canopy gap ForestPlot_fraq_NoG ")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of forest plot GD (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_GD, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=ForestPlot_GD, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.0001)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("forest plot gap density")+
  ggtitle("Canopy gap ForestPlot_GD")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of ForestPlot_mean_CHM4 (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_mean_CHM4, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=ForestPlot_mean_CHM4, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.5)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlim(10,30)+
  xlab("height [m]")+
  ggtitle("Canopy gap ForestPlot_mean_CHM4")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of ForestPlot_gini_CHM3 (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_gini_CHM3, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=ForestPlot_gini_CHM3, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.01)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlim(0,0.4)+
  xlab("Gini coef")+
  ggtitle("Canopy gap ForestPlot_gini_CHM3")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of ForestPlot_gini_CHMdiv (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_gini_CHMdiv, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=ForestPlot_gini_CHMdiv, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.5)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlim(-15,15)+
  xlab("Gini coef")+
  ggtitle("Canopy gap ForestPlot_gini_CHMdiv")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of ForestPlot_min_CHM4 (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_min_CHM4, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=ForestPlot_min_CHM4, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.0005)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("height [m]")+
  xlim(-0.002,0.03) +
  ggtitle("Canopy gap ForestPlot_min_CHM4")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of ForestPlot_max_CHMdiv (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_max_CHMdiv, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=ForestPlot_max_CHMdiv, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.5)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlim(10,40)+
  xlab("height [m]")+
  ggtitle("Canopy gap ForestPlot_max_CHMdiv")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of ForestPlot_sd_CHM3 (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_sd_CHM3, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=ForestPlot_sd_CHM3, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.5)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlim(0,15)+
  xlab("height [m]")+
  ggtitle("Canopy gap ForestPlot_sd_CHM3")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of ipcumzq70_AHN4 (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ipcumzq70_AHN4, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=ipcumzq70_AHN4, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.5)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("unit")+
  ggtitle("Canopy gap ipcumzq70_AHN4")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of ipground_AHN3 (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ipground_AHN3, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=ipground_AHN3, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 2)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("unit")+
  ggtitle("Canopy gap ipground_AHN3")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of ipground_AHN4 (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ipground_AHN4, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=ipground_AHN4, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 2)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("unit")+
  ggtitle("Canopy gap ipground_AHN4")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())


## Other interesting metrics

# Frequencies of canopy gap shape area (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(shape_area, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=shape_area, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 5)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  ylim(0.00,0.035)+
  #xlim(-1,500)+
  xlab("area [m2]")+
  ggtitle("Canopy gap area")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap shape perimeter (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(shape_perimeter, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=shape_perimeter, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 5)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  ylim(0.00,0.0375)+
  xlim(0,500)+
  xlab("perimeter [m]")+
  ggtitle("Canopy gap perimeter")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap zmean_AHN3 (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(zmean_AHN3, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=zmean_AHN3, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 1)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlim(0,30)+
  xlab("height [m]")+
  ggtitle("Canopy gap zmean_AHN3")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap mean_CHM3 (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(mean_CHM3, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=mean_CHM3, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 1)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlim(0,35)+
  xlab("height [m]")+
  ggtitle("Canopy gap mean_CHM3")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap zq95_AHN3 (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(zq95_AHN3, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=zq95_AHN3, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 1)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("height [m]")+
  ggtitle("Canopy gap zq95_AHN3")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap ipcumzq50_AHN3 (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ipcumzq50_AHN3, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=ipcumzq50_AHN3, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 1)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("height [m]")+
  ggtitle("Canopy gap ipcumzq50_AHN3")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap ipcumzq90_AHN3 (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ipcumzq90_AHN3, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=ipcumzq90_AHN3, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.5)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("percentage [%]")+
  ggtitle("Canopy gap ipcumzq90_AHN3")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap ipcumzq70_AHN3 (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ipcumzq70_AHN3, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=ipcumzq70_AHN3, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.5)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("percentage [%]")+
  ggtitle("Canopy gap ipcumzq70_AHN3")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap ForestPlot_mean_CHM3 (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_mean_CHM3, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=ForestPlot_mean_CHM3, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 1)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlim(0,35)+
  xlab("height [m]")+
  ggtitle("Canopy gap ForestPlot_mean_CHM3")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap zmean_AHN4 (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(zmean_AHN4, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=zmean_AHN4, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 1)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlim(0,20)+
  xlab("height [m]")+
  ggtitle("Canopy gap zmean_AHN4")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap zmax_AHN4 (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(zmax_AHN4, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=zmax_AHN4, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 1)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("n")+
  ggtitle("Canopy gap zmax_AHN4")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap mean_CHM4 (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(mean_CHM4, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=mean_CHM4, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 1)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlim(0,30)+
  xlab("height [m]")+
  ggtitle("Canopy gap mean_CHM4")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap max_CHM4 (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(max_CHM4, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=max_CHM4, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 1)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("height [m]")+
  ggtitle("Canopy gap max_CHM4")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap ForestPlot_mean_CHM4 (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_mean_CHM4, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=ForestPlot_mean_CHM4, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 1)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlim(0,35)+
  xlab("height [m]")+
  ggtitle("Canopy gap ForestPlot_mean_CHM4")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap mean_CHMdiv (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(mean_CHMdiv, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=mean_CHMdiv, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 1)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlim(-30,5)+
  xlab("height [m]")+
  ggtitle("Canopy gap mean_CHMdiv")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap ForestPlot_mean_CHMdiv (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_mean_CHMdiv, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=ForestPlot_mean_CHMdiv, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.25)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlim(-7.5,5)+
  xlab("height [m]")+
  ggtitle("Canopy gap ForestPlot_mean_CHMdiv")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap dist_nn (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(dist_nn, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=dist_nn, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 1)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlim(0,40)+
  xlab("distance [m]")+
  ggtitle("Canopy gap dist_nn")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap buffer5m_overlap_relative (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(buffer5m_overlap_relative, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=buffer5m_overlap_relative, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.001)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlim(0,0.2)+
  xlab("overlap [%]")+
  ggtitle("Canopy gap buffer5m_overlap_relative")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap buffer10m_overlap_relative (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(buffer10m_overlap_relative, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=buffer10m_overlap_relative, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.001)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlim(0,0.2)+
  xlab("overlap [%]")+
  ggtitle("Canopy gap buffer10m_overlap_relative")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of canopy gap shape_AHN3_eigen_largest (managed/unmanaged)
mu <- new_gaps_plot_code_class %>% 
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(shape_AHN3_eigen_largest, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=shape_AHN3_eigen_largest, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 10)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlim(-0.1,400)+
  xlab("overlap [%]")+
  ggtitle("Canopy gap shape_AHN3_eigen_largest")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of fraction new gap CHM-difference method (managed/unmanaged)
mu <- new_gaps_plot_code_class %>%
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(fraq_NGCM, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=fraq_NGCM, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.01)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlim(-0.02,1.02)+
  xlab("fraction")+
  ggtitle("Canopy gap fraq_NGCM")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of fraction new gap both methods (managed/unmanaged)
mu <- new_gaps_plot_code_class %>%
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(fraq_NGBM, na.rm=TRUE))
ggplot(new_gaps_plot_code_class, aes(x=fraq_NGBM, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.01)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlim(-0.02,1.02)+
  xlab("fraction")+
  ggtitle("Canopy gap fraq_NGBM")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())


## Histograms of metrics on forest plot level

# Frequencies of percentage forest in gap (managed/unmanaged)
mu <- forest_plots_metrics_and_class %>%
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_PIG, na.rm=TRUE))
ggplot(forest_plots_metrics_and_class, aes(x=ForestPlot_PIG, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.005)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlim(-0.02,0.3)+
  xlab("percentage [%]")+
  ggtitle("Forest plot percentage in gap")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of gap density (managed/unmanaged)
mu <- forest_plots_metrics_and_class %>%
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_GD, na.rm=TRUE))
ggplot(forest_plots_metrics_and_class, aes(x=ForestPlot_GD, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.0001)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlim(-0.0002,0.005)+
  xlab("gap density")+
  ggtitle("Forest plot gap density")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of mean CHM3 (managed/unmanaged)
mu <- forest_plots_metrics_and_class %>%
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_mean_CHM3, na.rm=TRUE))
ggplot(forest_plots_metrics_and_class, aes(x=ForestPlot_mean_CHM3, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 1)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("height [m]")+
  ggtitle("Forest plot mean CHM3")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of mean CHM4 (managed/unmanaged)
mu <- forest_plots_metrics_and_class %>%
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_mean_CHM4, na.rm=TRUE))
ggplot(forest_plots_metrics_and_class, aes(x=ForestPlot_mean_CHM4, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 1)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("height [m]")+
  ggtitle("Forest plot mean CHM4")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of fraction disappeared gap (managed/unmanaged)
mu <- forest_plots_metrics_and_class %>%
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_fraq_DG, na.rm=TRUE))
ggplot(forest_plots_metrics_and_class, aes(x=ForestPlot_fraq_DG, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.0025)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("fraction")+
  ggtitle("Forest plot fraction disappeared gap")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of fraction disappeared gap vertical closure (managed/unmanaged)
mu <- forest_plots_metrics_and_class %>%
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_fraq_VC, na.rm=TRUE))
ggplot(forest_plots_metrics_and_class, aes(x=ForestPlot_fraq_VC, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.01)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("percentage [%]")+
  ggtitle("Forest plot fraction disappeared gap vertical closure")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of fraction no gap (managed/unmanaged)
mu <- forest_plots_metrics_and_class %>%
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_fraq_NoG, na.rm=TRUE))
ggplot(forest_plots_metrics_and_class, aes(x=ForestPlot_fraq_NoG, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.01)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("fraction")+
  ggtitle("Forest plot fraction no gap")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of fraction remaining gap (managed/unmanaged)
mu <- forest_plots_metrics_and_class %>%
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_fraq_RG, na.rm=TRUE))
ggplot(forest_plots_metrics_and_class, aes(x=ForestPlot_fraq_RG, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.01)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("fraction")+
  ggtitle("Forest plot fraction remaining gap")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of fraction new gap both methods (managed/unmanaged)
mu <- forest_plots_metrics_and_class %>%
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_fraq_NGBM, na.rm=TRUE))
ggplot(forest_plots_metrics_and_class, aes(x=ForestPlot_fraq_NGBM, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.005)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("fraction")+
  ggtitle("Forest plot fraction new gap both methods")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Frequencies of fraction new gap CHM-subtraction method (managed/unmanaged)
mu <- forest_plots_metrics_and_class %>%
  group_by(managed_class) %>%
  dplyr::summarize(mu = mean(ForestPlot_fraq_NGCM, na.rm=TRUE))
ggplot(forest_plots_metrics_and_class, aes(x=ForestPlot_fraq_NGCM, color = managed_class)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill="white",position="dodge",
                 binwidth = 0.005)+
  geom_vline(data = mu, aes(xintercept = mu,
                            col = managed_class),
             linetype="dashed")+
  xlab("fraction")+
  ggtitle("Forest plot fraction new gap CHM-subtraction method")+
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# ## Comparison tree classes 'part of tree', 'one tree' and 'group of trees'
# 
# # Check difference in most important varable shape_AHN3_linearity
# mu <- metrics_canopy_gaps %>% 
#   group_by(class) %>%
#   dplyr::summarize(mu = mean(shape_AHN3_linearity, na.rm=TRUE))
# ggplot(metrics_canopy_gaps, aes(x=shape_AHN3_linearity, color = class)) +
#   geom_histogram(aes(y = after_stat(density)),
#                  fill="white",position="dodge",
#                  binwidth = 0.001)+
#   geom_vline(data = mu, aes(xintercept = mu,
#                             col = class),
#              linetype="dashed")+
#   xlab("perimeter [m]")+
#   ggtitle("Density histogram canopy gap shape_AHN3_linearity (comparison)")+
#   labs(color = "")+
#   theme(legend.position="top")
# 
# # Check difference in ForestPlot_GD
# mu <- metrics_canopy_gaps %>% 
#   group_by(class) %>%
#   dplyr::summarize(mu = mean(ForestPlot_GD, na.rm=TRUE))
# ggplot(metrics_canopy_gaps, aes(x=ForestPlot_GD, color = class)) +
#   geom_histogram(aes(y = after_stat(density)),
#                  fill="white",position="dodge",
#                  binwidth = 0.0001)+
#   geom_vline(data = mu, aes(xintercept = mu,
#                             col = class),
#              linetype="dashed")+
#   xlab("perimeter [m]")+
#   ggtitle("Density histogram canopy gap ForestPlot_GD (comparison)")+
#   labs(color = "")+
#   theme(legend.position="top")
# 
# # Check difference in shape_IER
# mu <- metrics_canopy_gaps %>% 
#   group_by(class) %>%
#   dplyr::summarize(mu = mean(shape_IER, na.rm=TRUE))
# ggplot(metrics_canopy_gaps, aes(x=shape_IER, color = class)) +
#   geom_histogram(aes(y = after_stat(density)),
#                  fill="white",position="dodge",
#                  binwidth = 0.1)+
#   geom_vline(data = mu, aes(xintercept = mu,
#                             col = class),
#              linetype="dashed")+
#   xlab("perimeter [m]")+
#   ggtitle("Density histogram canopy gap shape_IER (comparison)")+
#   labs(color = "")+
#   theme(legend.position="top")
# 
# # Check difference in shape_area
# mu <- metrics_canopy_gaps %>% 
#   group_by(class) %>%
#   dplyr::summarize(mu = mean(shape_area, na.rm=TRUE))
# ggplot(metrics_canopy_gaps, aes(x=shape_area, color = class)) +
#   geom_histogram(aes(y = after_stat(density)),
#                  fill="white",position="dodge",
#                  binwidth = 5)+
#   geom_vline(data = mu, aes(xintercept = mu,
#                             col = class),
#              linetype="dashed")+
#   xlab("area [m2]")+
#   ggtitle("Density histogram canopy gap shape_area (comparison)")+
#   labs(color = "")+
#   theme(legend.position="top")


