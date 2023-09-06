# Course: GRS-80436 / MSc Thesis
# Project: AHN canopy gap detection
# Author: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: L.C. Vreugdenhil MSc.
# Year: 2022

### Description ###
# Script to create bar charts to compare differences of different classes of
# the new canopy gap. It is split in different managed class, different
# number of trees class, and per tree species.

# Prepare statistical comparison 
my_comparisons = list( c("managed", "pseudo_unmanaged"), c("pseudo_unmanaged", "unmanaged"), 
                       c("managed", "unmanaged") )
my_comparisons_ntrees = list( c("group of trees", "one tree"), c("one tree", "part of tree"),
                              c("group of trees", "part of tree") )
symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf), symbols = c("****", "***", "**", "*", "ns"))


## Create bar charts of the most important statistics to classify different 
## management classes on canopy gap level 

# Bar chart of ForestPlot_gini_CHM4
mForestPlot_gini_CHM4 <- new_gaps_complete %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    mean_ForestPlot_gini_CHM4 = mean(ForestPlot_gini_CHM4),
    sd_ForestPlot_gini_CHM4   = sd(ForestPlot_gini_CHM4)) 

mForestPlot_gini_CHM4 %>% 
  ggplot(aes(managed_class, mean_ForestPlot_gini_CHM4)) +
  geom_col(aes(fill = managed_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_ForestPlot_gini_CHM4 - sd_ForestPlot_gini_CHM4,
                    ymax = mean_ForestPlot_gini_CHM4 + sd_ForestPlot_gini_CHM4),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "Gini coefficient",
    title = "Canopy gap forest plot Gini coefficient CHM4") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  margin = ggplot2::margin(b = 35)),
        axis.text = element_text(size = 10, color = "#22292F"),
        axis.title = element_text(size = 18, hjust = 1),
        axis.title.y = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        plot.caption = element_text(size = 12, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = ggplot2::margin(t = 15)),
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852"),
        panel.grid.major.y = element_line(color = "#DAE1E7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Bar chart of ForestPlot_fraq_NoG
mForestPlot_fraq_NoG <- new_gaps_complete %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    mean_ForestPlot_fraq_NoG = mean(ForestPlot_fraq_NoG),
    sd_ForestPlot_fraq_NoG   = sd(ForestPlot_fraq_NoG)) 

mForestPlot_fraq_NoG %>% 
  ggplot(aes(managed_class, mean_ForestPlot_fraq_NoG)) +
  geom_col(aes(fill = managed_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_ForestPlot_fraq_NoG - sd_ForestPlot_fraq_NoG,
                    ymax = mean_ForestPlot_fraq_NoG + sd_ForestPlot_fraq_NoG),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "fraction",
    title = "Canopy gap forest plot fraction no gap detected") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  margin = ggplot2::margin(b = 35)),
        axis.text = element_text(size = 10, color = "#22292F"),
        axis.title = element_text(size = 18, hjust = 1),
        axis.title.y = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        plot.caption = element_text(size = 12, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = ggplot2::margin(t = 15)),
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852"),
        panel.grid.major.y = element_line(color = "#DAE1E7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Bar chart of ForestPlot_fraq_NGCM
mForestPlot_fraq_NGCM <- new_gaps_complete %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    mean_ForestPlot_fraq_NGCM = mean(ForestPlot_fraq_NGCM),
    sd_ForestPlot_fraq_NGCM   = sd(ForestPlot_fraq_NGCM)) 

mForestPlot_fraq_NGCM %>% 
  ggplot(aes(managed_class, mean_ForestPlot_fraq_NGCM)) +
  geom_col(aes(fill = managed_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_ForestPlot_fraq_NGCM - sd_ForestPlot_fraq_NGCM,
                    ymax = mean_ForestPlot_fraq_NGCM + sd_ForestPlot_fraq_NGCM),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "fraction",
    title = "Canopy gap forest plot fraction new gap CHM-subtraction method") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 10,
                                  face = "bold",
                                  margin = ggplot2::margin(b = 35)),
        axis.text = element_text(size = 10, color = "#22292F"),
        axis.title = element_text(size = 18, hjust = 1),
        axis.title.y = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        plot.caption = element_text(size = 12, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = ggplot2::margin(t = 15)),
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852"),
        panel.grid.major.y = element_line(color = "#DAE1E7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Bar chart of ForestPlot_CE
mForestPlot_CE <- new_gaps_complete %>% 
  tidyr::replace_na(list(ForestPlot_DI = 0, ForestPlot_CE = 0)) %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    mean_ForestPlot_CE = mean(ForestPlot_CE),
    sd_ForestPlot_CE   = sd(ForestPlot_CE)) 

mForestPlot_CE %>% 
  ggplot(aes(managed_class, mean_ForestPlot_CE)) +
  geom_col(aes(fill = managed_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_ForestPlot_CE - sd_ForestPlot_CE,
                    ymax = mean_ForestPlot_CE + sd_ForestPlot_CE),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "canopy edge",
    title = "Canopy gap forest plot canopy edge") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  margin = ggplot2::margin(b = 35)),
        axis.text = element_text(size = 10, color = "#22292F"),
        axis.title = element_text(size = 18, hjust = 1),
        axis.title.y = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        plot.caption = element_text(size = 12, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = ggplot2::margin(t = 15)),
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852"),
        panel.grid.major.y = element_line(color = "#DAE1E7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())


## Create bar charts of the most interesting statistics to compare different 
## tree number classes on canopy gap level 

# Bar chart of shape_AHN3_planarity
mshape_AHN3_planarity <- new_gaps_complete %>% 
  tidyr::replace_na(list(ForestPlot_DI = 0, shape_AHN3_planarity = 0)) %>% 
  sf::st_drop_geometry() %>% 
  group_by(ntree_class) %>% 
  summarise(
    mean_shape_AHN3_planarity = mean(shape_AHN3_planarity),
    sd_shape_AHN3_planarity   = sd(shape_AHN3_planarity)) 

mshape_AHN3_planarity %>% 
  ggplot(aes(ntree_class, mean_shape_AHN3_planarity)) +
  geom_col(aes(fill = ntree_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_shape_AHN3_planarity - sd_shape_AHN3_planarity,
                    ymax = mean_shape_AHN3_planarity + sd_shape_AHN3_planarity),
                color = "#22292F",
                width = .1) +
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "planarity",
    title = "Canopy gap shape planarity AHN3") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  margin = ggplot2::margin(b = 35)),
        axis.text = element_text(size = 10, color = "#22292F"),
        axis.title = element_text(size = 18, hjust = 1),
        axis.title.y = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        plot.caption = element_text(size = 12, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = ggplot2::margin(t = 15)),
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852"),
        panel.grid.major.y = element_line(color = "#DAE1E7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Bar chart of ForestPlot_fraq_NGCM
mForestPlot_fraq_NGCM <- new_gaps_complete %>% 
  tidyr::replace_na(list(ForestPlot_DI = 0, ForestPlot_fraq_NGCM = 0)) %>% 
  sf::st_drop_geometry() %>% 
  group_by(ntree_class) %>% 
  summarise(
    mean_ForestPlot_fraq_NGCM = mean(ForestPlot_fraq_NGCM),
    sd_ForestPlot_fraq_NGCM   = sd(ForestPlot_fraq_NGCM)) 

mForestPlot_fraq_NGCM %>% 
  ggplot(aes(ntree_class, mean_ForestPlot_fraq_NGCM)) +
  geom_col(aes(fill = ntree_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_ForestPlot_fraq_NGCM - sd_ForestPlot_fraq_NGCM,
                    ymax = mean_ForestPlot_fraq_NGCM + sd_ForestPlot_fraq_NGCM),
                color = "#22292F",
                width = .1) +
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "fraction",
    title = "Canopy gap forest plot fraction new gap CHM-subtraction method") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 10,
                                  face = "bold",
                                  margin = ggplot2::margin(b = 35)),
        axis.text = element_text(size = 10, color = "#22292F"),
        axis.title = element_text(size = 18, hjust = 1),
        axis.title.y = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        plot.caption = element_text(size = 12, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = ggplot2::margin(t = 15)),
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852"),
        panel.grid.major.y = element_line(color = "#DAE1E7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Bar chart of ForestPlot_GD
mForestPlot_GD <- new_gaps_complete %>% 
  tidyr::replace_na(list(ForestPlot_DI = 0, ForestPlot_GD = 0)) %>% 
  sf::st_drop_geometry() %>% 
  group_by(ntree_class) %>% 
  summarise(
    mean_ForestPlot_GD = mean(ForestPlot_GD),
    sd_ForestPlot_GD   = sd(ForestPlot_GD)) 

mForestPlot_GD %>% 
  ggplot(aes(ntree_class, mean_ForestPlot_GD)) +
  geom_col(aes(fill = ntree_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_ForestPlot_GD - sd_ForestPlot_GD,
                    ymax = mean_ForestPlot_GD + sd_ForestPlot_GD),
                color = "#22292F",
                width = .1) +
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "gap density",
    title = "Canopy gap forest plot gap density") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  margin = ggplot2::margin(b = 35)),
        axis.text = element_text(size = 10, color = "#22292F"),
        axis.title = element_text(size = 18, hjust = 1),
        axis.title.y = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        plot.caption = element_text(size = 12, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = ggplot2::margin(t = 15)),
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852"),
        panel.grid.major.y = element_line(color = "#DAE1E7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())


## Create bar charts of the most interesting statistics to compare different 
## management classes on canopy gap level 

# Bar chart of shape_area
mshape_area <- new_gaps_complete %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    mean_shape_area = mean(shape_area),
    sd_shape_area   = sd(shape_area)) 

mshape_area %>% 
  ggplot(aes(managed_class, mean_shape_area)) +
  geom_col(aes(fill = managed_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_shape_area - sd_shape_area,
                    ymax = mean_shape_area + sd_shape_area),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "shape area [m2]",
    title = "Canopy gap shape area") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  margin = ggplot2::margin(b = 35)),
        axis.text = element_text(size = 10, color = "#22292F"),
        axis.title = element_text(size = 18, hjust = 1),
        axis.title.y = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        plot.caption = element_text(size = 12, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = ggplot2::margin(t = 15)),
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852"),
        panel.grid.major.y = element_line(color = "#DAE1E7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Bar chart of shape_perimeter
mshape_perimeter <- new_gaps_complete %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    mean_shape_perimeter = mean(shape_perimeter),
    sd_shape_perimeter   = sd(shape_perimeter)) 

mshape_perimeter %>% 
  ggplot(aes(managed_class, mean_shape_perimeter)) +
  geom_col(aes(fill = managed_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_shape_perimeter - sd_shape_perimeter,
                    ymax = mean_shape_perimeter + sd_shape_perimeter),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "shape perimeter [m]",
    title = "Canopy gap shape perimeter") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  margin = ggplot2::margin(b = 35)),
        axis.text = element_text(size = 10, color = "#22292F"),
        axis.title = element_text(size = 18, hjust = 1),
        axis.title.y = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        plot.caption = element_text(size = 12, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = ggplot2::margin(t = 15)),
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852"),
        panel.grid.major.y = element_line(color = "#DAE1E7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Bar chart of dist_nn
mdist_nn <- new_gaps_complete %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    mean_dist_nn = mean(dist_nn),
    sd_dist_nn   = sd(dist_nn)) 

mdist_nn %>% 
  ggplot(aes(managed_class, mean_dist_nn)) +
  geom_col(aes(fill = managed_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_dist_nn - sd_dist_nn,
                    ymax = mean_dist_nn + sd_dist_nn),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "distance [m]",
    title = "Canopy gap distance to nearest neighbour") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  margin = ggplot2::margin(b = 35)),
        axis.text = element_text(size = 10, color = "#22292F"),
        axis.title = element_text(size = 18, hjust = 1),
        axis.title.y = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        plot.caption = element_text(size = 12, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = ggplot2::margin(t = 15)),
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852"),
        panel.grid.major.y = element_line(color = "#DAE1E7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Bar chart of buffer5m_overlap_relative
mbuffer5m_overlap_relative <- new_gaps_complete %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    mean_buffer5m_overlap_relative = mean(buffer5m_overlap_relative),
    sd_buffer5m_overlap_relative   = sd(buffer5m_overlap_relative)) 

mbuffer5m_overlap_relative %>% 
  ggplot(aes(managed_class, mean_buffer5m_overlap_relative)) +
  geom_col(aes(fill = managed_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_buffer5m_overlap_relative - sd_buffer5m_overlap_relative,
                    ymax = mean_buffer5m_overlap_relative + sd_buffer5m_overlap_relative),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "overlap fraction",
    title = "Canopy gap 5 meter buffer overlap") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  margin = ggplot2::margin(b = 35)),
        axis.text = element_text(size = 10, color = "#22292F"),
        axis.title = element_text(size = 18, hjust = 1),
        axis.title.y = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        plot.caption = element_text(size = 12, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = ggplot2::margin(t = 15)),
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852"),
        panel.grid.major.y = element_line(color = "#DAE1E7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Bar chart of buffer10m_overlap_relative
mbuffer10m_overlap_relative <- new_gaps_complete %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    mean_buffer10m_overlap_relative = mean(buffer10m_overlap_relative),
    sd_buffer10m_overlap_relative   = sd(buffer10m_overlap_relative)) 

mbuffer10m_overlap_relative %>% 
  ggplot(aes(managed_class, mean_buffer10m_overlap_relative)) +
  geom_col(aes(fill = managed_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_buffer10m_overlap_relative - sd_buffer10m_overlap_relative,
                    ymax = mean_buffer10m_overlap_relative + sd_buffer10m_overlap_relative),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "overlap fraction",
    title = "Canopy gap 10 meter buffer overlap") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  margin = ggplot2::margin(b = 35)),
        axis.text = element_text(size = 10, color = "#22292F"),
        axis.title = element_text(size = 18, hjust = 1),
        axis.title.y = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        plot.caption = element_text(size = 12, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = ggplot2::margin(t = 15)),
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852"),
        panel.grid.major.y = element_line(color = "#DAE1E7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Bar chart of mean_CHM3
mmean_CHM3 <- new_gaps_complete %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    mean_mean_CHM3 = mean(mean_CHM3),
    sd_mean_CHM3   = sd(mean_CHM3)) 

mmean_CHM3 %>% 
  ggplot(aes(managed_class, mean_mean_CHM3)) +
  geom_col(aes(fill = managed_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_mean_CHM3 - sd_mean_CHM3,
                    ymax = mean_mean_CHM3 + sd_mean_CHM3),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "height",
    title = "Canopy gap height in CHM3") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  margin = ggplot2::margin(b = 35)),
        axis.text = element_text(size = 10, color = "#22292F"),
        axis.title = element_text(size = 18, hjust = 1),
        axis.title.y = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        plot.caption = element_text(size = 12, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = ggplot2::margin(t = 15)),
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852"),
        panel.grid.major.y = element_line(color = "#DAE1E7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Bar chart of mean_CHM4
mmean_CHM4 <- new_gaps_complete %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    mean_mean_CHM4 = mean(mean_CHM4),
    sd_mean_CHM4   = sd(mean_CHM4)) 

mmean_CHM4 %>% 
  ggplot(aes(managed_class, mean_mean_CHM4)) +
  geom_col(aes(fill = managed_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_mean_CHM4 - sd_mean_CHM4,
                    ymax = mean_mean_CHM4 + sd_mean_CHM4),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "height",
    title = "Canopy gap height in CHM4") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  margin = ggplot2::margin(b = 35)),
        axis.text = element_text(size = 10, color = "#22292F"),
        axis.title = element_text(size = 18, hjust = 1),
        axis.title.y = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        plot.caption = element_text(size = 12, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = ggplot2::margin(t = 15)),
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852"),
        panel.grid.major.y = element_line(color = "#DAE1E7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Bar chart of mean_CHMdiv
mmean_CHMdiv <- new_gaps_complete %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    mean_mean_CHMdiv = mean(mean_CHMdiv),
    sd_mean_CHMdiv   = sd(mean_CHMdiv)) 

mmean_CHMdiv %>% 
  ggplot(aes(managed_class, mean_mean_CHMdiv)) +
  geom_col(aes(fill = managed_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_mean_CHMdiv - sd_mean_CHMdiv,
                    ymax = mean_mean_CHMdiv + sd_mean_CHMdiv),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "height",
    title = "Canopy gap height difference between CHM3 & CHM4") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  margin = ggplot2::margin(b = 35)),
        axis.text = element_text(size = 10, color = "#22292F"),
        axis.title = element_text(size = 18, hjust = 1),
        axis.title.y = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        plot.caption = element_text(size = 12, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = ggplot2::margin(t = 15)),
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852"),
        panel.grid.major.y = element_line(color = "#DAE1E7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Bar chart of fraq_NGCM
mfraq_NGCM <- new_gaps_complete %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    mean_fraq_NGCM = mean(fraq_NGCM),
    sd_fraq_NGCM   = sd(fraq_NGCM)) 

mfraq_NGCM %>% 
  ggplot(aes(managed_class, mean_fraq_NGCM)) +
  geom_col(aes(fill = managed_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_fraq_NGCM - sd_fraq_NGCM,
                    ymax = mean_fraq_NGCM + sd_fraq_NGCM),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "fraction",
    title = "Canopy gap fraction new gap CHM-subtraction method") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 10,
                                  face = "bold",
                                  margin = ggplot2::margin(b = 35)),
        axis.text = element_text(size = 10, color = "#22292F"),
        axis.title = element_text(size = 18, hjust = 1),
        axis.title.y = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        plot.caption = element_text(size = 12, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = ggplot2::margin(t = 15)),
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852"),
        panel.grid.major.y = element_line(color = "#DAE1E7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Bar chart of fraq_NGBM
mfraq_NGBM <- new_gaps_complete %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    mean_fraq_NGBM = mean(fraq_NGBM),
    sd_fraq_NGBM   = sd(fraq_NGBM)) 

mfraq_NGBM %>% 
  ggplot(aes(managed_class, mean_fraq_NGBM)) +
  geom_col(aes(fill = managed_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_fraq_NGBM - sd_fraq_NGBM,
                    ymax = mean_fraq_NGBM + sd_fraq_NGBM),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "fraction",
    title = "Canopy gap fraction new gap both methods") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  margin = ggplot2::margin(b = 35)),
        axis.text = element_text(size = 10, color = "#22292F"),
        axis.title = element_text(size = 18, hjust = 1),
        axis.title.y = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        plot.caption = element_text(size = 12, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = ggplot2::margin(t = 15)),
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852"),
        panel.grid.major.y = element_line(color = "#DAE1E7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())


## Create bar charts of the most interesting statistics to compare different 
## number of tree classes on canopy gap level 

# Bar chart of shape_area
mshape_area <- new_gaps_complete %>% 
  tidyr::replace_na(list(ForestPlot_DI = 0, shape_area = 0)) %>% 
  sf::st_drop_geometry() %>% 
  group_by(ntree_class) %>% 
  summarise(
    mean_shape_area = mean(shape_area),
    sd_shape_area   = sd(shape_area)) 

mshape_area %>% 
  ggplot(aes(ntree_class, mean_shape_area)) +
  geom_col(aes(fill = ntree_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_shape_area - sd_shape_area,
                    ymax = mean_shape_area + sd_shape_area),
                color = "#22292F",
                width = .1) +
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "shape area [m2]",
    title = "Canopy gap shape area") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  margin = ggplot2::margin(b = 35)),
        axis.text = element_text(size = 10, color = "#22292F"),
        axis.title = element_text(size = 18, hjust = 1),
        axis.title.y = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        plot.caption = element_text(size = 12, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = ggplot2::margin(t = 15)),
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852"),
        panel.grid.major.y = element_line(color = "#DAE1E7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Bar chart of shape_perimeter
mshape_perimeter <- new_gaps_complete %>% 
  tidyr::replace_na(list(ForestPlot_DI = 0, shape_perimeter = 0)) %>% 
  sf::st_drop_geometry() %>% 
  group_by(ntree_class) %>% 
  summarise(
    mean_shape_perimeter = mean(shape_perimeter),
    sd_shape_perimeter   = sd(shape_perimeter)) 

mshape_perimeter %>% 
  ggplot(aes(ntree_class, mean_shape_perimeter)) +
  geom_col(aes(fill = ntree_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_shape_perimeter - sd_shape_perimeter,
                    ymax = mean_shape_perimeter + sd_shape_perimeter),
                color = "#22292F",
                width = .1) +
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "shape perimeter [m]",
    title = "Canopy gap shape perimeter") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  margin = ggplot2::margin(b = 35)),
        axis.text = element_text(size = 10, color = "#22292F"),
        axis.title = element_text(size = 18, hjust = 1),
        axis.title.y = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        plot.caption = element_text(size = 12, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = ggplot2::margin(t = 15)),
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852"),
        panel.grid.major.y = element_line(color = "#DAE1E7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Bar chart of dist_nn
mdist_nn <- new_gaps_complete %>% 
  tidyr::replace_na(list(ForestPlot_DI = 0, dist_nn = 0)) %>% 
  sf::st_drop_geometry() %>% 
  group_by(ntree_class) %>% 
  summarise(
    mean_dist_nn = mean(dist_nn),
    sd_dist_nn   = sd(dist_nn)) 

mdist_nn %>% 
  ggplot(aes(ntree_class, mean_dist_nn)) +
  geom_col(aes(fill = ntree_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_dist_nn - sd_dist_nn,
                    ymax = mean_dist_nn + sd_dist_nn),
                color = "#22292F",
                width = .1) +
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "distance [m]",
    title = "Canopy gap distance to nearest neighbour") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  margin = ggplot2::margin(b = 35)),
        axis.text = element_text(size = 10, color = "#22292F"),
        axis.title = element_text(size = 18, hjust = 1),
        axis.title.y = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        plot.caption = element_text(size = 12, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = ggplot2::margin(t = 15)),
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852"),
        panel.grid.major.y = element_line(color = "#DAE1E7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Bar chart of buffer5m_overlap_relative
mbuffer5m_overlap_relative <- new_gaps_complete %>% 
  sf::st_drop_geometry() %>% 
  group_by(ntree_class) %>% 
  summarise(
    mean_buffer5m_overlap_relative = mean(buffer5m_overlap_relative),
    sd_buffer5m_overlap_relative   = sd(buffer5m_overlap_relative)) 

mbuffer5m_overlap_relative %>% 
  ggplot(aes(ntree_class, mean_buffer5m_overlap_relative)) +
  geom_col(aes(fill = ntree_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_buffer5m_overlap_relative - sd_buffer5m_overlap_relative,
                    ymax = mean_buffer5m_overlap_relative + sd_buffer5m_overlap_relative),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "overlap fraction",
    title = "Canopy gap 5 meter buffer overlap") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  margin = ggplot2::margin(b = 35)),
        axis.text = element_text(size = 10, color = "#22292F"),
        axis.title = element_text(size = 18, hjust = 1),
        axis.title.y = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        plot.caption = element_text(size = 12, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = ggplot2::margin(t = 15)),
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852"),
        panel.grid.major.y = element_line(color = "#DAE1E7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Bar chart of buffer10m_overlap_relative
mbuffer10m_overlap_relative <- new_gaps_complete %>% 
  sf::st_drop_geometry() %>% 
  group_by(ntree_class) %>% 
  summarise(
    mean_buffer10m_overlap_relative = mean(buffer10m_overlap_relative),
    sd_buffer10m_overlap_relative   = sd(buffer10m_overlap_relative)) 

mbuffer10m_overlap_relative %>% 
  ggplot(aes(ntree_class, mean_buffer10m_overlap_relative)) +
  geom_col(aes(fill = ntree_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_buffer10m_overlap_relative - sd_buffer10m_overlap_relative,
                    ymax = mean_buffer10m_overlap_relative + sd_buffer10m_overlap_relative),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "overlap fraction",
    title = "Canopy gap 10 meter buffer overlap") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  margin = ggplot2::margin(b = 35)),
        axis.text = element_text(size = 10, color = "#22292F"),
        axis.title = element_text(size = 18, hjust = 1),
        axis.title.y = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        plot.caption = element_text(size = 12, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = ggplot2::margin(t = 15)),
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852"),
        panel.grid.major.y = element_line(color = "#DAE1E7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Bar chart of mean_CHM3
mmean_CHM3 <- new_gaps_complete %>% 
  sf::st_drop_geometry() %>% 
  group_by(ntree_class) %>% 
  summarise(
    mean_mean_CHM3 = mean(mean_CHM3),
    sd_mean_CHM3   = sd(mean_CHM3)) 

mmean_CHM3 %>% 
  ggplot(aes(ntree_class, mean_mean_CHM3)) +
  geom_col(aes(fill = ntree_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_mean_CHM3 - sd_mean_CHM3,
                    ymax = mean_mean_CHM3 + sd_mean_CHM3),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "height",
    title = "Canopy gap height in CHM3") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  margin = ggplot2::margin(b = 35)),
        axis.text = element_text(size = 10, color = "#22292F"),
        axis.title = element_text(size = 18, hjust = 1),
        axis.title.y = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        plot.caption = element_text(size = 12, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = ggplot2::margin(t = 15)),
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852"),
        panel.grid.major.y = element_line(color = "#DAE1E7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Bar chart of mean_CHM4
mmean_CHM4 <- new_gaps_complete %>% 
  sf::st_drop_geometry() %>% 
  group_by(ntree_class) %>% 
  summarise(
    mean_mean_CHM4 = mean(mean_CHM4),
    sd_mean_CHM4   = sd(mean_CHM4)) 

mmean_CHM4 %>% 
  ggplot(aes(ntree_class, mean_mean_CHM4)) +
  geom_col(aes(fill = ntree_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_mean_CHM4 - sd_mean_CHM4,
                    ymax = mean_mean_CHM4 + sd_mean_CHM4),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "height",
    title = "Canopy gap height in CHM4") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  margin = ggplot2::margin(b = 35)),
        axis.text = element_text(size = 10, color = "#22292F"),
        axis.title = element_text(size = 18, hjust = 1),
        axis.title.y = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        plot.caption = element_text(size = 12, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = ggplot2::margin(t = 15)),
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852"),
        panel.grid.major.y = element_line(color = "#DAE1E7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Bar chart of mean_CHMdiv
mmean_CHMdiv <- new_gaps_complete %>% 
  sf::st_drop_geometry() %>% 
  group_by(ntree_class) %>% 
  summarise(
    mean_mean_CHMdiv = mean(mean_CHMdiv),
    sd_mean_CHMdiv   = sd(mean_CHMdiv)) 

mmean_CHMdiv %>% 
  ggplot(aes(ntree_class, mean_mean_CHMdiv)) +
  geom_col(aes(fill = ntree_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_mean_CHMdiv - sd_mean_CHMdiv,
                    ymax = mean_mean_CHMdiv + sd_mean_CHMdiv),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "height",
    title = "Canopy gap height difference between CHM3 & CHM4") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  margin = ggplot2::margin(b = 35)),
        axis.text = element_text(size = 10, color = "#22292F"),
        axis.title = element_text(size = 18, hjust = 1),
        axis.title.y = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        plot.caption = element_text(size = 12, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = ggplot2::margin(t = 15)),
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852"),
        panel.grid.major.y = element_line(color = "#DAE1E7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Bar chart of fraq_NGCM
mfraq_NGCM <- new_gaps_complete %>% 
  sf::st_drop_geometry() %>% 
  group_by(ntree_class) %>% 
  summarise(
    mean_fraq_NGCM = mean(fraq_NGCM),
    sd_fraq_NGCM   = sd(fraq_NGCM)) 

mfraq_NGCM %>% 
  ggplot(aes(ntree_class, mean_fraq_NGCM)) +
  geom_col(aes(fill = ntree_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_fraq_NGCM - sd_fraq_NGCM,
                    ymax = mean_fraq_NGCM + sd_fraq_NGCM),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "fraction",
    title = "Canopy gap fraction new gap CHM-subtraction method") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 10,
                                  face = "bold",
                                  margin = ggplot2::margin(b = 35)),
        axis.text = element_text(size = 10, color = "#22292F"),
        axis.title = element_text(size = 18, hjust = 1),
        axis.title.y = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        plot.caption = element_text(size = 12, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = ggplot2::margin(t = 15)),
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852"),
        panel.grid.major.y = element_line(color = "#DAE1E7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Bar chart of fraq_NGBM
mfraq_NGBM <- new_gaps_complete %>% 
  sf::st_drop_geometry() %>% 
  group_by(ntree_class) %>% 
  summarise(
    mean_fraq_NGBM = mean(fraq_NGBM),
    sd_fraq_NGBM   = sd(fraq_NGBM)) 

mfraq_NGBM %>% 
  ggplot(aes(ntree_class, mean_fraq_NGBM)) +
  geom_col(aes(fill = ntree_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_fraq_NGBM - sd_fraq_NGBM,
                    ymax = mean_fraq_NGBM + sd_fraq_NGBM),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "fraction",
    title = "Canopy gap fraction new gap both methods") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  margin = ggplot2::margin(b = 35)),
        axis.text = element_text(size = 10, color = "#22292F"),
        axis.title = element_text(size = 18, hjust = 1),
        axis.title.y = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        plot.caption = element_text(size = 12, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = ggplot2::margin(t = 15)),
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852"),
        panel.grid.major.y = element_line(color = "#DAE1E7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

## Create bar charts of the most interesting statistics to compare different 
## management classes on forest plot level 


## Create bar charts of the most interesting statistics to compare different 
## tree species on canopy gap level 


## Create bar charts of the most interesting statistics to compare different 
## tree species on forest plot level 


## Optional: create bar charts of the most interesting statistics to compare different 
## age classes on canopy gap level 