# Course: GRS-80436 / MSc Thesis
# Project: AHN canopy gap detection
# Author: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: L.C. Vreugdenhil MSc.
# Year: 2022

### Description ###
# Script to visualize and statistically compare the variables that are most important
# in the classification of new canopy gaps in ntree class. 
# (See var_imp_ntree for most important variables).

# Prepare statistical comparison 
my_comparisons_ntrees = list( c("group of trees", "one tree"), c("one tree", "part of tree"),
                              c("group of trees", "part of tree") )
symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf), symbols = c("****", "***", "**", "*", "ns"))


## Boxplots

# ggbetweenstats boxplot with Kruskal-Wallis group median comparison
ggbetweenstats(
  data = new_gaps_complete,
  x = ntree_class,
  y = shape_AHN3_linearity,
  type = "nonparametric", 
  plot.type = "box",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap AHN3 shape linearity",
  xlab = '"number of trees" class',
  ylab = "linearity"
)

# Boxplot of canopy gap shape_AHN3_planarity
ggplot(metrics_canopy_gaps_na_replaced, 
       aes(class, shape_AHN3_planarity, fill = class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons_ntrees, 
                     label.y = c(0.92, 0.87, 1),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 1.2,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("planarity") +
  ggtitle("Canopy gap mean shape planarity AHN3")+
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

# Boxplot of canopy gap ForestPlot_fraq_NGCM
ggplot(metrics_canopy_gaps_na_replaced, 
       aes(class, ForestPlot_fraq_NGCM, fill = class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons_ntrees, 
                     label.y = c(0.3, 0.25, 0.35),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 0.4,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("fraction") +
  ggtitle("Canopy gap mean forest plot fraction new gap CHM-subtraction method")+
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 9,
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

# Boxplot of canopy gap ForestPlot_GD
ggplot(metrics_canopy_gaps_na_replaced, 
       aes(class, ForestPlot_GD, fill = class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons_ntrees, 
                     label.y = c(0.005, 0.005, 0.0055),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 0.007,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("gap density") +
  ggtitle("Canopy gap mean forest plot gap density")+
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


## Bar charts

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
    title = "Canopy gap mean shape planarity AHN3") +
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
    title = "Canopy gap mean forest plot fraction new gap CHM-subtraction method") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 9,
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
    title = "Canopy gap mean forest plot gap density") +
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
