# Course: GRS-80436 / MSc Thesis
# Project: AHN canopy gap detection
# Author: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: L.C. Vreugdenhil MSc.
# Year: 2022

### Description ###
# Script to create boxplots to check characteristics of all canopy gaps, and
# split the canopy gaps in managed and unmanaged to compare the differences.

# Prepare statistical comparison 
my_comparisons = list( c("managed", "pseudo_unmanaged"), c("pseudo_unmanaged", "unmanaged"), 
                       c("managed", "unmanaged") )
my_comparisons_ntrees = list( c("group of trees", "one tree"), c("one tree", "part of tree"),
                              c("group of trees", "part of tree") )
symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf), symbols = c("****", "***", "**", "*", "ns"))


## Create boxplots of the most important statistics to classify different 
## management classes on canopy gap level 

# Boxplot of canopy gap ForestPlot_gini_CHM4
ggplot(new_gaps_complete, 
       aes(managed_class, ForestPlot_gini_CHM4, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(0.65, 0.3, 0.7),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 0.8,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("Gini coefficient") +
  ggtitle("Canopy gap forest plot Gini coefficient CHM4")+
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

# Boxplot of canopy gap ForestPlot_fraq_NoG
ggplot(new_gaps_complete, 
       aes(managed_class, ForestPlot_fraq_NoG, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(1, 1.07, 1.15),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 1.3,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("fraction") +
  ggtitle("Canopy gap forest plot fraction no gap detected")+
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

# Boxplot of canopy gap ForestPlot_fraq_NGCM
ggplot(new_gaps_complete, 
       aes(managed_class, ForestPlot_fraq_NGCM, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(0.22, 0.15, 0.25),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 0.3,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("fraction") +
  ggtitle("Canopy gap forest plot fraction new gap CHM-subtraction method")+
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

# Boxplot of canopy gap ForestPlot_CE
ggplot(new_gaps_complete, 
       aes(managed_class, ForestPlot_CE, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(3, 2.7, 3.5),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 4,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("canopy edge") +
  ggtitle("Canopy gap forest plot canopy edge")+
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


## Create boxplots for the 3 most important statistics to distinguish the classes
## part of tree, one tree and group of trees)

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
  ggtitle("Canopy gap shape planarity AHN3")+
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
  ggtitle("Canopy gap forest plot fraction new gap CHM-subtraction method")+
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
  ggtitle("Canopy gap forest plot gap density")+
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


## Create boxplots of the most interesting statistics to compare different 
## management classes on canopy gap level 

# Boxplot of canopy gap shape_area
ggplot(new_gaps_complete, 
       aes(managed_class, shape_area, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(10100, 2000, 10900),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 12000,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("area [m2]") +
  ggtitle("Canopy gap shape area")+
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

# Boxplot of canopy gap shape_perimeter
ggplot(new_gaps_complete, 
       aes(managed_class, shape_perimeter, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(3900, 3000, 4400),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 5000,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("perimeter [m]") +
  ggtitle("Canopy gap shape perimeter")+
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

# Boxplot of canopy gap dist_nn
ggplot(new_gaps_complete, 
       aes(managed_class, dist_nn, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(120, 100, 130),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 150,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("distance [m]") +
  ggtitle("Canopy gap distance to nearest neighbour")+
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

# Boxplot of canopy gap buffer5m_overlap_relative
ggplot(new_gaps_complete, 
       aes(managed_class, buffer5m_overlap_relative, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(0.30, 0.2, 0.33),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 0.4,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("fraction overlap") +
  ggtitle("Canopy gap 5 meter buffer overlap")+
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

# Boxplot of canopy gap buffer10m_overlap_relative
ggplot(new_gaps_complete, 
       aes(managed_class, buffer10m_overlap_relative, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(0.45, 0.25, 0.50),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 0.65,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("fraction overlap") +
  ggtitle("Canopy gap 10 meter buffer overlap ")+
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

# Boxplot of canopy gap mean_CHM3
ggplot(new_gaps_complete, 
       aes(managed_class, mean_CHM3, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(38, 33, 42),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 48,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("height") +
  ggtitle("Canopy gap height in CHM3")+
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

# Boxplot of canopy gap mean_CHM4
ggplot(new_gaps_complete, 
       aes(managed_class, mean_CHM4, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(38, 33, 42),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 48,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("height") +
  ggtitle("Canopy gap height in CHM4")+
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

# Boxplot of canopy gap mean_CHMdiv
ggplot(new_gaps_complete, 
       aes(managed_class, mean_CHMdiv, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(0, -2, 5),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 10,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("height") +
  ggtitle("Canopy gap height difference between CHM3 & CHM4")+
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

# Boxplot of canopy gap fraq_NGCM
ggplot(new_gaps_complete, 
       aes(managed_class, fraq_NGCM, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(1.10, 1.05, 1.25),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 1.40,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("fraction") +
  ggtitle("Canopy gap fraction new gap CHM-subtraction method")+
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

# Boxplot of canopy gap fraq_NGBM
ggplot(new_gaps_complete, 
       aes(managed_class, fraq_NGBM, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(1.10, 1.05, 1.25),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 1.40,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("fraction") +
  ggtitle("Canopy gap fraction new gap both methods")+
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


## Create boxplots of the most interesting statistics to compare different 
## number of tree classes on canopy gap level 

# Boxplot of canopy gap shape_area
ggplot(metrics_canopy_gaps_na_replaced, 
       aes(class, shape_area, fill = class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons_ntrees, 
                     label.y = c(10100, 2500, 11100),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 13000,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("area [m2]") +
  ggtitle("Canopy gap shape area")+
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

# Boxplot of canopy gap shape_perimeter
ggplot(metrics_canopy_gaps_na_replaced, 
       aes(class, shape_perimeter, fill = class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons_ntrees, 
                     label.y = c(4000, 500, 4500),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 5500,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("perimeter [m]") +
  ggtitle("Canopy gap shape perimeter")+
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

# Boxplot of canopy gap dist_nn
ggplot(metrics_canopy_gaps_na_replaced, 
       aes(class, dist_nn, fill = class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons_ntrees, 
                     label.y = c(100, 125, 140),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 170,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("distance [m]") +
  ggtitle("Canopy gap distance to nearest neighbour")+
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

# Boxplot of canopy gap buffer5m_overlap_relative
ggplot(metrics_canopy_gaps_na_replaced, 
       aes(class, buffer5m_overlap_relative, fill = class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons_ntrees, 
                     label.y = c(0.30, 0.35, 0.40),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 0.45,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("fraction overlap") +
  ggtitle("Canopy gap 5 meter buffer overlap")+
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

# Boxplot of canopy gap buffer10m_overlap_relative
ggplot(metrics_canopy_gaps_na_replaced, 
       aes(class, buffer10m_overlap_relative, fill = class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons_ntrees, 
                     label.y = c(0.44, 0.46, 0.52),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 0.60,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("fraction overlap") +
  ggtitle("Canopy gap 10 meter buffer overlap")+
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

# Boxplot of canopy gap mean_CHM3
ggplot(metrics_canopy_gaps_na_replaced, 
       aes(class, mean_CHM3, fill = class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons_ntrees, 
                     label.y = c(42, 37, 46),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 53,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("height") +
  ggtitle("Canopy gap height in CHM3")+
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

# Boxplot of canopy gap mean_CHM4
ggplot(metrics_canopy_gaps_na_replaced, 
       aes(class, mean_CHM4, fill = class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons_ntrees, 
                     label.y = c(38, 33, 42),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 48,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("height") +
  ggtitle("Canopy gap height in CHM4")+
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

# Boxplot of canopy gap mean_CHMdiv
ggplot(metrics_canopy_gaps_na_replaced, 
       aes(class, mean_CHMdiv, fill = class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons_ntrees, 
                     label.y = c(1, -1, 5),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 10,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("height") +
  ggtitle("Canopy gap height difference between CHM3 & CHM4")+
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

# Boxplot of canopy gap fraq_NGCM
ggplot(metrics_canopy_gaps_na_replaced, 
       aes(class, fraq_NGCM, fill = class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons_ntrees, 
                     label.y = c(1.10, 1.05, 1.25),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 1.4,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("fraction") +
  ggtitle("Canopy gap fraction new gap CHM-subtraction method")+
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

# Boxplot of canopy gap fraq_NGBM
ggplot(metrics_canopy_gaps_na_replaced, 
       aes(class, fraq_NGBM, fill = class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons_ntrees, 
                     label.y = c(1.10, 1.05, 1.25),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 1.4,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("fraction") +
  ggtitle("Canopy gap fraction new gap both methods")+
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



## Old boxplots

# Boxplot of canopy Buffer10m_ipcumzq50_AHN4
ggplot(new_gaps_plot_code_class, aes(y = Buffer10m_ipcumzq50_AHN4,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(25, 80) +
  ylab("unit") +
  ggtitle("Canopy gap Buffer10m_ipcumzq50_AHN4")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy Buffer10m_ipcumzq30_AHN4
ggplot(new_gaps_plot_code_class, aes(y = Buffer10m_ipcumzq30_AHN4,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(25, 50) +
  ylab("unit") +
  ggtitle("Canopy gap Buffer10m_ipcumzq30_AHN4")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy Buffer10m_ipcumzq70_AHN3
ggplot(new_gaps_plot_code_class, aes(y = Buffer10m_ipcumzq70_AHN3,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(50, 100) +
  ylab("unit") +
  ggtitle("Canopy gap Buffer10m_ipcumzq70_AHN3")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy Buffer5m_isd_AHN4
ggplot(new_gaps_plot_code_class, aes(y = Buffer5m_isd_AHN4,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(250, 500) +
  ylab("unit") +
  ggtitle("Canopy gap Buffer5m_isd_AHN4")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy Buffer5m_ipcumzq70_AHN4
ggplot(new_gaps_plot_code_class, aes(y = Buffer5m_ipcumzq70_AHN4,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylab("unit") +
  ggtitle("Canopy gap Buffer5m_ipcumzq70_AHN4")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy Buffer10m_isd_AHN3
ggplot(new_gaps_plot_code_class, aes(y = Buffer10m_isd_AHN3,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylab("unit") +
  ggtitle("Canopy gap Buffer10m_isd_AHN3")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy Buffer10m_isd_AHN4
ggplot(new_gaps_plot_code_class, aes(y = Buffer10m_isd_AHN4,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(250, 500) +
  ylab("unit") +
  ggtitle("Canopy gap Buffer10m_isd_AHN4")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy Buffer10m_zpcum7_AHN3
ggplot(new_gaps_plot_code_class, aes(y = Buffer10m_zpcum7_AHN3,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(20, 100) +
  ylab("unit") +
  ggtitle("Canopy gap Buffer10m_zpcum7_AHN3")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy Buffer10m_ipcumzq10_AHN4
ggplot(new_gaps_plot_code_class, aes(y = Buffer10m_ipcumzq10_AHN4,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylab("unit") +
  ggtitle("Canopy gap Buffer10m_ipcumzq10_AHN4")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy ForestPlot_fraq_NGBM
ggplot(new_gaps_plot_code_class, aes(y = ForestPlot_fraq_NGBM,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylab("fraction") +
  ggtitle("Canopy gap ForestPlot_fraq_NGBM")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy ForestPlot_fraq_NGCM
ggplot(new_gaps_plot_code_class, aes(y = ForestPlot_fraq_NGCM,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylab("fraction") +
  ggtitle("Canopy gap ForestPlot_fraq_NGCM")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy ForestPlot_fraq_DG
ggplot(new_gaps_plot_code_class, aes(y = ForestPlot_fraq_DG,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(0, 0.05) +
  ylab("fraction") +
  ggtitle("Canopy gap ForestPlot_fraq_DG")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy ForestPlot_fraq_RG
ggplot(new_gaps_plot_code_class, aes(y = ForestPlot_fraq_RG,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylab("fraction") +
  ggtitle("Canopy gap ForestPlot_fraq_RG")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy ForestPlot_fraq_VC
ggplot(new_gaps_plot_code_class, aes(y = ForestPlot_fraq_VC,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylab("fraction") +
  ggtitle("Canopy gap ForestPlot_fraq_VC")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy ForestPlot_GD
ggplot(new_gaps_plot_code_class, aes(y = ForestPlot_GD,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(0, 0.005) +
  ylab("forest plot gap density") +
  ggtitle("Canopy gap ForestPlot_GD")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy ForestPlot_gini_CHM4
ggplot(new_gaps_plot_code_class, aes(y = ForestPlot_gini_CHM4,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(0, 0.6) +
  ylab("Gini coef") +
  ggtitle("Canopy gap ForestPlot_gini_CHM4")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy ForestPlot_mean_CHMdiv
ggplot(new_gaps_plot_code_class, aes(y = ForestPlot_mean_CHMdiv,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(-5, 1) +
  ylab("Gini coef") +
  ggtitle("Canopy gap ForestPlot_mean_CHMdiv")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy ForestPlot_PIG
ggplot(new_gaps_plot_code_class, aes(y = ForestPlot_PIG,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(0, 0.3) +
  ylab("forest plot percentage in gap") +
  ggtitle("Canopy gap ForestPlot_PIG")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy ForestPlot_mean_CHM3
ggplot(new_gaps_plot_code_class, aes(y = ForestPlot_mean_CHM3,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylab("height [m]") +
  ggtitle("Canopy gap ForestPlot_mean_CHM3")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy ForestPlot_fraq_NoG
ggplot(new_gaps_plot_code_class, aes(y = ForestPlot_fraq_NoG,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylab("fraction") +
  ggtitle("Canopy gap ForestPlot_fraq_NoG")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy ForestPlot_mean_CHM4
ggplot(new_gaps_plot_code_class, aes(y = ForestPlot_mean_CHM4,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylab("height [m]") +
  ggtitle("Canopy gap ForestPlot_mean_CHM4")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy ForestPlot_gini_CHM3
ggplot(new_gaps_plot_code_class, aes(y = ForestPlot_gini_CHM3,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(0,0.5) +
  ylab("Gini coef") +
  ggtitle("Canopy gap ForestPlot_gini_CHM3")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy ForestPlot_gini_CHMdiv
ggplot(new_gaps_plot_code_class, aes(y = ForestPlot_gini_CHMdiv,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(-20,20) +
  ylab("Gini coef") +
  ggtitle("Canopy gap ForestPlot_gini_CHMdiv")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy ForestPlot_min_CHM4
ggplot(new_gaps_plot_code_class, aes(y = ForestPlot_min_CHM4,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(-0.002,0.03) +
  ylab("height [m]") +
  ggtitle("Canopy gap ForestPlot_min_CHM4")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy ForestPlot_max_CHMdiv 
ggplot(new_gaps_plot_code_class, aes(y = ForestPlot_max_CHMdiv,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(10,40) +
  ylab("height [m]") +
  ggtitle("Canopy gap ForestPlot_max_CHMdiv")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy ForestPlot_sd_CHM3 
ggplot(new_gaps_plot_code_class, aes(y = ForestPlot_sd_CHM3,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(0,15) +
  ylab("height [m]") +
  ggtitle("Canopy gap ForestPlot_sd_CHM3")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy ipcumzq70_AHN4 
ggplot(new_gaps_plot_code_class, aes(y = ipcumzq70_AHN4,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(50,100) +
  ylab("height [m]") +
  ggtitle("Canopy gap ipcumzq70_AHN4")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy ipground_AHN3 
ggplot(new_gaps_plot_code_class, aes(y = ipground_AHN3,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(0,100) +
  ylab("height [m]") +
  ggtitle("Canopy gap ipground_AHN3")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())


## Other interesting metrics

# Boxplot of canopy gap shape area
ggplot(new_gaps_plot_code_class, aes(y = shape_area,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  ylim(0, 100) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylab("area [m2]") +
  ggtitle("Canopy gap area")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy gap shape perimeter
ggplot(new_gaps_plot_code_class, aes(y = shape_perimeter,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(0, 100) +
  ylab("perimeter [m]") +
  ggtitle("Canopy gap perimeter")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy gap zmean_AHN3
ggplot(new_gaps_plot_code_class, aes(y = zmean_AHN3,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(0, 30) +
  ylab("height [m]") +
  ggtitle("Canopy gap zmean_AHN3")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy gap mean_CHM3
ggplot(new_gaps_plot_code_class, aes(y = mean_CHM3,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylab("height [m]") +
  ggtitle("Canopy gap mean_CHM3")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy gap ForestPlot_mean_CHM3
ggplot(new_gaps_plot_code_class, aes(y = ForestPlot_mean_CHM3,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(5, 30) +
  ylab("height [m]") +
  ggtitle("Canopy gap ForestPlot_mean_CHM3")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy gap zmean_AHN4
ggplot(new_gaps_plot_code_class, aes(y = zmean_AHN4,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(0, 30) +
  ylab("height [m]") +
  ggtitle("Canopy gap zmean_AHN4")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy gap mean_CHM4
ggplot(new_gaps_plot_code_class, aes(y = mean_CHM4,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(0, 35) +
  ylab("height [m]") +
  ggtitle("Canopy gap mean_CHM4")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy gap max_CHM4
ggplot(new_gaps_plot_code_class, aes(y = max_CHM4,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(0, 35) +
  ylab("height [m]") +
  ggtitle("Canopy gap max_CHM4")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy gap ForestPlot_mean_CHM4
ggplot(new_gaps_plot_code_class, aes(y = ForestPlot_mean_CHM4,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(5, 30) +
  ylab("height [m]") +
  ggtitle("Canopy gap ForestPlot_mean_CHM4")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy gap mean_CHMdiv
ggplot(new_gaps_plot_code_class, aes(y = mean_CHMdiv,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(-30,5) +
  ylab("height [m]") +
  ggtitle("Canopy gap mean_CHMdiv")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy gap ForestPlot_mean_CHMdiv
ggplot(new_gaps_plot_code_class, aes(y = ForestPlot_mean_CHMdiv,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(-5.5,2.55) +
  ylab("height [m]") +
  ggtitle("Canopy gap ForestPlot_mean_CHMdiv")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy gap dist_nn
ggplot(new_gaps_plot_code_class, aes(y = dist_nn,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(0,40) +
  ylab("height [m]") +
  ggtitle("Canopy gap dist_nn")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy gap buffer5m_overlap_relative
ggplot(new_gaps_plot_code_class, aes(y = buffer5m_overlap_relative,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(-0.01,0.2) +
  ylab("height [m]") +
  ggtitle("Canopy gap buffer5m_overlap_relative")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy gap buffer10m_overlap_relative
ggplot(new_gaps_plot_code_class, aes(y = buffer10m_overlap_relative,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(-0.01,0.3) +
  ylab("height [m]") +
  ggtitle("Canopy gap buffer10m_overlap_relative")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy gap fraq_NGCM
ggplot(new_gaps_plot_code_class, aes(y = fraq_NGCM,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(-0.02,1.02) +
  ylab("height [m]") +
  ggtitle("Canopy gap fraq_NGCM")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of canopy gap fraq_NGBM
ggplot(new_gaps_plot_code_class, aes(y = fraq_NGBM,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(-0.02,1.02) +
  ylab("height [m]") +
  ggtitle("Canopy gap fraq_NGBM")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())


## Histograms of metrics on forest plot level

# Boxplot of percentage forest in gap (managed/unmanaged)
ggplot(forest_plots_metrics_and_class, aes(y = ForestPlot_PIG,
                                     x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylab("height [m]") +
  ggtitle("percentage [%]")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of gap density (managed/unmanaged)
ggplot(forest_plots_metrics_and_class, aes(y = ForestPlot_GD,
                                           x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylim(-0.0002,0.005) +
  ylab("gap density") +
  ggtitle("Forest plot gap density")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of mean CHM3 (managed/unmanaged)
ggplot(forest_plots_metrics_and_class, aes(y = ForestPlot_mean_CHM3,
                                           x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylab("height [m]") +
  ggtitle("Forest plot mean CHM3")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of mean CHM4 (managed/unmanaged)
ggplot(forest_plots_metrics_and_class, aes(y = ForestPlot_mean_CHM4,
                                           x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylab("height [m]") +
  ggtitle("Forest plot mean CHM4")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of fraction disappeared gap (managed/unmanaged)
ggplot(forest_plots_metrics_and_class, aes(y = ForestPlot_fraq_DG,
                                           x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylab("fraction") +
  ggtitle("Forest plot disappeared gap")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of fraction disappeared gap vertical closure (managed/unmanaged)
ggplot(forest_plots_metrics_and_class, aes(y = ForestPlot_fraq_VC,
                                           x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylab("percentage [%]") +
  ggtitle("Forest plot disappeared gap vertical closure")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of fraction no gap (managed/unmanaged)
ggplot(forest_plots_metrics_and_class, aes(y = ForestPlot_fraq_NoG,
                                           x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylab("fraction") +
  ggtitle("Forest plot no gap")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of fraction remaining gap (managed/unmanaged)
ggplot(forest_plots_metrics_and_class, aes(y = ForestPlot_fraq_RG,
                                           x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylab("fraction") +
  ggtitle("Forest plot remaining gap")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of fraction new gap both methods (managed/unmanaged)
ggplot(forest_plots_metrics_and_class, aes(y = ForestPlot_fraq_NGBM,
                                           x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylab("fraction") +
  ggtitle("Forest plot new gap both methods")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# Boxplot of fraction new gap CHM-subtraction method (managed/unmanaged)
ggplot(forest_plots_metrics_and_class, aes(y = ForestPlot_fraq_NGCM,
                                           x = managed_class)) +
  geom_boxplot(aes(colour = managed_class),
               outlier.shape = NA) +
  geom_jitter(aes(colour = managed_class),
              width = 0.36,
              pch = 1)+
  ylab("fraction") +
  ggtitle("Forest plot new gap CHM-subtraction method")+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        legend.title = element_blank())

# ## Comparison tree classes 'part of tree', 'one tree' and 'group of trees'
#
# # Boxplot ofmetrics_canopy_gaps  canopy shape_area 
# ggplot(metrics_canopy_gaps, aes(y = shape_area,
#                                      x = class)) +
#   geom_boxplot(aes(colour = class),
#                outlier.shape = NA) +
#   geom_jitter(aes(colour = class),
#               width = 0.36,
#               pch = 1)+
#   ylim(0,300) +
#   ylab("height [m]") +
#   ggtitle("shape_area")+
#   theme(legend.position="none",
#         plot.title = element_text(hjust = 0.5),
#         axis.title.x=element_blank(),
#         legend.title = element_blank())
# 
# # Boxplot ofmetrics_canopy_gaps  canopy shape_AHN3_linearity 
# ggplot(metrics_canopy_gaps, aes(y = shape_AHN3_linearity,
#                                 x = class)) +
#   geom_boxplot(aes(colour = class),
#                outlier.shape = NA) +
#   geom_jitter(aes(colour = class),
#               width = 0.36,
#               pch = 1)+
#   ylim(0,1) +
#   ylab("height [m]") +
#   ggtitle("shape_AHN3_linearity")+
#   theme(legend.position="none",
#         plot.title = element_text(hjust = 0.5),
#         axis.title.x=element_blank(),
#         legend.title = element_blank())
# 
# # Boxplot ofmetrics_canopy_gaps  canopy shape_AHN3_planarity 
# ggplot(metrics_canopy_gaps, aes(y = shape_AHN3_planarity,
#                                 x = class)) +
#   geom_boxplot(aes(colour = class),
#                outlier.shape = NA) +
#   geom_jitter(aes(colour = class),
#               width = 0.36,
#               pch = 1)+
#   ylim(0,1) +
#   ylab("height [m]") +
#   ggtitle("shape_AHN3_planarity")+
#   theme(legend.position="none",
#         plot.title = element_text(hjust = 0.5),
#         axis.title.x=element_blank(),
#         legend.title = element_blank())
# 
# # Boxplot ofmetrics_canopy_gaps  canopy shape_AHN3_eigen_medium 
# ggplot(metrics_canopy_gaps, aes(y = shape_AHN3_eigen_medium,
#                                 x = class)) +
#   geom_boxplot(aes(colour = class),
#                outlier.shape = NA) +
#   geom_jitter(aes(colour = class),
#               width = 0.36,
#               pch = 1)+
#   ylim(0,35) +
#   ylab("height [m]") +
#   ggtitle("shape_AHN3_eigen_medium")+
#   theme(legend.position="none",
#         plot.title = element_text(hjust = 0.5),
#         axis.title.x=element_blank(),
#         legend.title = element_blank())
# 
# # Boxplot ofmetrics_canopy_gaps  canopy ForestPlot_GD 
# ggplot(metrics_canopy_gaps, aes(y = ForestPlot_GD,
#                                 x = class)) +
#   geom_boxplot(aes(colour = class),
#                outlier.shape = NA) +
#   geom_jitter(aes(colour = class),
#               width = 0.36,
#               pch = 1)+
#   ylim(0,0.005) +
#   ylab("height [m]") +
#   ggtitle("ForestPlot_GD")+
#   theme(legend.position="none",
#         plot.title = element_text(hjust = 0.5),
#         axis.title.x=element_blank(),
#         legend.title = element_blank())
# 
# # Boxplot ofmetrics_canopy_gaps  canopy zq45_AHN3 
# ggplot(metrics_canopy_gaps, aes(y = zq45_AHN3,
#                                 x = class)) +
#   geom_boxplot(aes(colour = class),
#                outlier.shape = NA) +
#   geom_jitter(aes(colour = class),
#               width = 0.36,
#               pch = 1)+
#   ylim(0,30) +
#   ylab("height [m]") +
#   ggtitle("zq45_AHN3")+
#   theme(legend.position="none",
#         plot.title = element_text(hjust = 0.5),
#         axis.title.x=element_blank(),
#         legend.title = element_blank())


