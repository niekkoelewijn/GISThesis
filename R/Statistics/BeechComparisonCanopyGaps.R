# Course: GRS-80436 / MSc Thesis
# Project: AHN canopy gap detection
# Author: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: L.C. Vreugdenhil MSc.
# Year: 2022

### Description ###
# Script to visualize and statistically compare the variables of the different
# management classes on canopy gap level, for the tree species beech only.

# Prepare statistical comparison 
my_comparisons = list( c("managed", "pseudo_unmanaged"), c("pseudo_unmanaged", "unmanaged"), 
                       c("managed", "unmanaged") )
symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf), symbols = c("****", "***", "**", "*", "ns"))

# Select the canopy gaps that are situated in forest plots with as dominant
# tree species beech
new_gaps_beech <- new_gaps_complete %>% 
  dplyr::filter(ForestPlot_tree_species == "BU")


## Boxplots

# ggbetweenstats boxplot with Kruskal-Wallis group median comparison
ggbetweenstats(
  data = new_gaps_beech,
  x = managed_class,
  y = Buffer10m_ipcumzq70_AHN3,
  type = "nonparametric", 
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap 10 m buffer overlap per tree species",
  xlab = "tree species",
  ylab = "overlap [%]"
)

# Boxplot of canopy gap shape_area
ggplot(new_gaps_beech, 
       aes(managed_class, shape_area, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(2100, 500, 2300),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 2600,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("area [m2]") +
  ggtitle("Canopy gap mean shape area, beech")+
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
ggplot(new_gaps_beech, 
       aes(managed_class, shape_perimeter, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(1000, 250, 1100),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 1250,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("perimeter [m]") +
  ggtitle("Canopy gap mean shape perimeter, beech")+
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
ggplot(new_gaps_beech, 
       aes(managed_class, dist_nn, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(110, 100, 130),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 150,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("distance [m]") +
  ggtitle("Canopy gap mean distance to nearest neighbour, beech")+
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
ggplot(new_gaps_beech, 
       aes(managed_class, buffer5m_overlap_relative, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(0.25, 0.2, 0.3),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 0.4,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("fraction overlap") +
  ggtitle("Canopy gap mean 5 meter buffer overlap, beech")+
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
ggplot(new_gaps_beech, 
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
  ggtitle("Canopy gap mean 10 meter buffer overlap, beech")+
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
ggplot(new_gaps_beech, 
       aes(managed_class, mean_CHM3, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(38, 33, 42),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 48,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("height [m]") +
  ggtitle("Canopy gap mean height in CHM3, beech")+
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
ggplot(new_gaps_beech, 
       aes(managed_class, mean_CHM4, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(33, 29, 38),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 45,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("height [m]") +
  ggtitle("Canopy gap mean height in CHM4, beech")+
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
ggplot(new_gaps_beech, 
       aes(managed_class, mean_CHMdiv, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(0, -2, 5),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 10,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("height [m]") +
  ggtitle("Canopy gap mean height difference between CHM3 & CHM4, beech")+
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

# Boxplot of canopy gap fraq_NGCM
ggplot(new_gaps_beech, 
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
  ggtitle("Canopy gap mean fraction new gap CHM-subtraction method, beech")+
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
ggplot(new_gaps_beech, 
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
  ggtitle("Canopy gap mean fraction new gap both methods, beech")+
  theme(legend.position="none",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 11,
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

# Bar chart of shape_area
mshape_area <- new_gaps_beech %>% 
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
    title = "Canopy gap mean shape area, beech") +
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
mshape_perimeter <- new_gaps_beech %>% 
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
    title = "Canopy gap mean shape perimeter, beech") +
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
mdist_nn <- new_gaps_beech %>% 
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
    title = "Canopy gap mean distance to nearest neighbour, beech") +
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
mbuffer5m_overlap_relative <- new_gaps_beech %>% 
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
    title = "Canopy gap mean 5 meter buffer overlap, beech") +
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
mbuffer10m_overlap_relative <- new_gaps_beech %>% 
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
    title = "Canopy gap mean 10 meter buffer overlap, beech") +
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
mmean_CHM3 <- new_gaps_beech %>% 
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
    y = "height [m]",
    title = "Canopy gap mean height in CHM3, beech") +
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
mmean_CHM4 <- new_gaps_beech %>% 
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
    y = "height [m]",
    title = "Canopy gap mean height in CHM4, beech") +
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
mmean_CHMdiv <- new_gaps_beech %>% 
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
    y = "height [m]",
    title = "Canopy gap mean height difference between CHM3 & CHM4, beech") +
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

# Bar chart of fraq_NGCM
mfraq_NGCM <- new_gaps_beech %>% 
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
    title = "Canopy gap mean fraction new gap CHM-subtraction method, beech") +
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

# Bar chart of fraq_NGBM
mfraq_NGBM <- new_gaps_beech %>% 
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
    title = "Canopy gap mean fraction new gap both methods, beech") +
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

