# Course: GRS-80436 / MSc Thesis
# Project: AHN canopy gap detection
# Author: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: L.C. Vreugdenhil MSc.
# Year: 2022

### Description ###
# Script to visualize and statistically compare the variables of the most important
# different tree species on canopy gap level.

# Filter new_gaps_complete
new_gaps_important_species <- new_gaps_complete %>% 
  dplyr::filter(ForestPlot_tree_species == "BU" | 
                  ForestPlot_tree_species == "EI" | 
                  ForestPlot_tree_species == "DG" | 
                  ForestPlot_tree_species == "FS" | 
                  ForestPlot_tree_species == "GD" | 
                  ForestPlot_tree_species == "JL")%>% 
  dplyr::mutate(ForestPlot_tree_species = forcats::as_factor(ForestPlot_tree_species)) %>% 
  dplyr::mutate(ForestPlot_tree_species = forcats::fct_relevel(ForestPlot_tree_species, "BU", "EI"))

# Translate labels to English: BE: beech, SP: scotch pine, OA: oak, JL: Japanese
# larch, DG: Douglas fir, NS: Norway spruce
levels(new_gaps_important_species$ForestPlot_tree_species) <- c("BE", "OA", "SP",
                                                                "JL", "DG", "NS")

## Boxplots

# ggbetweenstats boxplot with Kruskal-Wallis group median comparison
ggbetweenstats(
  data = new_gaps_important_species,
  x = ForestPlot_tree_species,
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
dunnTest(shape_area ~ ForestPlot_tree_species,
         data = new_gaps_important_species,
         method = "bonferroni")

ggplot(new_gaps_important_species, 
       aes(ForestPlot_tree_species, shape_area, fill = ForestPlot_tree_species)) +
  geom_boxplot() +
  stat_compare_means(label.y = 8000,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  xlab("tree species") +
  ylab("area [m2]") +
  ggtitle("Canopy gap mean shape area")+
  theme(legend.position="none",
        axis.title.x = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
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
dunnTest(shape_perimeter ~ ForestPlot_tree_species,
         data = new_gaps_important_species,
         method = "bonferroni")

ggplot(new_gaps_important_species, 
       aes(ForestPlot_tree_species, shape_perimeter, fill = ForestPlot_tree_species)) +
  geom_boxplot() +
  stat_compare_means(label.y = 3000,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  xlab("tree species") +
  ylab("perimeter [m]") +
  ggtitle("Canopy gap mean shape perimeter")+
  theme(legend.position="none",
        axis.title.x = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
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
dunnTest(dist_nn ~ ForestPlot_tree_species,
         data = new_gaps_important_species,
         method = "bonferroni")

ggplot(new_gaps_important_species, 
       aes(ForestPlot_tree_species, dist_nn, fill = ForestPlot_tree_species)) +
  geom_boxplot() +
  stat_compare_means(label.y = 125,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  xlab("tree species") +
  ylab("distance [m]") +
  ggtitle("Canopy gap mean distance to the nearest neighbour")+
  theme(legend.position="none",
        axis.title.x = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
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
dunnTest(buffer5m_overlap_relative ~ ForestPlot_tree_species,
         data = new_gaps_important_species,
         method = "bonferroni")

ggplot(new_gaps_important_species, 
       aes(ForestPlot_tree_species, buffer5m_overlap_relative, fill = ForestPlot_tree_species)) +
  geom_boxplot() +
  stat_compare_means(label.y = 0.4,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  xlab("tree species") +
  ylab("fraction overlap") +
  ggtitle("Canopy gap mean 5 meter buffer overlap")+
  theme(legend.position="none",
        axis.title.x = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
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
dunnTest(buffer10m_overlap_relative ~ ForestPlot_tree_species,
         data = new_gaps_important_species,
         method = "bonferroni")

ggplot(new_gaps_important_species, 
       aes(ForestPlot_tree_species, buffer10m_overlap_relative, fill = ForestPlot_tree_species)) +
  geom_boxplot() +
  stat_compare_means(label.y = 0.65,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  xlab("tree species") +
  ylab("fraction overlap") +
  ggtitle("Canopy gap mean 10 meter buffer overlap ")+
  theme(legend.position="none",
        axis.title.x = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
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
dunnTest(mean_CHM3 ~ ForestPlot_tree_species,
         data = new_gaps_important_species,
         method = "bonferroni")

ggplot(new_gaps_important_species, 
       aes(ForestPlot_tree_species, mean_CHM3, fill = ForestPlot_tree_species)) +
  geom_boxplot() +
  stat_compare_means(label.y = 48,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  xlab("tree species") +
  ylab("height [m]") +
  ggtitle("Canopy gap mean height in CHM3")+
  theme(legend.position="none",
        legend.title = element_blank(),
        axis.title.x = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
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
dunnTest(mean_CHM4 ~ ForestPlot_tree_species,
         data = new_gaps_important_species,
         method = "bonferroni")

ggplot(new_gaps_important_species, 
       aes(ForestPlot_tree_species, mean_CHM4, fill = ForestPlot_tree_species)) +
  geom_boxplot() +
  stat_compare_means(label.y = 48,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  xlab("tree species") +
  ylab("height [m]") +
  ggtitle("Canopy gap mean height in CHM4")+
  theme(legend.position="none",
        axis.title.x = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
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
dunnTest(mean_CHMdiv ~ ForestPlot_tree_species,
         data = new_gaps_important_species,
         method = "bonferroni")

ggplot(new_gaps_important_species, 
       aes(ForestPlot_tree_species, mean_CHMdiv, fill = ForestPlot_tree_species)) +
  geom_boxplot() +
  stat_compare_means(label.y = 10,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  xlab("tree species") +
  ylab("height [m]") +
  ggtitle("Canopy gap mean height difference between CHM3 & CHM4")+
  theme(legend.position="none",
        axis.title.x = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
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

# Boxplot of canopy gap fraq_NGCM
dunnTest(fraq_NGCM ~ ForestPlot_tree_species,
         data = new_gaps_important_species,
         method = "bonferroni")

ggplot(new_gaps_important_species, 
       aes(ForestPlot_tree_species, fraq_NGCM, fill = ForestPlot_tree_species)) +
  geom_boxplot() +
  stat_compare_means(label.y = 1.40,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  xlab("tree species") +
  ylab("fraction") +
  ggtitle("Canopy gap mean fraction new gap CHM-subtraction method")+
  theme(legend.position="none",
        axis.title.x = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
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

# Boxplot of canopy gap fraq_NGBM
dunnTest(fraq_NGCM ~ ForestPlot_tree_species,
         data = new_gaps_important_species,
         method = "bonferroni")

ggplot(new_gaps_important_species, 
       aes(ForestPlot_tree_species, fraq_NGBM, fill = ForestPlot_tree_species)) +
  geom_boxplot() +
  stat_compare_means(label.y = 1.40,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  xlab("tree species") +
  ylab("fraction") +
  ggtitle("Canopy gap mean fraction new gap both methods")+
  theme(legend.position="none",
        axis.title.x = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
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

# Bar chart of shape_area
mshape_area <- new_gaps_important_species %>% 
  sf::st_drop_geometry() %>% 
  group_by(ForestPlot_tree_species) %>% 
  summarise(
    mean_shape_area = mean(shape_area),
    sd_shape_area   = sd(shape_area)) 

mshape_area %>% 
  ggplot(aes(ForestPlot_tree_species, mean_shape_area)) +
  geom_col(aes(fill = ForestPlot_tree_species), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_shape_area - sd_shape_area,
                    ymax = mean_shape_area + sd_shape_area),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    x = "tree species",
    y = "shape area [m2]",
    title = "Canopy gap mean shape area") +
  theme(legend.position="none",
        axis.title.x = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
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
mshape_perimeter <- new_gaps_important_species %>% 
  sf::st_drop_geometry() %>% 
  group_by(ForestPlot_tree_species) %>% 
  summarise(
    mean_shape_perimeter = mean(shape_perimeter),
    sd_shape_perimeter   = sd(shape_perimeter)) 

mshape_perimeter %>% 
  ggplot(aes(ForestPlot_tree_species, mean_shape_perimeter)) +
  geom_col(aes(fill = ForestPlot_tree_species), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_shape_perimeter - sd_shape_perimeter,
                    ymax = mean_shape_perimeter + sd_shape_perimeter),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    x = "tree species",
    y = "shape perimeter [m]",
    title = "Canopy gap mean shape perimeter") +
  theme(legend.position="none",
        axis.title.x = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
        axis.title.x = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
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
mdist_nn <- new_gaps_important_species %>% 
  sf::st_drop_geometry() %>% 
  group_by(ForestPlot_tree_species) %>% 
  summarise(
    mean_dist_nn = mean(dist_nn),
    sd_dist_nn   = sd(dist_nn)) 

mdist_nn %>% 
  ggplot(aes(ForestPlot_tree_species, mean_dist_nn)) +
  geom_col(aes(fill = ForestPlot_tree_species), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_dist_nn - sd_dist_nn,
                    ymax = mean_dist_nn + sd_dist_nn),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    x = "tree species",
    y = "distance [m]",
    title = "Canopy gap mean distance to nearest neighbour") +
  theme(legend.position="none",
        axis.title.x = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
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
mbuffer5m_overlap_relative <- new_gaps_important_species %>% 
  sf::st_drop_geometry() %>% 
  group_by(ForestPlot_tree_species) %>% 
  summarise(
    mean_buffer5m_overlap_relative = mean(buffer5m_overlap_relative),
    sd_buffer5m_overlap_relative   = sd(buffer5m_overlap_relative)) 

mbuffer5m_overlap_relative %>% 
  ggplot(aes(ForestPlot_tree_species, mean_buffer5m_overlap_relative)) +
  geom_col(aes(fill = ForestPlot_tree_species), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_buffer5m_overlap_relative - sd_buffer5m_overlap_relative,
                    ymax = mean_buffer5m_overlap_relative + sd_buffer5m_overlap_relative),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    x = "tree species",
    y = "overlap fraction",
    title = "Canopy gap mean 5 meter buffer overlap") +
  theme(legend.position="none",
        axis.title.x = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
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
mbuffer10m_overlap_relative <- new_gaps_important_species %>% 
  sf::st_drop_geometry() %>% 
  group_by(ForestPlot_tree_species) %>% 
  summarise(
    mean_buffer10m_overlap_relative = mean(buffer10m_overlap_relative),
    sd_buffer10m_overlap_relative   = sd(buffer10m_overlap_relative)) 

mbuffer10m_overlap_relative %>% 
  ggplot(aes(ForestPlot_tree_species, mean_buffer10m_overlap_relative)) +
  geom_col(aes(fill = ForestPlot_tree_species), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_buffer10m_overlap_relative - sd_buffer10m_overlap_relative,
                    ymax = mean_buffer10m_overlap_relative + sd_buffer10m_overlap_relative),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    x = "tree species",
    y = "overlap fraction",
    title = "Canopy gap mean 10 meter buffer overlap") +
  theme(legend.position="none",
        axis.title.x = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
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
mmean_CHM3 <- new_gaps_important_species %>% 
  sf::st_drop_geometry() %>% 
  group_by(ForestPlot_tree_species) %>% 
  summarise(
    mean_mean_CHM3 = mean(mean_CHM3),
    sd_mean_CHM3   = sd(mean_CHM3)) 

mmean_CHM3 %>% 
  ggplot(aes(ForestPlot_tree_species, mean_mean_CHM3)) +
  geom_col(aes(fill = ForestPlot_tree_species), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_mean_CHM3 - sd_mean_CHM3,
                    ymax = mean_mean_CHM3 + sd_mean_CHM3),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    x = "tree species",
    y = "height [m]",
    title = "Canopy gap mean height in CHM3") +
  theme(legend.position="none",
        axis.title.x = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
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
mmean_CHM4 <- new_gaps_important_species %>% 
  sf::st_drop_geometry() %>% 
  group_by(ForestPlot_tree_species) %>% 
  summarise(
    mean_mean_CHM4 = mean(mean_CHM4),
    sd_mean_CHM4   = sd(mean_CHM4)) 

mmean_CHM4 %>% 
  ggplot(aes(ForestPlot_tree_species, mean_mean_CHM4)) +
  geom_col(aes(fill = ForestPlot_tree_species), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_mean_CHM4 - sd_mean_CHM4,
                    ymax = mean_mean_CHM4 + sd_mean_CHM4),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    x = "tree species",
    y = "height [m]",
    title = "Canopy gap mean height in CHM4") +
  theme(legend.position="none",
        axis.title.x = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
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
mmean_CHMdiv <- new_gaps_important_species %>% 
  sf::st_drop_geometry() %>% 
  group_by(ForestPlot_tree_species) %>% 
  summarise(
    mean_mean_CHMdiv = mean(mean_CHMdiv),
    sd_mean_CHMdiv   = sd(mean_CHMdiv)) 

mmean_CHMdiv %>% 
  ggplot(aes(ForestPlot_tree_species, mean_mean_CHMdiv)) +
  geom_col(aes(fill = ForestPlot_tree_species), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_mean_CHMdiv - sd_mean_CHMdiv,
                    ymax = mean_mean_CHMdiv + sd_mean_CHMdiv),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    x = "tree species",
    y = "height [m]",
    title = "Canopy gap mean height difference between CHM3 & CHM4") +
  theme(legend.position="none",
        axis.title.x = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
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

# Bar chart of fraq_NGCM
mfraq_NGCM <- new_gaps_important_species %>% 
  sf::st_drop_geometry() %>% 
  group_by(ForestPlot_tree_species) %>% 
  summarise(
    mean_fraq_NGCM = mean(fraq_NGCM),
    sd_fraq_NGCM   = sd(fraq_NGCM)) 

mfraq_NGCM %>% 
  ggplot(aes(ForestPlot_tree_species, mean_fraq_NGCM)) +
  geom_col(aes(fill = ForestPlot_tree_species), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_fraq_NGCM - sd_fraq_NGCM,
                    ymax = mean_fraq_NGCM + sd_fraq_NGCM),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    x = "tree species",
    y = "fraction",
    title = "Canopy gap mean fraction new gap CHM-subtraction method") +
  theme(legend.position="none",
        axis.title.x = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
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

# Bar chart of fraq_NGBM
mfraq_NGBM <- new_gaps_important_species %>% 
  sf::st_drop_geometry() %>% 
  group_by(ForestPlot_tree_species) %>% 
  summarise(
    mean_fraq_NGBM = mean(fraq_NGBM),
    sd_fraq_NGBM   = sd(fraq_NGBM)) 

mfraq_NGBM %>% 
  ggplot(aes(ForestPlot_tree_species, mean_fraq_NGBM)) +
  geom_col(aes(fill = ForestPlot_tree_species), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_fraq_NGBM - sd_fraq_NGBM,
                    ymax = mean_fraq_NGBM + sd_fraq_NGBM),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    x = "tree species",
    y = "fraction",
    title = "Canopy gap mean fraction new gap both methods") +
  theme(legend.position="none",
        axis.title.x = element_text(size = 10,
                                    margin = ggplot2::margin(r = 5)),
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

