# Course: GRS-80436 / MSc Thesis
# Project: AHN canopy gap detection
# Author: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: L.C. Vreugdenhil MSc.
# Year: 2022

### Description ###
# Script to visualize and statistically compare the variables of the different
# management classes on forest plot level.

# Prepare statistical comparison 
my_comparisons = list( c("managed", "pseudo_unmanaged"), c("pseudo_unmanaged", "unmanaged"), 
                       c("managed", "unmanaged") )
symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf), symbols = c("****", "***", "**", "*", "ns"))

# Change manged_class attribute of new_canopy_gaps to factor
forest_metrics_raster_metrics <- forest_metrics_raster_metrics %>% 
  dplyr::mutate(managed_class = forcats::as_factor(managed_class)) %>% 
  dplyr::mutate(managed_class = forcats::fct_relevel(managed_class, "managed", "pseudo_unmanaged"))


## Boxplots

percentage in gap, gap density, dispersion index, canopy edge, mean of CHM3, mean of CHM4, mean of difference CHM, Gini coefficient of CHM4, fraction NoG, fraction DG, fraction RG, fraction NGBM, fraction NGCM, and percentage vertical closure of fraction DG were compared.

# ggbetweenstats boxplot with Kruskal-Wallis group median comparison
ggstatsplot::ggbetweenstats(
  data = forest_metrics_raster_metrics,
  x = managed_class,
  y = ForestPlot_PIG,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "s",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Forest plot fraction remaining gap per management class",
  xlab = "management class",
  ylab = "fraction"
)

# Boxplot of canopy gap ForestPlot_PIG
dunnTest(ForestPlot_PIG ~ managed_class,
         data = forest_metrics_raster_metrics,
         method = "bonferroni")

ggplot(forest_metrics_raster_metrics, 
       aes(managed_class, ForestPlot_PIG, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(0.8, 0.1, 0.9),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 1,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("fraction in gap") +
  ggtitle("Forest plot mean fraction in gap")+
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

# Boxplot of canopy gap ForestPlot_GD
dunnTest(ForestPlot_GD ~ managed_class,
         data = forest_metrics_raster_metrics,
         method = "bonferroni")

ggplot(forest_metrics_raster_metrics, 
       aes(managed_class, ForestPlot_GD, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(0.006, 0.002, 0.008),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 0.01,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("gap density") +
  ggtitle("Forest plot mean gap density")+
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

# Boxplot of canopy gap ForestPlot_DI
dunnTest(ForestPlot_DI ~ managed_class,
         data = forest_metrics_raster_metrics,
         method = "bonferroni")

ggplot(forest_metrics_raster_metrics, 
       aes(managed_class, ForestPlot_DI, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(6.5, 2, 7.5),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 8.5,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("dispersion index") +
  ggtitle("Forest plot mean dispersion index")+
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

# Boxplot of canopy gap ForestPlot_CE
dunnTest(ForestPlot_CE ~ managed_class,
         data = forest_metrics_raster_metrics,
         method = "bonferroni")

ggplot(forest_metrics_raster_metrics, 
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
  ggtitle("Forest plot mean canopy edge")+
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

# Boxplot of canopy gap ForestPlot_mean_CHM3
dunnTest(ForestPlot_mean_CHM3 ~ managed_class,
         data = forest_metrics_raster_metrics,
         method = "bonferroni")

ggplot(forest_metrics_raster_metrics, 
       aes(managed_class, ForestPlot_mean_CHM3, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(32, 29, 35),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 40,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylim(0,45) +
  ylab("height [m]") +
  ggtitle("Forest plot mean height in CHM3")+
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

# Boxplot of canopy gap ForestPlot_mean_CHM4
dunnTest(ForestPlot_mean_CHM4 ~ managed_class,
         data = forest_metrics_raster_metrics,
         method = "bonferroni")

ggplot(forest_metrics_raster_metrics, 
       aes(managed_class, ForestPlot_mean_CHM4, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(33, 30, 36),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 42,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylim(0,45) +
  ylab("height [m]") +
  ggtitle("Forest plot mean height in CHM4")+
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

# Boxplot of canopy gap ForestPlot_mean_CHMdiv
dunnTest(ForestPlot_mean_CHMdiv ~ managed_class,
         data = forest_metrics_raster_metrics,
         method = "bonferroni")

ggplot(forest_metrics_raster_metrics, 
       aes(managed_class, ForestPlot_mean_CHMdiv, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(2, 0.5, 3),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 5,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("height [m]") +
  ggtitle("Forest plot mean difference in height between CHM3 & CHM4")+
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

# Boxplot of canopy gap ForestPlot_gini_CHM4
dunnTest(ForestPlot_gini_CHM4 ~ managed_class,
         data = forest_metrics_raster_metrics,
         method = "bonferroni")

ggplot(forest_metrics_raster_metrics, 
       aes(managed_class, ForestPlot_gini_CHM4, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(0.7, 0.2, 0.8),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 1,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("Gini coefficient") +
  ggtitle("Forest plot mean gini coefficient CHM4")+
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

# Boxplot of canopy gap ForestPlot_fraq_NoG
dunnTest(ForestPlot_fraq_NoG ~ managed_class,
         data = forest_metrics_raster_metrics,
         method = "bonferroni")

ggplot(forest_metrics_raster_metrics, 
       aes(managed_class, ForestPlot_fraq_NoG, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(1.1, 1.2, 1.3),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 1.5,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("fraction") +
  ggtitle("Forest plot mean fraction no gap detected")+
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

# Boxplot of canopy gap ForestPlot_fraq_DG
dunnTest(ForestPlot_fraq_DG ~ managed_class,
         data = forest_metrics_raster_metrics,
         method = "bonferroni")

ggplot(forest_metrics_raster_metrics, 
       aes(managed_class, ForestPlot_fraq_DG, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(0.25, 0.05, 0.3),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 0.4,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("fraction") +
  ggtitle("Forest plot mean fraction disappeared gap")+
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

# Boxplot of canopy gap ForestPlot_fraq_RG
dunnTest(ForestPlot_fraq_RG ~ managed_class,
         data = forest_metrics_raster_metrics,
         method = "bonferroni")

ggplot(forest_metrics_raster_metrics, 
       aes(managed_class, ForestPlot_fraq_RG, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(0.75, 0.15, 0.85),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 1,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("fraction") +
  ggtitle("Forest plot mean fraction remaining gap")+
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

# Boxplot of canopy gap ForestPlot_fraq_NGCM
dunnTest(ForestPlot_fraq_NGCM ~ managed_class,
         data = forest_metrics_raster_metrics,
         method = "bonferroni")

ggplot(forest_metrics_raster_metrics, 
       aes(managed_class, ForestPlot_fraq_NGCM, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(0.3, 0.15, 0.4),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 0.5,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("fraction") +
  ggtitle("Forest plot mean fraction new gap CHM-subtraction method")+
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

# Boxplot of canopy gap ForestPlot_fraq_NGBM
dunnTest(ForestPlot_fraq_NGBM ~ managed_class,
         data = forest_metrics_raster_metrics,
         method = "bonferroni")

ggplot(forest_metrics_raster_metrics, 
       aes(managed_class, ForestPlot_fraq_NGBM, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(0.45, 0.15, 0.55),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 0.70,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("fraction") +
  ggtitle("Forest plot mean fraction new gap both methods")+
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

# Boxplot of canopy gap ForestPlot_fraq_VC
dunnTest(ForestPlot_fraq_VC ~ managed_class,
         data = forest_metrics_raster_metrics,
         method = "bonferroni")

ggplot(forest_metrics_raster_metrics, 
       aes(managed_class, ForestPlot_fraq_VC, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(0.65, 0.5, 0.75),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 0.90,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("fraction") +
  ggtitle("Forest plot mean fraction disappeared gaps due to vertical closure")+
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

# Boxplot of canopy gap ForestPlot_age
dunnTest(ForestPlot_age ~ managed_class,
         data = forest_metrics_raster_metrics,
         method = "bonferroni")

ggplot(forest_metrics_raster_metrics, 
       aes(managed_class, ForestPlot_age, fill = managed_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(200, 210, 230),
                     symnum.args = symnum.args)+
  stat_compare_means(label.y = 270,
                     symnum.args = symnum.args) +
  scale_fill_grey(start = 0.3) +
  theme_minimal() +
  ylab("age [y]") +
  ggtitle("Forest plot mean age")+
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

# Bar chart of ForestPlot_PIG
mForestPlot_PIG <- forest_metrics_raster_metrics %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    mean_ForestPlot_PIG = mean(ForestPlot_PIG),
    sd_ForestPlot_PIG   = sd(ForestPlot_PIG)) 

mForestPlot_PIG %>% 
  ggplot(aes(managed_class, mean_ForestPlot_PIG)) +
  geom_col(aes(fill = managed_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_ForestPlot_PIG - sd_ForestPlot_PIG,
                    ymax = mean_ForestPlot_PIG + sd_ForestPlot_PIG),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "fraction in gap",
    title = "Forest plot mean fraction in gap") +
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

# Bar chart of ForestPlot_GD
mForestPlot_GD <- forest_metrics_raster_metrics %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    mean_ForestPlot_GD = mean(ForestPlot_GD),
    sd_ForestPlot_GD   = sd(ForestPlot_GD)) 

mForestPlot_GD %>% 
  ggplot(aes(managed_class, mean_ForestPlot_GD)) +
  geom_col(aes(fill = managed_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_ForestPlot_GD - sd_ForestPlot_GD,
                    ymax = mean_ForestPlot_GD + sd_ForestPlot_GD),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "gap density",
    title = "Forest plot mean gap density") +
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

# Bar chart of ForestPlot_DI
mForestPlot_DI <- forest_metrics_raster_metrics %>% 
  tidyr::replace_na(list(ForestPlot_DI = 0, ForestPlot_CE = 0)) %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    mean_ForestPlot_DI = mean(ForestPlot_DI),
    sd_ForestPlot_DI   = sd(ForestPlot_DI)) 

mForestPlot_DI %>% 
  ggplot(aes(managed_class, mean_ForestPlot_DI)) +
  geom_col(aes(fill = managed_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_ForestPlot_DI - sd_ForestPlot_DI,
                    ymax = mean_ForestPlot_DI + sd_ForestPlot_DI),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "dispersion index",
    title = "Forest plot mean dispersion index") +
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

# Bar chart of ForestPlot_CE
mForestPlot_CE <- forest_metrics_raster_metrics %>% 
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
    title = "Forest plot mean canopy edge") +
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

# Bar chart of ForestPlot_mean_CHM3
mForestPlot_mean_CHM3 <- forest_metrics_raster_metrics %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    mean_ForestPlot_mean_CHM3 = mean(ForestPlot_mean_CHM3),
    sd_ForestPlot_mean_CHM3   = sd(ForestPlot_mean_CHM3)) 

mForestPlot_mean_CHM3 %>% 
  ggplot(aes(managed_class, mean_ForestPlot_mean_CHM3)) +
  geom_col(aes(fill = managed_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_ForestPlot_mean_CHM3 - sd_ForestPlot_mean_CHM3,
                    ymax = mean_ForestPlot_mean_CHM3 + sd_ForestPlot_mean_CHM3),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  ylim(0,30)+
  labs(
    y = "height [m]",
    title = "Forest plot mean height in CHM3") +
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

# Bar chart of ForestPlot_mean_CHM4
mForestPlot_mean_CHM4 <- forest_metrics_raster_metrics %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    mean_ForestPlot_mean_CHM4 = mean(ForestPlot_mean_CHM4),
    sd_ForestPlot_mean_CHM4   = sd(ForestPlot_mean_CHM4)) 

mForestPlot_mean_CHM4 %>% 
  ggplot(aes(managed_class, mean_ForestPlot_mean_CHM4)) +
  geom_col(aes(fill = managed_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_ForestPlot_mean_CHM4 - sd_ForestPlot_mean_CHM4,
                    ymax = mean_ForestPlot_mean_CHM4 + sd_ForestPlot_mean_CHM4),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  ylim(0,30)+
  labs(
    y = "height [m]",
    title = "Forest plot mean height in CHM4") +
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

# Bar chart of ForestPlot_mean_CHMdiv
mForestPlot_mean_CHMdiv <- forest_metrics_raster_metrics %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    mean_ForestPlot_mean_CHMdiv = mean(ForestPlot_mean_CHMdiv),
    sd_ForestPlot_mean_CHMdiv   = sd(ForestPlot_mean_CHMdiv)) 

mForestPlot_mean_CHMdiv %>% 
  ggplot(aes(managed_class, mean_ForestPlot_mean_CHMdiv)) +
  geom_col(aes(fill = managed_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_ForestPlot_mean_CHMdiv - sd_ForestPlot_mean_CHMdiv,
                    ymax = mean_ForestPlot_mean_CHMdiv + sd_ForestPlot_mean_CHMdiv),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "height [m]",
    title = "Forest plot mean difference in height in CHM3 & CHM4") +
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

# Bar chart of ForestPlot_gini_CHM4
mForestPlot_gini_CHM4 <- forest_metrics_raster_metrics %>% 
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
    title = "Forest plot mean Gini coefficient  CHM4") +
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
mForestPlot_fraq_NoG <- forest_metrics_raster_metrics %>% 
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
    title = "Forest plot mean fraction no gap detected") +
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

# Bar chart of ForestPlot_fraq_DG
mForestPlot_fraq_DG <- forest_metrics_raster_metrics %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    mean_ForestPlot_fraq_DG = mean(ForestPlot_fraq_DG),
    sd_ForestPlot_fraq_DG   = sd(ForestPlot_fraq_DG)) 

mForestPlot_fraq_DG %>% 
  ggplot(aes(managed_class, mean_ForestPlot_fraq_DG)) +
  geom_col(aes(fill = managed_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_ForestPlot_fraq_DG - sd_ForestPlot_fraq_DG,
                    ymax = mean_ForestPlot_fraq_DG + sd_ForestPlot_fraq_DG),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "fraction",
    title = "Forest plot mean fraction disappeared gap") +
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

# Bar chart of ForestPlot_fraq_RG
mForestPlot_fraq_RG <- forest_metrics_raster_metrics %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    mean_ForestPlot_fraq_RG = mean(ForestPlot_fraq_RG),
    sd_ForestPlot_fraq_RG   = sd(ForestPlot_fraq_RG)) 

mForestPlot_fraq_RG %>% 
  ggplot(aes(managed_class, mean_ForestPlot_fraq_RG)) +
  geom_col(aes(fill = managed_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_ForestPlot_fraq_RG - sd_ForestPlot_fraq_RG,
                    ymax = mean_ForestPlot_fraq_RG + sd_ForestPlot_fraq_RG),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "fraction",
    title = "Forest plot mean fraction remaining gap") +
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
mForestPlot_fraq_NGCM <- forest_metrics_raster_metrics %>% 
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
    title = "Forest plot mean fraction new gap CHM-subtraction method") +
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

# Bar chart of ForestPlot_fraq_NGBM
mForestPlot_fraq_NGBM <- forest_metrics_raster_metrics %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    mean_ForestPlot_fraq_NGBM = mean(ForestPlot_fraq_NGBM),
    sd_ForestPlot_fraq_NGBM   = sd(ForestPlot_fraq_NGBM)) 

mForestPlot_fraq_NGBM %>% 
  ggplot(aes(managed_class, mean_ForestPlot_fraq_NGBM)) +
  geom_col(aes(fill = managed_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_ForestPlot_fraq_NGBM - sd_ForestPlot_fraq_NGBM,
                    ymax = mean_ForestPlot_fraq_NGBM + sd_ForestPlot_fraq_NGBM),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "fraction",
    title = "Forest plot mean fraction new gap both methods") +
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

# Bar chart of ForestPlot_fraq_VC
mForestPlot_fraq_VC <- forest_metrics_raster_metrics %>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    mean_ForestPlot_fraq_VC = mean(ForestPlot_fraq_VC),
    sd_ForestPlot_fraq_VC   = sd(ForestPlot_fraq_VC)) 

mForestPlot_fraq_VC %>% 
  ggplot(aes(managed_class, mean_ForestPlot_fraq_VC)) +
  geom_col(aes(fill = managed_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_ForestPlot_fraq_VC - sd_ForestPlot_fraq_VC,
                    ymax = mean_ForestPlot_fraq_VC + sd_ForestPlot_fraq_VC),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    y = "fraction",
    title = "Forest plot mean fraction disappeared gaps due to vertical closure") +
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

# Bar chart of ForestPlot_age
mForestPlot_age <- forest_metrics_raster_metrics%>% 
  sf::st_drop_geometry() %>% 
  group_by(managed_class) %>% 
  summarise(
    mean_ForestPlot_age = mean(ForestPlot_age),
    sd_ForestPlot_age   = sd(ForestPlot_age)) 

mForestPlot_age %>% 
  ggplot(aes(managed_class, mean_ForestPlot_age)) +
  geom_col(aes(fill = managed_class), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_ForestPlot_age - sd_ForestPlot_age,
                    ymax = mean_ForestPlot_age + sd_ForestPlot_age),
                color = "#22292F",
                width = .1)+
  scale_fill_grey(start = 0.3) +
  guides(fill = FALSE) +
  theme_minimal() +
  ylim(0,210)+
  labs(
    y = "age [y]",
    title = "Forest plot age") +
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


