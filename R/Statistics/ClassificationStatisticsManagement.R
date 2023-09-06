# Course: GRS-80436 / MSc Thesis
# Project: AHN canopy gap detection
# Author: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: L.C. Vreugdenhil MSc.
# Year: 2022

### Description ###
# Script to visualize and statistically compare the variables that are most important
# in the classification of new canopy gaps in management class. 
# (See var_imp_three_management_classes for most important variables)

# Prepare statistical comparison 
my_comparisons = list( c("managed", "pseudo_unmanaged"), c("pseudo_unmanaged", "unmanaged"), 
                       c("managed", "unmanaged") )
symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf), symbols = c("****", "***", "**", "*", "ns"))


## Boxplots

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
  ggtitle("Canopy gap mean forest plot Gini coefficient CHM4")+
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
  ggtitle("Canopy gap mean fraction forest plot no gap detected")+
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
  ggtitle("Canopy gap mean fraction forest plot new gap CHM-subtraction method")+
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
  ggtitle("Canopy gap mean forest plot canopy edge")+
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
    title = "Canopy gap mean forest plot Gini coefficient CHM4") +
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
    title = "Canopy gap mean forest plot fraction no gap detected") +
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
    title = "Canopy gap mean forest plot canopy edge") +
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