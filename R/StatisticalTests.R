# Course: GRS-80436 / MSc Thesis
# Project: AHN canopy gap detection
# Author: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: L.C. Vreugdenhil MSc.
# Year: 2022

### Description ###
# In this script the different characteristics of managed and unmanaged will be
# compared using the parametric t-tests or non-parametric Mann-Whitney U tests, 
# depending on the result of the test of Levene


## Test statistical significance for the most important canopy gap metrics
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Comparison canopy gap Buffer10m_ipcumzq50_AHN4 managed vs unmanaged
leveneTest(new_gaps_plot_code_class$Buffer10m_ipcumzq50_AHN4, new_gaps_plot_code_class$managed_class) # p < 2.2e-16
wilcox.test(managed_gaps$Buffer10m_ipcumzq50_AHN4, unmanaged_gaps$Buffer10m_ipcumzq50_AHN4, alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap Buffer10m_ipcumzq30_AHN4 managed vs unmanaged
leveneTest(new_gaps_plot_code_class$Buffer10m_ipcumzq30_AHN4, new_gaps_plot_code_class$managed_class) # p = 5.037e-13
wilcox.test(managed_gaps$Buffer10m_ipcumzq30_AHN4, unmanaged_gaps$Buffer10m_ipcumzq30_AHN4, alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap Buffer10m_ipcumzq70_AHN3 managed vs unmanaged
leveneTest(new_gaps_plot_code_class$Buffer10m_ipcumzq70_AHN3, new_gaps_plot_code_class$managed_class) # p = < 2.2e-16
wilcox.test(managed_gaps$Buffer10m_ipcumzq70_AHN3, unmanaged_gaps$Buffer10m_ipcumzq70_AHN3, alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap Buffer5m_isd_AHN4 managed vs unmanaged
leveneTest(new_gaps_plot_code_class$Buffer5m_isd_AHN4, new_gaps_plot_code_class$managed_class) # p = 0.009112
wilcox.test(managed_gaps$Buffer5m_isd_AHN4, unmanaged_gaps$Buffer5m_isd_AHN4, alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap Buffer5m_ipcumzq70_AHN4 managed vs unmanaged
leveneTest(new_gaps_plot_code_class$Buffer5m_ipcumzq70_AHN4, new_gaps_plot_code_class$managed_class) # < 2.2e-16
wilcox.test(managed_gaps$Buffer5m_ipcumzq70_AHN4, unmanaged_gaps$Buffer5m_ipcumzq70_AHN4, alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap Buffer10m_isd_AHN3 managed vs unmanaged
leveneTest(new_gaps_plot_code_class$Buffer10m_isd_AHN3, new_gaps_plot_code_class$managed_class) # p = 0.05724
t.test(managed_gaps$Buffer10m_isd_AHN3, unmanaged_gaps$Buffer10m_isd_AHN3 , alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap Buffer10m_isd_AHN4 managed vs unmanaged
leveneTest(new_gaps_plot_code_class$Buffer10m_isd_AHN4, new_gaps_plot_code_class$managed_class) # p = 0.2813
t.test(managed_gaps$Buffer10m_isd_AHN4, unmanaged_gaps$Buffer10m_isd_AHN4 , alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap Buffer10m_zpcum7_AHN3 managed vs unmanaged
leveneTest(new_gaps_plot_code_class$Buffer10m_zpcum7_AHN3, new_gaps_plot_code_class$managed_class) # p = 0.033
wilcox.test(managed_gaps$Buffer10m_zpcum7_AHN3, unmanaged_gaps$Buffer10m_zpcum7_AHN3, alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap Buffer10m_ipcumzq10_AHN4 managed vs unmanaged
leveneTest(new_gaps_plot_code_class$Buffer10m_ipcumzq10_AHN4, new_gaps_plot_code_class$managed_class) # p = 0.7479
t.test(managed_gaps$Buffer10m_ipcumzq10_AHN4, unmanaged_gaps$Buffer10m_ipcumzq10_AHN4, alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap ForestPlot_fraq_NGBM managed vs unmanaged
leveneTest(new_gaps_plot_code_class$ForestPlot_fraq_NGBM, new_gaps_plot_code_class$managed_class) # p < 2.2e-16
wilcox.test(managed_gaps$ForestPlot_fraq_NGBM, unmanaged_gaps$ForestPlot_fraq_NGBM, alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap ForestPlot_fraq_NGCM managed vs unmanaged
leveneTest(new_gaps_plot_code_class$ForestPlot_fraq_NGCM, new_gaps_plot_code_class$managed_class) # p < 2.2e-16
wilcox.test(managed_gaps$ForestPlot_fraq_NGCM, unmanaged_gaps$ForestPlot_fraq_NGCM, alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap ForestPlot_fraq_DG managed vs unmanaged
leveneTest(new_gaps_plot_code_class$ForestPlot_fraq_DG, new_gaps_plot_code_class$managed_class) # p < 2.2e-16
wilcox.test(managed_gaps$ForestPlot_fraq_DG, unmanaged_gaps$ForestPlot_fraq_DG, alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap ForestPlot_fraq_RG managed vs unmanaged
leveneTest(new_gaps_plot_code_class$ForestPlot_fraq_RG, new_gaps_plot_code_class$managed_class) # p < 2.2e-16
wilcox.test(managed_gaps$ForestPlot_fraq_RG, unmanaged_gaps$ForestPlot_fraq_RG, alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap ForestPlot_fraq_VC managed vs unmanaged
leveneTest(new_gaps_plot_code_class$ForestPlot_fraq_VC, new_gaps_plot_code_class$managed_class) # p < 2.2e-16
wilcox.test(managed_gaps$ForestPlot_fraq_VC, unmanaged_gaps$ForestPlot_fraq_VC, alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap ForestPlot_GD managed vs unmanaged
leveneTest(new_gaps_plot_code_class$ForestPlot_GD, new_gaps_plot_code_class$managed_class) # p < 2.2e-16
wilcox.test(managed_gaps$ForestPlot_GD, unmanaged_gaps$ForestPlot_GD, alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap ForestPlot_gini_CHM4 managed vs unmanaged
leveneTest(new_gaps_plot_code_class$ForestPlot_gini_CHM4, new_gaps_plot_code_class$managed_class) # p < 2.2e-16
wilcox.test(managed_gaps$ForestPlot_gini_CHM4, unmanaged_gaps$ForestPlot_gini_CHM4, alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap ForestPlot_mean_CHMdiv managed vs unmanaged
leveneTest(new_gaps_plot_code_class$ForestPlot_mean_CHMdiv, new_gaps_plot_code_class$managed_class) # p < 2.2e-16
wilcox.test(managed_gaps$ForestPlot_mean_CHMdiv, unmanaged_gaps$ForestPlot_mean_CHMdiv, alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap ForestPlot_PIG managed vs unmanaged
leveneTest(new_gaps_plot_code_class$ForestPlot_PIG, new_gaps_plot_code_class$managed_class) # p < 2.2e-16
wilcox.test(managed_gaps$ForestPlot_PIG, unmanaged_gaps$ForestPlot_PIG, alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap ForestPlot_mean_CHM3 managed vs unmanaged
leveneTest(new_gaps_plot_code_class$ForestPlot_mean_CHM3, new_gaps_plot_code_class$managed_class) # p < 2.2e-16
wilcox.test(managed_gaps$ForestPlot_mean_CHM3, unmanaged_gaps$ForestPlot_mean_CHM3, alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap ForestPlot_fraq_NoG managed vs unmanaged
leveneTest(new_gaps_plot_code_class$ForestPlot_fraq_NoG, new_gaps_plot_code_class$managed_class) # p < 2.2e-16
wilcox.test(managed_gaps$ForestPlot_fraq_NoG, unmanaged_gaps$ForestPlot_fraq_NoG, alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap ForestPlot_mean_CHM4 managed vs unmanaged
leveneTest(new_gaps_plot_code_class$ForestPlot_mean_CHM4, new_gaps_plot_code_class$managed_class) # p < 2.2e-16
wilcox.test(managed_gaps$ForestPlot_mean_CHM4, unmanaged_gaps$ForestPlot_mean_CHM4, alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap ForestPlot_mean_CHM4 managed vs unmanaged
leveneTest(new_gaps_plot_code_class$ForestPlot_mean_CHM4, new_gaps_plot_code_class$managed_class) # p < 2.2e-16
wilcox.test(managed_gaps$ForestPlot_mean_CHM4, unmanaged_gaps$ForestPlot_mean_CHM4, alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap ForestPlot_gini_CHM3 managed vs unmanaged
leveneTest(new_gaps_plot_code_class$ForestPlot_gini_CHM3, new_gaps_plot_code_class$managed_class) # p < 2.2e-16
wilcox.test(managed_gaps$ForestPlot_gini_CHM3, unmanaged_gaps$ForestPlot_gini_CHM3, alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap ForestPlot_gini_CHMdiv managed vs unmanaged
leveneTest(new_gaps_plot_code_class$ForestPlot_gini_CHMdiv, new_gaps_plot_code_class$managed_class) # p < 2.2e-16
wilcox.test(managed_gaps$ForestPlot_gini_CHMdiv, unmanaged_gaps$ForestPlot_gini_CHMdiv, alternative = "two.sided") #  p-value = 1.344e-06 ***

# Comparison canopy gap ForestPlot_min_CHM4 managed vs unmanaged
leveneTest(new_gaps_plot_code_class$ForestPlot_min_CHM4, new_gaps_plot_code_class$managed_class) # p = 0.007429
wilcox.test(managed_gaps$ForestPlot_min_CHM4, unmanaged_gaps$ForestPlot_min_CHM4, alternative = "two.sided") #  p-value = 1.29e-14 ***

# Comparison canopy gap ForestPlot_max_CHMdiv managed vs unmanaged
leveneTest(new_gaps_plot_code_class$ForestPlot_max_CHMdiv, new_gaps_plot_code_class$managed_class) # p < 2.2e-16
wilcox.test(managed_gaps$ForestPlot_max_CHMdiv, unmanaged_gaps$ForestPlot_max_CHMdiv, alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap ForestPlot_sd_CHM3 managed vs unmanaged
leveneTest(new_gaps_plot_code_class$ForestPlot_sd_CHM3, new_gaps_plot_code_class$managed_class) # p < 2.2e-16
wilcox.test(managed_gaps$ForestPlot_sd_CHM3, unmanaged_gaps$ForestPlot_sd_CHM3, alternative = "two.sided") #  p-value < 2.2e-16

# Comparison canopy gap ipcumzq70_AHN4 managed vs unmanaged
leveneTest(new_gaps_plot_code_class$ipcumzq70_AHN4, new_gaps_plot_code_class$managed_class) # p < 2.2e-16
wilcox.test(managed_gaps$ipcumzq70_AHN4, unmanaged_gaps$ipcumzq70_AHN4, alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap ipground_AHN3 managed vs unmanaged
leveneTest(new_gaps_plot_code_class$ipground_AHN3, new_gaps_plot_code_class$managed_class) # p < 0.0007876
wilcox.test(managed_gaps$ipground_AHN3, unmanaged_gaps$ipground_AHN3, alternative = "two.sided") #  p-value < 2.2e-16 ***


## Other interesting canopy gap metrics

# Comparison canopy gap area managed vs unmanaged
leveneTest(new_gaps_plot_code_class$shape_area, new_gaps_plot_code_class$managed_class, center = median) # p = 0.2709
t.test(managed_gaps$shape_area, unmanaged_gaps$shape_area, alternative = "two.sided") # p = 0.01187 *

# Comparison canopy gap perimeter managed vs unmanaged
leveneTest(new_gaps_plot_code_class$shape_perimeter, new_gaps_plot_code_class$managed_class, center = median) # p = 0.05734
t.test(managed_gaps$shape_perimeter, unmanaged_gaps$shape_perimeter, alternative = "two.sided") # p = 0.002323 **

# Comparison canopy gap zmean_AHN3 managed vs unmanaged
leveneTest(new_gaps_plot_code_class$zmean_AHN3, new_gaps_plot_code_class$managed_class) # p = 1.138e-05
wilcox.test(managed_gaps$zmean_AHN3, unmanaged_gaps$zmean_AHN3, alternative = "two.sided") #  p-value < 8.776e-11 ***

# Comparison canopy gap mean_CHM3 managed vs unmanaged
leveneTest(new_gaps_plot_code_class$mean_CHM3, new_gaps_plot_code_class$managed_class) # p = 0.001903
wilcox.test(managed_gaps$mean_CHM3, unmanaged_gaps$mean_CHM3, alternative = "two.sided") #  p-value = 0.318

# Comparison canopy gap zmean_AHN4 managed vs unmanaged
leveneTest(new_gaps_plot_code_class$zmean_AHN4, new_gaps_plot_code_class$managed_class) # p = 2.659e-07
wilcox.test(managed_gaps$zmean_AHN4, unmanaged_gaps$zmean_AHN4, alternative = "two.sided") #  p-value = 2.84e-05 ***

# Comparison canopy gap mean_CHM4 managed vs unmanaged
leveneTest(new_gaps_plot_code_class$mean_CHM4, new_gaps_plot_code_class$managed_class) # p = 0.001054
wilcox.test(managed_gaps$mean_CHM4, unmanaged_gaps$mean_CHM4, alternative = "two.sided") #  p-value = 0.6436

# Comparison canopy gap max_CHM4 managed vs unmanaged
leveneTest(new_gaps_plot_code_class$max_CHM4, new_gaps_plot_code_class$managed_class) # p = 9.462e-06
wilcox.test(managed_gaps$max_CHM4, unmanaged_gaps$max_CHM4, alternative = "two.sided") #  p-value = 0.2071

# Comparison canopy gap mean_CHMdiv managed vs unmanaged
leveneTest(new_gaps_plot_code_class$mean_CHMdiv, new_gaps_plot_code_class$managed_class) # p = 5.874e-06
wilcox.test(managed_gaps$mean_CHMdiv, unmanaged_gaps$mean_CHMdiv, alternative = "two.sided") #  p-value = 0.2874

# Comparison canopy gap dist_nn managed vs unmanaged
leveneTest(new_gaps_plot_code_class$dist_nn, new_gaps_plot_code_class$managed_class) # p = < 2.2e-16 
wilcox.test(managed_gaps$dist_nn, unmanaged_gaps$dist_nn, alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap buffer5m_overlap_relative managed vs unmanaged
leveneTest(new_gaps_plot_code_class$buffer5m_overlap_relative, new_gaps_plot_code_class$managed_class) # p = < 2.513e-13
wilcox.test(managed_gaps$buffer5m_overlap_relative, unmanaged_gaps$buffer5m_overlap_relative, alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap buffer10m_overlap_relative managed vs unmanaged
leveneTest(new_gaps_plot_code_class$buffer10m_overlap_relative, new_gaps_plot_code_class$managed_class) # p < 2.2e-16
wilcox.test(managed_gaps$buffer10m_overlap_relative, unmanaged_gaps$buffer10m_overlap_relative, alternative = "two.sided") #  p-value < 2.2e-16 ***

# Comparison canopy gap fraq_NGCM managed vs unmanaged
leveneTest(new_gaps_plot_code_class$fraq_NGCM, new_gaps_plot_code_class$managed_class) # p = 0.002081
wilcox.test(managed_gaps$fraq_NGCM, unmanaged_gaps$fraq_NGCM, alternative = "two.sided") #  p-value = 0.2115

# Comparison canopy gap fraq_NGBM managed vs unmanaged
leveneTest(new_gaps_plot_code_class$fraq_NGBM, new_gaps_plot_code_class$managed_class) # p = 0.001911
wilcox.test(managed_gaps$fraq_NGBM, unmanaged_gaps$fraq_NGBM, alternative = "two.sided") #  p-value = 0.3421


## Comparing interesting forest plot metrics

# Comparison forest plot percentage in gap managed vs unmanaged
leveneTest(forest_plots_metrics_and_class$ForestPlot_PIG, forest_plots_metrics_and_class$managed_class) # p = 0.001653
wilcox.test(forest_plots_managed_metrics$ForestPlot_PIG, forest_plots_unmanaged_metrics$ForestPlot_PIG, alternative = "two.sided") #  p-value = 0.1388

# Comparison forest plot gap density managed vs unmanaged
leveneTest(forest_plots_metrics_and_class$ForestPlot_GD, forest_plots_metrics_and_class$managed_class) # p = 1.53e-06 
wilcox.test(forest_plots_managed_metrics$ForestPlot_GD, forest_plots_unmanaged_metrics$ForestPlot_GD, alternative = "two.sided") #  p-value = 0.03654 *

# Comparison forest plot mean CHM3
leveneTest(forest_plots_metrics_and_class$ForestPlot_mean_CHM3, forest_plots_metrics_and_class$managed_class) # p = 4.241e-06
wilcox.test(forest_plots_managed_metrics$ForestPlot_mean_CHM3, forest_plots_unmanaged_metrics$ForestPlot_mean_CHM3, alternative = "two.sided") #  p-value = 5.902e-11 ***

# Comparison forest plot mean CHM4
leveneTest(forest_plots_metrics_and_class$ForestPlot_mean_CHM4, forest_plots_metrics_and_class$managed_class) # p = 3.299e-06
wilcox.test(forest_plots_managed_metrics$ForestPlot_mean_CHM4, forest_plots_unmanaged_metrics$ForestPlot_mean_CHM4, alternative = "two.sided") #  p-value = 5.655e-12 ***

# Comparison forest plot fraction disappeared gap
leveneTest(forest_plots_metrics_and_class$ForestPlot_fraq_DG, forest_plots_metrics_and_class$managed_class) # p = 0.002324
wilcox.test(forest_plots_managed_metrics$ForestPlot_fraq_DG, forest_plots_unmanaged_metrics$ForestPlot_fraq_DG, alternative = "two.sided") #  p-value = 3.956e-05 ***

# Comparison forest plot fraction disappeared gap vertical closure
leveneTest(forest_plots_metrics_and_class$ForestPlot_fraq_VC, forest_plots_metrics_and_class$managed_class) # p = 0.01033
wilcox.test(forest_plots_managed_metrics$ForestPlot_fraq_VC, forest_plots_unmanaged_metrics$ForestPlot_fraq_VC, alternative = "two.sided") #  p-value = 0.00259 **

# Comparison forest plot fraction no gap
leveneTest(forest_plots_metrics_and_class$ForestPlot_fraq_NoG, forest_plots_metrics_and_class$managed_class) # p = 1.374e-05
wilcox.test(forest_plots_managed_metrics$ForestPlot_fraq_NoG, forest_plots_unmanaged_metrics$ForestPlot_fraq_NoG, alternative = "two.sided") #  p-value = 3.025e-09 ***

# Comparison forest plot fraction remaining gap
leveneTest(forest_plots_metrics_and_class$ForestPlot_fraq_RG, forest_plots_metrics_and_class$managed_class) # p = 0.003017
wilcox.test(forest_plots_managed_metrics$ForestPlot_fraq_RG, forest_plots_unmanaged_metrics$ForestPlot_fraq_RG, alternative = "two.sided") #  p-value = 0.0002126 **

# Comparison forest plot fraction new gap both methods
leveneTest(forest_plots_metrics_and_class$ForestPlot_fraq_NGBM, forest_plots_metrics_and_class$managed_class) # p = 0.005805
wilcox.test(forest_plots_managed_metrics$ForestPlot_fraq_NGBM, forest_plots_unmanaged_metrics$ForestPlot_fraq_NGBM, alternative = "two.sided") #  p-value = 0.08577 .

# Comparison forest plot fraction new gap both methods
leveneTest(forest_plots_metrics_and_class$ForestPlot_fraq_NGCM, forest_plots_metrics_and_class$managed_class) # p = 3.039e-05
wilcox.test(forest_plots_managed_metrics$ForestPlot_fraq_NGCM, forest_plots_unmanaged_metrics$ForestPlot_fraq_NGCM, alternative = "two.sided") #  p-value = 0.03682 *



### Deriving important numbers


## Canopy gap numbers

# Average CHM3
mean(new_gaps_plot_code_class$ForestPlot_mean_CHM3) # all: 21.3438 
mean(managed_gaps$ForestPlot_mean_CHM3) # managed: 19.58327 
mean(unmanaged_gaps$ForestPlot_mean_CHM3) # unmanaged: 23.9535

# Average CHM4
mean(new_gaps_plot_code_class$ForestPlot_mean_CHM4) # all: 19.38651 
mean(managed_gaps$ForestPlot_mean_CHM4) # managed: 18.54271 
mean(unmanaged_gaps$ForestPlot_mean_CHM4) # unmanaged: 23.89327

# Average canopy gap shape
mean(new_gaps_plot_code_class$shape_area) # all: 60.8423 
mean(managed_gaps$shape_area) # managed: 63.59722 
mean(unmanaged_gaps$shape_area) # unmanaged: 46.12821

# Average canopy gap perimeter
mean(new_gaps_plot_code_class$shape_perimeter) # all: 49.57218 
mean(managed_gaps$shape_perimeter) # managed: 50.85454 
mean(unmanaged_gaps$shape_perimeter) # unmanaged: 42.72308

# Average canopy gap distance to nearest neighbour
mean(new_gaps_plot_code_class$dist_nn) # all: 6.631784 
mean(managed_gaps$dist_nn) # managed: 5.157287 
mean(unmanaged_gaps$dist_nn) # unmanaged: 14.50711

# Average canopy gap relative overlap buffer 5 meter
mean(new_gaps_plot_code_class$buffer5m_overlap_relative) # all: 0.02040149 
mean(managed_gaps$buffer5m_overlap_relative) # managed: 0.02234008 
mean(unmanaged_gaps$buffer5m_overlap_relative) # unmanaged: 0.01004746

# Average canopy gap relative overlap buffer 10 meter
mean(new_gaps_plot_code_class$buffer10m_overlap_relative) # all: 0.05461351 
mean(managed_gaps$buffer10m_overlap_relative) # managed: 0.06134297 
mean(unmanaged_gaps$buffer10m_overlap_relative) # unmanaged: 0.01867132

# Average forest plot percentage in gap
mean(new_gaps_plot_code_class$ForestPlot_PIG) # all: 0.09847435 
mean(managed_gaps$ForestPlot_PIG) # managed: 0.1130785 
mean(unmanaged_gaps$ForestPlot_PIG) # unmanaged: 0.0204731

# Average forest plot gap density
mean(new_gaps_plot_code_class$ForestPlot_GD) # all: 0.001957275 
mean(managed_gaps$ForestPlot_GD) # 0.managed: 002240007 
mean(unmanaged_gaps$ForestPlot_GD) # unmanaged: 0.0004471964

# Average forest plot fraction new gap both methods
mean(new_gaps_plot_code_class$ForestPlot_fraq_NGBM) # all: 0.05233048 
mean(managed_gaps$ForestPlot_fraq_NGBM) # managed: 0.05997824 
mean(unmanaged_gaps$ForestPlot_fraq_NGBM) # unmanaged: 0.01148361

# Average forest plot fraction new gap CHM-difference method
mean(new_gaps_plot_code_class$ForestPlot_fraq_NGCM) # all: 0.04683926 
mean(managed_gaps$ForestPlot_fraq_NGCM) # managed: 0.05394604 
mean(unmanaged_gaps$ForestPlot_fraq_NGCM) # unmanaged: 0.00888181

# Average forest plot fraction remaining gap
mean(new_gaps_plot_code_class$ForestPlot_fraq_RG) # all: 0.06663502 
mean(managed_gaps$ForestPlot_fraq_RG) # managed: 0.07372087 
mean(unmanaged_gaps$ForestPlot_fraq_RG) # unmanaged: 0.02878932

# Average forest plot fraction disappeared gap
mean(new_gaps_plot_code_class$ForestPlot_fraq_DG) # all: 0.01397212 
mean(managed_gaps$ForestPlot_fraq_DG) # managed: 0.01552027 
mean(unmanaged_gaps$ForestPlot_fraq_DG) # unmanaged: 0.005703434

# Average forest plot fraction disappeared gap vertical closure
mean(new_gaps_plot_code_class$ForestPlot_fraq_VC) # all: 0.1428453 
mean(managed_gaps$ForestPlot_fraq_VC) # managed: 0.153059 
mean(unmanaged_gaps$ForestPlot_fraq_VC) # unmanaged: 0.08829381

# Average forest plot fraction no gap
mean(new_gaps_plot_code_class$ForestPlot_fraq_NoG) # all: 0.8186933 
mean(managed_gaps$ForestPlot_fraq_NoG) # managed: 0.7951033 
mean(unmanaged_gaps$ForestPlot_fraq_NoG) # unmanaged: 0.944688

# Average forest plot fraction disappeared gap vertical
mean(new_gaps_plot_code_class$ForestPlot_fraq_VC) # all: 0.1428453 
mean(managed_gaps$ForestPlot_fraq_VC) # managed: 0.153059 
mean(unmanaged_gaps$ForestPlot_fraq_VC) # unmanaged: 0.08829381

# Average canopy gap fraction new gap both methods
mean(new_gaps_plot_code_class$fraq_NGBM) # all: 0.4316125 
mean(managed_gaps$fraq_NGBM) # managed: 0.4292782 
mean(unmanaged_gaps$fraq_NGBM) # unmanaged: 0.4440803

# Average canopy gap fraction new gap CHM-difference method
mean(new_gaps_plot_code_class$fraq_NGCM) # all: 0.5447979 
mean(managed_gaps$fraq_NGCM) # managed: 0.5480097 
mean(unmanaged_gaps$fraq_NGCM) # unmanaged: 0.5276436


## Forest plot numbers

# Average forest plot percentage in gap
mean(forest_plots_metrics_and_class$ForestPlot_PIG) # all: 0.05918533 
mean(forest_plots_managed_metrics$ForestPlot_PIG) # managed: 0.06902908 
mean(forest_plots_unmanaged_metrics$ForestPlot_PIG) # unmanaged: 0.01670177

# Average forest plot gap density
mean(forest_plots_metrics_and_class$ForestPlot_GD) # all: 0.0009198214 
mean(forest_plots_managed_metrics$ForestPlot_GD) # managed: 0.001049723 
mean(forest_plots_unmanaged_metrics$ForestPlot_GD) # unmanaged: 0.0003591934

# Average forest plot mean CHM3
mean(forest_plots_metrics_and_class$ForestPlot_mean_CHM3) # all: 19.38661 
mean(forest_plots_managed_metrics$ForestPlot_mean_CHM3) # managed: 18.34325 
mean(forest_plots_unmanaged_metrics$ForestPlot_mean_CHM3) # unmanaged: 23.88953

# Average forest plot mean CHM4
mean(forest_plots_metrics_and_class$ForestPlot_mean_CHM4) # all: 19.09651 
mean(forest_plots_managed_metrics$ForestPlot_mean_CHM4) # managed: 17.97541 
mean(forest_plots_unmanaged_metrics$ForestPlot_mean_CHM4) # unmanaged: 23.93493

# Average forest plot fraction disappeared gap
mean(forest_plots_metrics_and_class$ForestPlot_fraq_DG) # all: 0.02128879 
mean(forest_plots_managed_metrics$ForestPlot_fraq_DG) # managed: 0.02464364 
mean(forest_plots_unmanaged_metrics$ForestPlot_fraq_DG) # unmanaged: 0.006809972

# Average forest plot fraction disappeared gap vertical closure
mean(forest_plots_metrics_and_class$ForestPlot_fraq_VC) # all: 0.1483503 
mean(forest_plots_managed_metrics$ForestPlot_fraq_VC) # managed: 0.1629379 
mean(forest_plots_unmanaged_metrics$ForestPlot_fraq_VC) # unmanaged: 0.08539333

# Average forest plot fraction no gap
mean(forest_plots_metrics_and_class$ForestPlot_fraq_NoG) # all: 0.8250836 
mean(forest_plots_managed_metrics$ForestPlot_fraq_NoG) # managed: 0.7977251 
mean(forest_plots_unmanaged_metrics$ForestPlot_fraq_NoG) # unmanaged: 0.9431569

# Average forest plot fraction remaining gap
mean(forest_plots_metrics_and_class$ForestPlot_fraq_RG) # all: 0.090553 
mean(forest_plots_managed_metrics$ForestPlot_fraq_RG) # managed: 0.1038107 
mean(forest_plots_unmanaged_metrics$ForestPlot_fraq_RG) # unmanaged: 0.03333537

# Average forest plot fraction new gap both methods
mean(forest_plots_metrics_and_class$ForestPlot_fraq_NGBM) # all: 0.03489611 
mean(forest_plots_managed_metrics$ForestPlot_fraq_NGBM) # managed: 0.04078537 
mean(forest_plots_unmanaged_metrics$ForestPlot_fraq_NGBM) # unmanaged: 0.009479296

# Average forest plot fraction new gap CHM-subtraction methods
mean(forest_plots_metrics_and_class$ForestPlot_fraq_NGCM) # all: 0.02671718 
mean(forest_plots_managed_metrics$ForestPlot_fraq_NGCM) # managed: 0.03132461 
mean(forest_plots_unmanaged_metrics$ForestPlot_fraq_NGCM) # unmanaged: 0.006832468



