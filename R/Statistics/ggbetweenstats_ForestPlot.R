## Forest plot level

# ForestPlot_PIG
ggstatsplot::ggbetweenstats(
  data = forest_plots_important_species,
  x = managed_class,
  y = ForestPlot_PIG,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "s",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Forest plot fraction of area in new canopy gap",
  xlab = "management class",
  ylab = "fraction"
)

# ForestPlot_GD
ggstatsplot::ggbetweenstats(
  data = forest_plots_important_species,
  x = managed_class,
  y = ForestPlot_GD,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "s",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Forest plot new canopy gap density",
  xlab = "management class",
  ylab = "density (n/area)"
)

# ForestPlot_DI
ggstatsplot::ggbetweenstats(
  data = forest_plots_important_species,
  x = managed_class,
  y = ForestPlot_DI,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "s",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Forest plot new canopy gap dispersion index",
  xlab = "management class",
  ylab = "dispersion index"
)

# ForestPlot_CE
ggstatsplot::ggbetweenstats(
  data = forest_plots_important_species,
  x = managed_class,
  y = ForestPlot_CE,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "s",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Forest plot canopy edge",
  xlab = "management class",
  ylab = "canopy edge"
)

# ForestPlot_mean_CHM3
ggstatsplot::ggbetweenstats(
  data = forest_plots_important_species,
  x = managed_class,
  y = ForestPlot_mean_CHM3,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "s",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Forest plot mean CHM3",
  xlab = "management class",
  ylab = "height (m)"
)

# ForestPlot_mean_CHM4
ggstatsplot::ggbetweenstats(
  data = forest_plots_important_species,
  x = managed_class,
  y = ForestPlot_mean_CHM4,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "s",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Forest plot mean CHM4",
  xlab = "management class",
  ylab = "height (m)"
)

# ForestPlot_mean_CHMdiv
ggstatsplot::ggbetweenstats(
  data = forest_plots_important_species,
  x = managed_class,
  y = ForestPlot_mean_CHMdiv,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "s",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Forest plot mean CHMdiv",
  xlab = "management class",
  ylab = "height (m)"
)

# ForestPlot_gini_CHM3
ggstatsplot::ggbetweenstats(
  data = forest_plots_important_species,
  x = managed_class,
  y = ForestPlot_gini_CHM3,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "s",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Forest plot Gini coefficient CHM3",
  xlab = "management class",
  ylab = "Gini coefficient"
)

# ForestPlot_gini_CHM4
ggstatsplot::ggbetweenstats(
  data = forest_plots_important_species,
  x = managed_class,
  y = ForestPlot_gini_CHM4,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "s",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Forest plot Gini coefficient CHM4",
  xlab = "management class",
  ylab = "Gini coefficient"
)

# ForestPlot_fraq_NoG
ggstatsplot::ggbetweenstats(
  data = forest_plots_important_species,
  x = managed_class,
  y = ForestPlot_fraq_NoG,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "s",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Forest plot fraction NoG",
  xlab = "management class",
  ylab = "fraction"
)

# ForestPlot_fraq_DG
ggstatsplot::ggbetweenstats(
  data = forest_plots_important_species,
  x = managed_class,
  y = ForestPlot_fraq_DG,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "s",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Forest plot fraction DG",
  xlab = "management class",
  ylab = "fraction"
)

# ForestPlot_fraq_RG
ggstatsplot::ggbetweenstats(
  data = forest_plots_important_species,
  x = managed_class,
  y = ForestPlot_fraq_RG,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "s",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Forest plot fraction RG",
  xlab = "management class",
  ylab = "fraction"
)

# ForestPlot_fraq_VC
ggstatsplot::ggbetweenstats(
  data = forest_plots_important_species,
  x = managed_class,
  y = ForestPlot_fraq_VC,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "s",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Forest plot fraction VC of fraction DG",
  xlab = "management class",
  ylab = "fraction"
)

# ForestPlot_fraq_NGBM
ggstatsplot::ggbetweenstats(
  data = forest_plots_important_species,
  x = managed_class,
  y = ForestPlot_fraq_NGBM,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "s",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Forest plot fraction NGBM",
  xlab = "management class",
  ylab = "fraction"
)

# ForestPlot_fraq_NGCM
ggstatsplot::ggbetweenstats(
  data = forest_plots_important_species,
  x = managed_class,
  y = ForestPlot_fraq_NGCM,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "s",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Forest plot fraction NGLM",
  xlab = "management class",
  ylab = "fraction"
)



forest_plots_important_species