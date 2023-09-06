## Comparisons 15 most important variables to distinguish "number of trees" classes

# shape_area
ggbetweenstats(
  data = new_gaps_complete,
  x = ntree_class,
  y = shape_area,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap area",
  xlab = "'number of trees' class",
  ylab = "area (m2)"
)

# shape_perimeter
ggbetweenstats(
  data = new_gaps_complete,
  x = ntree_class,
  y = shape_perimeter,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap perimeter",
  xlab = "'number of trees' class",
  ylab = "perimeter (m)"
)

# shape_AHN3_planarity
ggbetweenstats(
  data = new_gaps_complete,
  x = ntree_class,
  y = shape_AHN3_planarity,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap shape planarity AHN3",
  xlab = "'number of trees' class",
  ylab = "planarity"
)

# shape_AHN3_linearity
ggbetweenstats(
  data = new_gaps_complete,
  x = ntree_class,
  y = shape_AHN3_linearity,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap shape linearity AHN3",
  xlab = "'number of trees' class",
  ylab = "linearity"
)

# shape_AHN3_eigen_medium
ggbetweenstats(
  data = new_gaps_complete,
  x = ntree_class,
  y = shape_AHN3_eigen_medium,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap shape minimum eigenvalue AHN3",
  xlab = "'number of trees' class",
  ylab = "eigenvalue"
)

# ipcumzq90_AHN3
ggbetweenstats(
  data = new_gaps_complete,
  x = ntree_class,
  y = ipcumzq90_AHN3,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap percentage of intensity returned below the 90th height \npercentile AHN3",
  xlab = "'number of trees' class",
  ylab = "percentage (%)"
)

# ipcumzq70_AHN3
ggbetweenstats(
  data = new_gaps_complete,
  x = ntree_class,
  y = ipcumzq70_AHN3,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap percentage of intensity returned below the 70th height \npercentile AHN3",
  xlab = "'number of trees' class",
  ylab = "percentage (%)"
)

# shape_IER
ggbetweenstats(
  data = new_gaps_complete,
  x = ntree_class,
  y = shape_IER,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap interior edge retio",
  xlab = "'number of trees' class",
  ylab = "IER"
)

# shape_FD
ggbetweenstats(
  data = new_gaps_complete,
  x = ntree_class,
  y = shape_FD,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap fractional dimension",
  xlab = "'number of trees' class",
  ylab = "fractional dimension"
)

# shape_AFF
ggbetweenstats(
  data = new_gaps_complete,
  x = ntree_class,
  y = shape_AFF,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap areal form factor",
  xlab = "'number of trees' class",
  ylab = "areal form factor"
)

# shape_roundness
ggbetweenstats(
  data = new_gaps_complete,
  x = ntree_class,
  y = shape_roundness,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap roundness",
  xlab = "'number of trees' class",
  ylab = "roundness"
)

# shape_GSCI
ggbetweenstats(
  data = new_gaps_complete,
  x = ntree_class,
  y = shape_GSCI,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap shape complexity index",
  xlab = "'number of trees' class",
  ylab = "shape complexity index"
)

# shape_circularity
ggbetweenstats(
  data = new_gaps_complete,
  x = ntree_class,
  y = shape_circularity,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap circularity",
  xlab = "'number of trees' class",
  ylab = "circularity"
)

# shape_SI
ggbetweenstats(
  data = new_gaps_complete,
  x = ntree_class,
  y = shape_SI,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap shape index",
  xlab = "'number of trees' class",
  ylab = "shape index"
)

# ForestPlot_fraq_NGLM
ggbetweenstats(
  data = new_gaps_complete,
  x = ntree_class,
  y = ForestPlot_fraq_NGCM,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap forest plot fraction NGCM",
  xlab = "'number of trees' class",
  ylab = "fraction"
)

# ForestPlot_GD
ggbetweenstats(
  data = new_gaps_complete,
  x = ntree_class,
  y = ForestPlot_GD,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap forest plot gap density",
  xlab = "'number of trees' class",
  ylab = "gap density"
)

# ForestPlot_GD
ggbetweenstats(
  data = new_gaps_complete,
  x = ntree_class,
  y = ForestPlot_GD,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap forest plot gap density",
  xlab = "'number of trees' class",
  ylab = "gap density"
)

# ForestPlot_PIG
ggbetweenstats(
  data = new_gaps_complete,
  x = ntree_class,
  y = ForestPlot_PIG,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap forest plot fraction in gap",
  xlab = "'number of trees' class",
  ylab = "fraction"
)



