## Canopy gap level

# shape_area
ggbetweenstats(
  data = new_gaps_complete,
  x = managed_class,
  y = shape_area,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap area",
  xlab = "management class",
  ylab = "area (m2)"
) 


# shape_perimeter
ggbetweenstats(
  data = new_gaps_complete,
  x = managed_class,
  y = shape_perimeter,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap perimeter",
  xlab = "management class",
  ylab = "perimeter (m)"
)

# dist_nn
ggbetweenstats(
  data = new_gaps_complete,
  x = managed_class,
  y = dist_nn,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap distance to nearest neighbour",
  xlab = "management class",
  ylab = "distance (m)"
)

# buffer5m_overlap_relative
ggbetweenstats(
  data = new_gaps_complete,
  x = managed_class,
  y = buffer5m_overlap_relative,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap buffer 5m overlap",
  xlab = "management class",
  ylab = "overlap (%)"
)

# buffer10m_overlap_relative
ggbetweenstats(
  data = new_gaps_complete,
  x = managed_class,
  y = buffer10m_overlap_relative,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap buffer 10m overlap",
  xlab = "management class",
  ylab = "overlap (%)"
)

# mean_CHM3
ggbetweenstats(
  data = new_gaps_complete,
  x = managed_class,
  y = mean_CHM3,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap mean height CHM3",
  xlab = "management class",
  ylab = "height (m)"
)

# mean_CHM4
ggbetweenstats(
  data = new_gaps_complete,
  x = managed_class,
  y = mean_CHM4,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap mean height CHM4",
  xlab = "management class",
  ylab = "height (m)"
)

# mean_CHMdiv
ggbetweenstats(
  data = new_gaps_complete,
  x = managed_class,
  y = mean_CHMdiv,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap mean height difference",
  xlab = "management class",
  ylab = "height difference (m)"
)

# fraq_NGBM
ggbetweenstats(
  data = new_gaps_complete,
  x = managed_class,
  y = fraq_NGBM,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap fraction NGBM",
  xlab = "management class",
  ylab = "fraction"
)

# fraq_NGCM
ggbetweenstats(
  data = new_gaps_complete,
  x = managed_class,
  y = fraq_NGCM,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap fraction NGLM",
  xlab = "management class",
  ylab = "fraction"
)

# fraq_NGSM
ggbetweenstats(
  data = new_gaps_complete,
  x = managed_class,
  y = fraq_NGSM,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap fraction NGSM",
  xlab = "management class",
  ylab = "fraction"
)

# fraq_RGCM
ggbetweenstats(
  data = new_gaps_complete,
  x = managed_class,
  y = fraq_RGCM,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap fraction RGLM",
  xlab = "management class",
  ylab = "fraction"
)

# Buffer10m_ipcumzq90_AHN4
ggbetweenstats(
  data = new_gaps_complete,
  x = managed_class,
  y = Buffer10m_ipcumzq90_AHN4,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap doughnut buffer 10m percentage of intensity returned below the \n90th height percentile AHN4",
  xlab = "management class",
  ylab = "percentage (%)"
)

# Buffer5m_ipground_AHN4
ggbetweenstats(
  data = new_gaps_complete,
  x = managed_class,
  y = Buffer5m_ipground_AHN4,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap doughnut buffer 5m percentage of intensity returned by points \nclassified as 'ground' AHN4",
  xlab = "management class",
  ylab = "percentage (%)"
)

# Buffer10m_iskew_AHN4
ggbetweenstats(
  data = new_gaps_complete,
  x = managed_class,
  y = Buffer10m_iskew_AHN4,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap doughnut buffer 10m skewness of intensity distribution AHN4",
  xlab = "management class",
  ylab = "skewness"
)

# Buffer10m_zq25_AHN3
ggbetweenstats(
  data = new_gaps_complete,
  x = managed_class,
  y = Buffer10m_zq25_AHN3,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap doughnut buffer 10m 25th percentile of height distribution AHN3",
  xlab = "management class",
  ylab = "percentage (%)"
)

# ForestPlot_fraq_NGLM
ggbetweenstats(
  data = new_gaps_complete,
  x = managed_class,
  y = ForestPlot_fraq_NGCM,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap forest plot fraction NGLM",
  xlab = "management class",
  ylab = "fraction"
)

# ForestPlot_GD
ggbetweenstats(
  data = new_gaps_complete,
  x = managed_class,
  y = ForestPlot_GD,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap forest plot gap density",
  xlab = "management class",
  ylab = "gap density"
)

# ForestPlot_gini_CHM4
ggbetweenstats(
  data = new_gaps_complete,
  x = managed_class,
  y = ForestPlot_gini_CHM4,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap forest plot Gini coefficient CHM4",
  xlab = "management class",
  ylab = "Gini coefficient"
)

# ForestPlot_fraq_NoG
ggbetweenstats(
  data = new_gaps_complete,
  x = managed_class,
  y = ForestPlot_fraq_NoG,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap forest plot fraction NoG",
  xlab = "management class",
  ylab = "fraction"
)

# ForestPlot_mean_CHM4
ggbetweenstats(
  data = new_gaps_complete,
  x = managed_class,
  y = ForestPlot_mean_CHM4,
  type = "nonparametric", 
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  title = "Canopy gap mean CHM4",
  xlab = "management class",
  ylab = "height (m)"
)



