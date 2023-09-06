# Course: GRS-80436 / MSc Thesis
# Project: AHN canopy gap detection
# Author: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: L.C. Vreugdenhil MSc.
# Year: 2022

### Description ###
# Derive metrics from of the new canopy gaps on a forest stand level, and add 
# the metrics to the polygons of the identified canopy gaps. Most metrics are 
# derived from Blackburn & Milton (1996).


## Get the forest plots of the Speulderbos that intersect completely with the 
## study area

# # Investigate forest plots that intersect with the study area
# plot(st_geometry(study_area_speulderbos))
# plot(st_geometry(forest_plots_speulderbos), add = T)

# Intersect forest plots with study area
forest_plots_in_study_area <- forest_plots_speulderbos[st_covered_by(forest_plots_speulderbos, study_area_speulderbos) %>% lengths > 0,]
forest_plots_in_study_area <- forest_plots_in_study_area %>% 
  dplyr::mutate(forest_plot_area = as.vector(st_area(forest_plots_in_study_area))) %>% 
  dplyr::select(COMBI, forest_plot_area)

# # Visualize forest plots that are within the study area
# plot(st_geometry(study_area_speulderbos))
# plot(st_geometry(forest_plots_in_study_area), add = T)
# plot(st_geometry(new_gaps_polygons), add = T)

# Get new gap polygons that are situated within forest plots
new_gaps_in_plots <- new_gaps_polygons[st_within(new_gaps_polygons, st_union(forest_plots_in_study_area)) %>% lengths > 0,]

# # Visualize canopy gaps that are within the forest plots in the study area
# plot(st_geometry(new_gaps_polygons))
# plot(st_geometry(forest_plots_in_study_area), add = T)
# plot(st_geometry(new_gaps_in_plots), add = T, col = "yellow")

# Assign forest plot code to the canopy gaps as an attribute
new_gaps_plot_code <- new_gaps_in_plots %>% 
  st_join(y = forest_plots_in_study_area, join = st_intersects, largest = T) %>%  # If gaps are on the border of two forest plots, the forest plot with the largest overlap is assigned to the gap
  dplyr::mutate(gap_area = as.vector(st_area(new_gaps_in_plots))) %>% 
  dplyr::select(ID, COMBI, forest_plot_area, gap_area)


## Add forest stand level canopy gap metrics to the new gap polygons

# Function to calculate PIG (percentage in gaps) of forest plots
# PIG = area of gaps in the forest plot / area of forest plot
PIG <- function(canopy_gaps, plots){
  PIG_per_plot <- c()
  for(i in seq_along(plots$COMBI)){
    
    # Calculate the sum of the area of canopy gaps in a forest plot
    tot_area_gaps <- as.numeric(sum(st_area(canopy_gaps[which(canopy_gaps$COMBI == plots$COMBI[i]),]))) 
    
    # Divide the total area of canopy gaps in a forest plot by the area of the forest plot to get the PIG
    PIG_elem <- tot_area_gaps / plots$forest_plot_area[i]
    
    # Add result to vector
    PIG_per_plot <- append(PIG_per_plot, PIG_elem)
  }
  return(PIG_per_plot)
}

# Function to calculate the GD (Gap Density) of forest plots
# GD = n gaps / area of forest plot
GD <- function(canopy_gaps, plots){
  GD_per_plot <- c()
  for(i in seq_along(plots$COMBI)){
    
    # Get the number of gaps for a forest plot
    n_gaps <- nrow(canopy_gaps[which(canopy_gaps$COMBI == plots$COMBI[i]),])
    
    # Divide the number of gaps by the area of forest plots
    GD_elem <- n_gaps / plots$forest_plot_area[i]
    
    # Add result to vector
    GD_per_plot <- append(GD_per_plot, GD_elem)
  }
  return(GD_per_plot)
}

# Add distance to nearest neighbor and perimeter to gap polygons in forest plots
new_gaps_plot_code_extracolumns <- new_gaps_plot_code %>% 
  dplyr::left_join(y = sf::st_drop_geometry(new_gaps_shape_metrics),keep = F) %>% 
  dplyr::select(ID, COMBI, forest_plot_area, gap_area, dist_nn, shape_perimeter)

# Function to calculate the Dispersion Index (DI) of forest plots
# DI = 2 * (GD)^0.5 * (mean nearest neighbor distance gaps in forest plot / 10)
DI <- function(canopy_gaps, plots){
  DI_per_plot <- c()
  for(i in seq_along(plots$COMBI)){
    
    # Get the mean of nearest neighbor distance of the gaps for a forest plot
    mean_nn <- mean(canopy_gaps[which(canopy_gaps$COMBI == plots$COMBI[i]),]$dist_nn)
    
    # Calculate the dispersion index for the forest plot 
    # DI = 1 indicates a random distribution of gaps
    # DI < 1 indicates a regular, dispersed patter
    # DI > 1 indicates aggregation of gaps
    DI_elem <- 2 * (plots$GD[i]*100)^0.5 * (mean_nn / 10) 
    
    # Add result to vector
    DI_per_plot <- append(DI_per_plot, DI_elem)
  }
  return(DI_per_plot)
}

# Function to calculate the Canopy Edge (CE) of forest plots
# CE = sum of gap perimeters in the forest plot / sum of gap area in the forest plot
CE <- function(canopy_gaps, plots){
  CE_per_plot <- c()
  for(i in seq_along(plots$COMBI)){
    
    # Calculate the sum of the area of canopy gaps in a forest plot
    sum_area_gaps <- as.numeric(sum(st_area(canopy_gaps[which(canopy_gaps$COMBI == plots$COMBI[i]),]))) 
    
    # Calculate the sum of the perimeter of the gaps for a forest plot
    sum_perimeter <- as.numeric(sum(canopy_gaps$shape_perimeter[which(canopy_gaps$COMBI == plots$COMBI[i])])) 
    
    # Calculate the canopy edge
    CE_elem <- sum_perimeter / sum_area_gaps
    
    # Add result to vector
    CE_per_plot <- append(CE_per_plot, CE_elem)
  }
  return(CE_per_plot)
}

# Add metrics to forest plots using pipes
forest_metrics <- forest_plots_in_study_area %>% 
  dplyr::mutate(PIG = PIG(new_gaps_plot_code, forest_plots_in_study_area),
                GD = GD(new_gaps_plot_code, forest_plots_in_study_area))

forest_metrics <- forest_metrics %>% 
  dplyr::mutate(DI = DI(new_gaps_plot_code_extracolumns, forest_metrics),
                CE = CE(new_gaps_plot_code_extracolumns, forest_metrics))


## Add forest stand level raster metrics to the new gap polygons

# Calculate fraction per category of div methods per forest plot
fraction_categories_fp <- exactextractr::exact_extract(div_methods, forest_metrics, fun = "frac")
fraction_GCA_fp <- exactextractr::exact_extract(ClassifiedGCA_mask, forest_metrics, fun = "frac")

# Add CHM3 and CHM4 metrics and share of div methods raster output to forest plots
forest_metrics_raster_metrics <- forest_metrics %>% 
  
  # Add CHM3 metrics metrics 
  dplyr::mutate(mean_CHM3 = terra::extract(chm_AHN3, forest_metrics, fun = 'mean')$Z,
                min_CHM3 = terra::extract(chm_AHN3, forest_metrics, fun = 'min')$Z,
                max_CHM3 = terra::extract(chm_AHN3, forest_metrics, fun = 'max')$Z,
                sd_CHM3 = terra::extract(chm_AHN3, forest_metrics, fun = 'sd')$Z,
                gini_CHM3 = terra::extract(chm_AHN3, forest_metrics, fun = 'GiniCoeff')$Z,
                range_CHM3 = max_CHM3 - min_CHM3) %>% 
  
  # Add CHM4 metrics metrics 
  dplyr::mutate(mean_CHM4 = terra::extract(chm_AHN4, forest_metrics, fun = 'mean')$Z,
                min_CHM4 = terra::extract(chm_AHN4, forest_metrics, fun = 'min')$Z,
                max_CHM4 = terra::extract(chm_AHN4, forest_metrics, fun = 'max')$Z,
                sd_CHM4 = terra::extract(chm_AHN4, forest_metrics, fun = 'sd')$Z,
                gini_CHM4 = terra::extract(chm_AHN4, forest_metrics, fun = 'GiniCoeff')$Z,
                range_CHM4 = max_CHM4 - min_CHM4) %>% 
  
  # Add CHM div metrics metrics 
  dplyr::mutate(mean_CHMdiv = terra::extract(div_chm, forest_metrics, fun = 'mean')$Z,
                min_CHMdiv = terra::extract(div_chm, forest_metrics, fun = 'min')$Z,
                max_CHMdiv = terra::extract(div_chm, forest_metrics, fun = 'max')$Z,
                sd_CHMdiv = terra::extract(div_chm, forest_metrics, fun = 'sd')$Z,
                gini_CHMdiv = terra::extract(div_chm, forest_metrics, fun = 'GiniCoeff')$Z,
                range_CHMdiv = max_CHMdiv - min_CHMdiv) %>% 
  
  # Add share per category as an attribute
  dplyr::mutate(fraq_DG = fraction_categories_fp$`frac_-1`,    # DG = Gap disappeared over time
                fraq_NoG = fraction_categories_fp$frac_0,      # NoG = No gap detected in both AHN versions
                fraq_RG = fraction_categories_fp$frac_1,       # RG = Remaining gap, so gap in both AHN versions
                fraq_NGSM = fraction_categories_fp$frac_2,     # SMNG = New Gap detected only with Silva Method, so new gap but no height difference of 2m
                fraq_NGCM = fraction_categories_fp$frac_3,     # CMNG = New Gap detected only with CHM-subtraction Method, so new gap with undergrowth taller than 5m
                fraq_RGCM = fraction_categories_fp$frac_4,     # RGCM = According to Silva method, it is a Remaining Gap, but new gap according to CHM-subtraction method, so less than 5m in both AHN versions, but more than 2m height difference
                fraq_NGBM = fraction_categories_fp$frac_5) %>% # NGBM = New Gap detected with Both Methods
  
  # Add share per gap closure area type
  dplyr::mutate(fraq_VC = fraction_GCA_fp$frac_0,     # VC = Vertical closure, share of closed gaps that is closed due to vertical growth
                fraq_LC = fraction_GCA_fp$frac_1)     # LC = Lateral closure, share of closed gaps that is closed due to lateral growth
  
  # Remove the redundant forest_plot_area column
  #dplyr::select(-forest_plot_area)
  
# Write forest_metrics_raster_metrics to file
st_write(forest_metrics_raster_metrics, "~/ahncanopygaps/InputData/forest_metrics_raster_metrics.gpkg", append=FALSE)


## Join forest metrics with previously identified metrics  

# Adapt the names of the forest plot level metrics
colnames(forest_metrics_raster_metrics) <- paste("ForestPlot",colnames(forest_metrics_raster_metrics),sep="_")
colnames(forest_metrics_raster_metrics)[1] <- "COMBI"
colnames(forest_metrics_raster_metrics)[length(colnames(forest_metrics_raster_metrics))] <- "geometry"

# Get polygons with only the forest plot code and join with the forest plot metrics tibble
new_gaps_forestplotmetrics <- new_gaps_plot_code %>% 
  dplyr::select(ID, COMBI) %>% 
  dplyr::left_join(sf::st_drop_geometry(forest_metrics_raster_metrics))

# Join with NewGapWithRasterMetrics
new_gaps_all_metrics <- new_gaps_raster_metrics %>% 
  dplyr::left_join(sf::st_drop_geometry(new_gaps_forestplotmetrics)) %>% 
  tidyr::drop_na(COMBI) %>% 
  dplyr::select(-COMBI)

# # Write output to file
# st_write(new_gaps_all_metrics, paste0(path, "new_gaps_all_metrics.gpkg"), append = F)
