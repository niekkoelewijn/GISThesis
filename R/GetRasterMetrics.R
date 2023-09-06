# Course: GRS-80436 / MSc Thesis
# Project: AHN canopy gap detection
# Author: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: L.C. Vreugdenhil MSc.
# Year: 2022

### Description ###
# Derive metrics from relevant rasters, and add the metrics to the polygons of 
# the identified canopy gaps.


## Add raster metrics to the new gap polygons

# Function to calculate the gini coefficient, a coefficient that shows how
# equal the values are spread over the polygons
GiniCoeff <- function(x, finite.sample = TRUE, na.rm = TRUE) {
  if (!na.rm && any(is.na(x))) {
    return(NA_real_)
  }
  x <- as.numeric(stats::na.omit(x))
  n <- length(x)
  x <- sort(x)
  G <- 2 * sum(x * 1L:n) / sum(x) - (n + 1L)
  if (finite.sample) {
    GC <- G / (n - 1L)
  } else {
    GC <- G / n
  }
  return(GC)
}

# Add CHM3 metrics columns using pipes
new_gaps_polygons_raster_metrics_CHM3 <- new_gaps_polygons %>% 
  
  # Add CHM3 metrics metrics 
  dplyr::mutate(mean_CHM3 = terra::extract(chm_AHN3, new_gaps_polygons, fun = 'mean')$Z,
                min_CHM3 = terra::extract(chm_AHN3, new_gaps_polygons, fun = 'min')$Z,
                max_CHM3 = terra::extract(chm_AHN3, new_gaps_polygons, fun = 'max')$Z,
                sd_CHM3 = terra::extract(chm_AHN3, new_gaps_polygons, fun = 'sd')$Z,
                gini_CHM3 = terra::extract(chm_AHN3, new_gaps_polygons, fun = 'GiniCoeff')$Z,
                range_CHM3 = max_CHM3 - min_CHM3) 

# Add CHM4 metrics columns using pipes
new_gaps_polygons_raster_metrics_CHM4 <- new_gaps_polygons %>% 
  
  # Add CHM4 metrics metrics 
  dplyr::mutate(mean_CHM4 = terra::extract(chm_AHN4, new_gaps_polygons, fun = 'mean')$Z,
                min_CHM4 = terra::extract(chm_AHN4, new_gaps_polygons, fun = 'min')$Z,
                max_CHM4 = terra::extract(chm_AHN4, new_gaps_polygons, fun = 'max')$Z,
                sd_CHM4 = terra::extract(chm_AHN4, new_gaps_polygons, fun = 'sd')$Z,
                gini_CHM4 = terra::extract(chm_AHN4, new_gaps_polygons, fun = 'GiniCoeff')$Z,
                range_CHM4 = max_CHM4 - min_CHM4) 

# Add CHM div metrics columns using pipes
new_gaps_polygons_raster_metrics_CHMdiv <- new_gaps_polygons %>% 
  
  # Add CHM div metrics metrics 
  dplyr::mutate(mean_CHMdiv = terra::extract(div_chm, new_gaps_polygons, fun = 'mean')$Z,
                min_CHMdiv = terra::extract(div_chm, new_gaps_polygons, fun = 'min')$Z,
                max_CHMdiv = terra::extract(div_chm, new_gaps_polygons, fun = 'max')$Z,
                sd_CHMdiv = terra::extract(div_chm, new_gaps_polygons, fun = 'sd')$Z,
                gini_CHMdiv = terra::extract(div_chm, new_gaps_polygons, fun = 'GiniCoeff')$Z,
                range_CHMdiv = max_CHMdiv - min_CHMdiv) 

# Calculate fraction per category of div methods
fraction_categories <- exactextractr::exact_extract(div_methods, new_gaps_polygons, fun = "frac")

# Add div methods raster output columns using pipes
new_gaps_polygons_raster_metrics_divmethods <- new_gaps_polygons %>% 
  
  # Add share per category as an attribute
  dplyr::mutate(fraq_NGSM = fraction_categories$frac_2, # SMNG = New Gap detected only with Silva Method, so new gap but no height difference of 2m
                fraq_NGCM = fraction_categories$frac_3, # CMNG = New Gap detected only with CHM-subtraction Method, so new gap with undergrowth taller than 5m
                fraq_RGCM = fraction_categories$frac_4, # RGCM = According to Silva method, it is a Remaining Gap, but new gap according to CHM-subtraction method, so less than 5m in both AHN versions, but more than 2m height difference
                fraq_NGBM = fraction_categories$frac_5) # NGBM = New Gap detected with Both Methods


## Join the raster metrics data

# Make tibbles from the sf features
new_gaps_polygons_raster_metrics_CHM3_tibble <- new_gaps_polygons_raster_metrics_CHM3 %>% 
  as_tibble() %>% 
  select(-geometry)

new_gaps_polygons_raster_metrics_CHM4_tibble <- new_gaps_polygons_raster_metrics_CHM4 %>% 
  as_tibble() %>% 
  select(-geometry)

new_gaps_polygons_raster_metrics_CHMdiv_tibble <- new_gaps_polygons_raster_metrics_CHMdiv %>% 
  as_tibble() %>% 
  select(-geometry)

new_gaps_polygons_raster_metrics_divmethods_tibble <- new_gaps_polygons_raster_metrics_divmethods %>% 
  as_tibble() %>% 
  select(-geometry)

# Join the output of all the rasters
new_gaps_raster_metrics <- dplyr::left_join(new_gaps_shape_metrics, new_gaps_polygons_raster_metrics_CHM3_tibble,
                                            by = "ID") %>% 
  dplyr::left_join(new_gaps_polygons_raster_metrics_CHM4_tibble, by = "ID") %>% 
  dplyr::left_join(new_gaps_polygons_raster_metrics_CHMdiv_tibble, by = "ID") %>% 
  dplyr::left_join(new_gaps_polygons_raster_metrics_divmethods_tibble, by = "ID")


# # Write output to gpkg 
# st_write(new_gaps_raster_metrics, paste0(path, "new_gaps_raster_metrics.gpkg"), append = F)
