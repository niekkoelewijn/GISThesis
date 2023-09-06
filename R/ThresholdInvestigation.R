# Course: GRS-80436 / MSc Thesis
# Project: AHN canopy gap detection
# Author: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: L.C. Vreugdenhil MSc.
# Year: 2022

### Description ###
# Script to check what comibination of thresholds leads to the best canopy gap
# detection results. Comparisons of outcomes is done with QGIS, therefore the
# polygons are written to shapefiles.


### CHM-subtraction method


## CHM-subtraction, 5m, 20m2

# Get canopy gaps
div_gaps_5_20_noupper <- getForestGaps(div_chm, threshold = -5, size = c(20, 10^99))
plot(div_gaps_5_20_noupper, legend = F, main = "Gaps from div CHM, height = 5m, size = 20-10^99m2")

# Vectorize canopy gaps
CanopyGapDivPolygons520 <- terra::as.polygons(div_gaps_5_20_noupper) %>% 
  st_as_sf() %>% 
  dplyr::rename("ID" = "gaps")

# Write canopy gaps to file
st_write(CanopyGapDivPolygons520, paste0(path, "CHMSubtractionMethod520.shp"), overwrite = T)


## CHM-subtraction, 5m, 10m2

# Get canopy gaps
div_gaps_5_10_noupper <- getForestGaps(div_chm, threshold = -5, size = c(10, 10^99))
plot(div_gaps_5_10_noupper, legend = F, main = "Gaps from div CHM, height = 5m, size = 10-10^99m2")

# Vectorize canopy gaps
CanopyGapDivPolygons510 <- terra::as.polygons(div_gaps_5_10_noupper) %>% 
  st_as_sf() %>% 
  dplyr::rename("ID" = "gaps")

# Write canopy gaps to file
st_write(CanopyGapDivPolygons510, paste0(path, "CHMSubtractionMethod510.shp"), overwrite = T)


## CHM-subtraction, 2m, 20m2

# Get canopy gaps
div_gaps_2_20_noupper <- getForestGaps(div_chm, threshold = -2, size = c(20, 10^99))
plot(div_gaps_2_20_noupper, legend = F, main = "Gaps from div CHM, height = 2m, size = 20-10^99m2")

# Vectorize canopy gaps
CanopyGapDivPolygons220 <- terra::as.polygons(div_gaps_2_20_noupper) %>% 
  st_as_sf() %>% 
  dplyr::rename("ID" = "gaps")

# Write canopy gaps to file
st_write(CanopyGapDivPolygons220, paste0(path, "CHMSubtractionMethod220.shp"), overwrite = T)


## CHM-subtraction, 2m, 10m2

# Get canopy gaps
div_gaps_2_10_noupper <- getForestGaps(div_chm, threshold = -2, size = c(10, 10^99))
plot(div_gaps_2_10_noupper, legend = F, main = "Gaps from div CHM, height = 2m, size = 10-10^99m2")

# Vectorize canopy gaps
CanopyGapDivPolygons210 <- terra::as.polygons(div_gaps_2_10_noupper) %>% 
  st_as_sf() %>% 
  dplyr::rename("ID" = "gaps")

# Write canopy gaps to file
st_write(CanopyGapDivPolygons210, paste0(path, "CHMSubtractionMethod210.shp"), overwrite = T)


## CHM-subtraction, 40cm, 20m2

# Get canopy gaps
div_gaps_040_20_noupper <- getForestGaps(div_chm, threshold = -0.4, size = c(20, 10^99))
plot(div_gaps_040_20_noupper, legend = F, main = "Gaps from div CHM, height = 0.4m, size = 20-10^99m2")

# Vectorize canopy gaps
CanopyGapDivPolygons04020 <- terra::as.polygons(div_gaps_040_20_noupper) %>% 
  st_as_sf() %>% 
  dplyr::rename("ID" = "gaps")

# Write canopy gaps to file
st_write(CanopyGapDivPolygons04020, paste0(path, "CHMSubtractionMethod04020.shp"), overwrite = T)


## CHM-subtraction, 40cm, 10m2

# Get canopy gaps
div_gaps_040_10_noupper <- getForestGaps(div_chm, threshold = -0.4, size = c(10, 10^99))
plot(div_gaps_040_10_noupper, legend = F, main = "Gaps from div CHM, height = 0.4m, size = 10-10^99m2")

# Vectorize canopy gaps
CanopyGapDivPolygons04010 <- terra::as.polygons(div_gaps_040_10_noupper) %>% 
  st_as_sf() %>% 
  dplyr::rename("ID" = "gaps")

# Write canopy gaps to file
st_write(CanopyGapDivPolygons04010, paste0(path, "CHMSubtractionMethod04010.shp"), overwrite = T)


## Create histograms of the difference in CHM per canopy gap detected with the
## different thresholds for the CHM-subtraction method

# Mask the difference CHM to the detected canopy gap areas per threshold
div_gaps_5_20_noupper_divchm <- terra::mask(div_chm, div_gaps_5_20_noupper)
div_gaps_5_10_noupper_divchm <- terra::mask(div_chm, div_gaps_5_10_noupper)
div_gaps_2_20_noupper_divchm <- terra::mask(div_chm, div_gaps_2_20_noupper)
div_gaps_2_10_noupper_divchm <- terra::mask(div_chm, div_gaps_2_10_noupper)
div_gaps_040_20_noupper_divchm <- terra::mask(div_chm, div_gaps_040_20_noupper)
div_gaps_040_10_noupper_divchm <- terra::mask(div_chm, div_gaps_040_10_noupper)

# Investigate frequenties of difference per cell
hist(div_gaps_5_20_noupper_divchm, main = "CHM-subtraction method, 5m, 20m2", breaks = 500)
hist(div_gaps_5_10_noupper_divchm, main = "CHM-subtraction method, 5m, 10m2", breaks = 500)
hist(div_gaps_2_20_noupper_divchm, main = "CHM-subtraction method, 2m, 20m2", breaks = 500)
hist(div_gaps_2_10_noupper_divchm, main = "CHM-subtraction method, 2m, 10m2", breaks = 500)
hist(div_gaps_040_20_noupper_divchm, main = "CHM-subtraction method, 0.4m, 20m2", breaks = 500)
hist(div_gaps_040_10_noupper_divchm, main = "CHM-subtraction method, 0.4m, 10m2", breaks = 500)

# Investigate frequenties of differences per gap
div_gaps_5_20_zonal <- terra::zonal(div_chm, div_gaps_5_20_noupper, fun=mean)
div_gaps_5_10_zonal <- terra::zonal(div_chm, div_gaps_5_10_noupper, fun=mean)
div_gaps_2_20_zonal <- terra::zonal(div_chm, div_gaps_2_20_noupper, fun=mean)
div_gaps_2_10_zonal <- terra::zonal(div_chm, div_gaps_2_10_noupper, fun=mean)
div_gaps_040_20_zonal <- terra::zonal(div_chm, div_gaps_040_20_noupper, fun=mean)
div_gaps_040_10_zonal <- terra::zonal(div_chm, div_gaps_040_10_noupper, fun=mean)

hist(div_gaps_5_20_zonal$Z, main = "CHM-subtraction method, 5m, 20m2", breaks = 500)
hist(div_gaps_5_10_zonal$Z, main = "CHM-subtraction method, 5m, 10m2", breaks = 500)
hist(div_gaps_2_20_zonal$Z, main = "CHM-subtraction method, 2m, 20m2", breaks = 500)
hist(div_gaps_2_10_zonal$Z, main = "CHM-subtraction method, 2m, 10m2", breaks = 500)
hist(div_gaps_040_20_zonal$Z, main = "CHM-subtraction method, 0.4m, 20m2", breaks = 500)
hist(div_gaps_040_10_zonal$Z, main = "CHM-subtraction method, 0.4m, 10m2", breaks = 500)



### Silva method


## Silva, 5m, 20m2

gaps_AHN3_5_20_noupper <- getForestGaps(chm_AHN3, threshold = 5, size = c(20, 10^99))
gaps_AHN4_5_20_noupper <- getForestGaps(chm_AHN4, threshold = 5, size = c(20, 10^99))
plot(gaps_AHN3_5_20_noupper, main = "Gaps AHN3, max height = 5m , size = 20-10^99m2")
plot(gaps_AHN4_5_20_noupper, main = "Gaps AHN4, max height = 5m, size = 20-10^99m2")

gaps_AHN3_5_20_noupper_binary <- getGapsBinaryAHN3(gaps_AHN3_5_20_noupper)
plot(gaps_AHN3_5_20_noupper_binary, main = "Gaps AHN3, gap = green, no gap = grey", legend = F)

gaps_AHN4_5_20_noupper_binary <- getGapsBinaryAHN4(gaps_AHN4_5_20_noupper)
plot(gaps_AHN4_5_20_noupper_binary, main = "Gaps AHN4, gap = green, no gap = grey", legend = F)

gaps_div_5_20_noupper <- gaps_AHN4_5_20_noupper_binary - gaps_AHN3_5_20_noupper_binary
gapDifferenceVisual(gaps_div_5_20_noupper)

NewGaps_5_20_noupper <- GetNewGaps(gap_div_layer = gaps_div_5_20_noupper, size = c(20, 20^99))
plot(NewGaps_5_20_noupper, legend = F, main = "New gaps, height = 5m, size = 20-20^99m2")

CanopyGapPolygons20 <- terra::as.polygons(NewGaps_5_20_noupper) %>% 
  st_as_sf() %>% 
  dplyr::rename("ID" = "gaps")

st_write(CanopyGapPolygons20, paste0(path, "SilvaMethod20.shp"), overwrite = T)

## Silva, 5m, 10m2

gaps_AHN3_5_10_noupper <- getForestGaps(chm_AHN3, threshold = 5, size = c(10, 10^99))
gaps_AHN4_5_10_noupper <- getForestGaps(chm_AHN4, threshold = 5, size = c(10, 10^99))
plot(gaps_AHN3_5_10_noupper, main = "Gaps AHN3, max height = 5m , size = 10-10^99m2")
plot(gaps_AHN4_5_10_noupper, main = "Gaps AHN4, max height = 5m, size = 10-10^99m2")

gaps_AHN3_5_10_noupper_binary <- getGapsBinaryAHN3(gaps_AHN3_5_10_noupper)
plot(gaps_AHN3_5_10_noupper_binary, main = "Gaps AHN3, gap = green, no gap = grey", legend = F)

gaps_AHN4_5_10_noupper_binary <- getGapsBinaryAHN4(gaps_AHN4_5_10_noupper)
plot(gaps_AHN4_5_10_noupper_binary, main = "Gaps AHN4, gap = green, no gap = grey", legend = F)

gaps_div_5_10_noupper <- gaps_AHN4_5_10_noupper_binary - gaps_AHN3_5_10_noupper_binary
gapDifferenceVisual(gaps_div_5_10_noupper)

NewGaps_5_10_noupper <- GetNewGaps(gap_div_layer = gaps_div_5_10_noupper, size = c(10, 10^99))
plot(NewGaps_5_10_noupper, legend = F, main = "New gaps, height = 5m, size = 10-10^99m2")

CanopyGapPolygons10 <- terra::as.polygons(NewGaps_5_10_noupper) %>% 
  st_as_sf() %>% 
  dplyr::rename("ID" = "gaps")

st_write(CanopyGapPolygons10, paste0(path, "SilvaMethod10.shp"), overwrite = T)


## Silva, 5m, 5m2

gaps_AHN3_5_5_noupper <- getForestGaps(chm_AHN3, threshold = 5, size = c(5, 10^99))
gaps_AHN4_5_5_noupper <- getForestGaps(chm_AHN4, threshold = 5, size = c(5, 10^99))
plot(gaps_AHN3_5_5_noupper, main = "Gaps AHN3, max height = 5m , size = 5-10^99m2")
plot(gaps_AHN4_5_5_noupper, main = "Gaps AHN4, max height = 5m, size = 5-10^99m2")

gaps_AHN3_5_5_noupper_binary <- getGapsBinaryAHN3(gaps_AHN3_5_5_noupper)
plot(gaps_AHN3_5_5_noupper_binary, main = "Gaps AHN3, gap = green, no gap = grey", legend = F)

gaps_AHN4_5_5_noupper_binary <- getGapsBinaryAHN4(gaps_AHN4_5_5_noupper)
plot(gaps_AHN4_5_5_noupper_binary, main = "Gaps AHN4, gap = green, no gap = grey", legend = F)

gaps_div_5_5_noupper <- gaps_AHN4_5_5_noupper_binary - gaps_AHN3_5_5_noupper_binary
gapDifferenceVisual(gaps_div_5_5_noupper)

NewGaps_5_5_noupper <- GetNewGaps(gap_div_layer = gaps_div_5_5_noupper, size = c(5, 10^99))
plot(NewGaps_5_5_noupper, legend = F, main = "New gaps, height = 5m, size = 5-10^99m2")

CanopyGapPolygons5 <- terra::as.polygons(NewGaps_5_5_noupper) %>% 
  st_as_sf() %>% 
  dplyr::rename("ID" = "gaps")

st_write(CanopyGapPolygons5, paste0(path, "SilvaMethod5.shp"), overwrite = T)


## Compare plots of gaps

# CHM difference
plot(CanopyGapDivPolygons520, main = "CHM-subtraction method, 5m, 20m2")
plot(CanopyGapDivPolygons510, main = "CHM-subtraction method, 5m, 10m2")
plot(CanopyGapDivPolygons220, main = "CHM-subtraction method, 2m, 20m2")
plot(CanopyGapDivPolygons210, main = "CHM-subtraction method, 2m, 10m2")
plot(CanopyGapDivPolygons04020, main = "CHM-subtraction method, 0.4m, 20m2")
plot(CanopyGapDivPolygons04010, main = "CHM-subtraction method, 0.4m, 10m2")

# Silva
plot(CanopyGapPolygons20, main = "Silva method, 5m, 20m2")
plot(CanopyGapPolygons10, main = "Silva method, 5m, 10m2")
plot(CanopyGapPolygons5, main = "Silva method, 5m, 5m2")

