# Course: GRS-80436 / MSc Thesis
# Project: AHN canopy gap detection
# Author: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: L.C. Vreugdenhil MSc.
# Year: 2022

### Description ###
# Script to create DTM's from AHN version 3 and 4 using different algorithms 
# and to compare them in speed and output. 


# ## Observe cross-section
# 
# # Create function to plot cross-section
# plot_crossection <- function(las,
#                              p1 = c(min(las@data$X), mean(las@data$Y)),
#                              p2 = c(max(las@data$X), mean(las@data$Y)),
#                              width = 4, colour_by = NULL){
#   colour_by <- enquo(colour_by)
#   data_clip <- clip_transect(las, p1, p2, width)
#   p <- ggplot(data_clip@data, aes(X,Z)) + geom_point(size = 0.5) + coord_equal() + theme_minimal()
#   
#   if (!is.null(colour_by))
#     p <- p + aes(color = !!colour_by) + labs(color = "")
#   
#   return(p)
# }
# 
# # Plot cross-section
# p1 <- c(175510, 474800)
# p2 <- c(175710, 474800)
# plot_crossection(AHN3_study_area_clip, p1 , p2, colour_by = factor(Classification))


# ## Investigation of ground opints
# 
# # AHN3
# groundAHN3 <- filter_ground(AHN3_study_area_clip)
# plot(groundAHN3, size = 3)
# RasterGroundAHN3 <- grid_metrics(groundAHN3, res=1, mean(Z)) 
# plot(RasterGroundAHN3, main = "Ground AHN3", col=height.colors(500))
# 
# # AHN4
# groundAHN4 <- filter_ground(AHN4_study_area_clip)
# plot(groundAHN4, size = 3)
# RasterGroundAHN4 <- grid_metrics(groundAHN4, res=1, mean(Z)) 
# plot(RasterGroundAHN4, main = "Ground AHN4", col=height.colors(500))



### Create DTM with different algorithms


## AHN3

# # TIN
# tic("DTM AHN3 tin")
# dtm_AHN3_tin <- rasterize_terrain(AHN3_study_area_clip, res = 1, algorithm = tin())
# toc() # DTM AHN3 tin: 18.83 sec elapsed

# KnnIDW
tic("DTM AHN3 knnidw")
dtm_AHN3_idw <- rasterize_terrain(AHN3_catalog, res = 1, algorithm = knnidw(k = 10L, p = 2), overwrite = T)
dtm_AHN3_idw_mask <- terra::mask(dtm_AHN3_idw, study_area_speulderbos)
toc() # DTM AHN3 knnidw: 428.622 sec elapsed

# # Kriging
# tic("DTM AHN3 kriging")
# dtm_AHN3_kriging <- rasterize_terrain(AHN3_study_area_clip, algorithm = kriging(k = 40))
# toc() # DTM AHN3 kriging: 402.971 sec elapsed
# 
# # MBA
# mba <- function(n = 1, m = 1, h = 8, extend = TRUE) {
#   # f is created inside mba and receive the ground points in a LAS (gnd)
#   # and the location where to compute the interpolation (where) 
#   f <- function(gnd, where) {
#     # computation of the interpolation (see the documentation of MBA package)
#     res <- MBA::mba.points(gnd@data, where, n, m , h, extend)
#     return(res$xyz.est[,3])
#   }
#   
#   # f is a function but we can set compatible classes. Here it is an
#   # algorithm for DTM 
#   f <- plugin_dtm(f)
#   return(f)
# }
# tic("DTM AHN3 MBA")
# dtm_AHN3_mba <- rasterize_terrain(AHN3_study_area_clip, algorithm = mba(n = 1, h = 8))
# toc() # DTM AHN3 MBA: 7.375 sec elapsed


## AHN4

# # TIN
# tic("DTM AHN4 tin")
# dtm_AHN4_tin <- rasterize_terrain(AHN4_catalog, res = 1, algorithm = tin())
# toc() # DTM AHN4 tin: 51.748 sec elapsed

# KnnIDW
tic("DTM AHN4 knnidw")
dtm_AHN4_idw <- rasterize_terrain(AHN4_catalog, algorithm = knnidw(k = 10L, p = 2))
dtm_AHN4_idw_mask <- terra::mask(dtm_AHN4_idw, study_area_speulderbos)
toc() # DTM AHN4 knnidw: 1680.703 sec elapsed

# # Kriging
# tic("DTM AHN4 kriging")
# dtm_AHN4_kriging <- rasterize_terrain(AHN4_catalog, algorithm = kriging(k = 40))
# toc() # DTM AHN4 kriging: 1869.115 sec elapsed
# 
# # MBA
# tic("DTM AHN4 MBA")
# dtm_AHN4_mba <- rasterize_terrain(AHN4_catalog, algorithm = mba(n = 1, h = 8))
# toc() # DTM AHN4 MBA: 15.8 sec elapsed



# ### Visualize DTM's of different algorithmns
# 
# 
# ## 2D
# 
# # AHN3
# plot(dtm_AHN3_tin, main="DTM AHN3 TIN", col=height.colors(500))
# plot(dtm_AHN3_idw, main="DTM AHN3 IDW", col=height.colors(500))
# plot(dtm_AHN3_kriging, main="DTM AHN3 Kriging", col=height.colors(500))
# plot(dtm_AHN3_mba, main="DTM AHN3 Spline", col=height.colors(500))
# 
# # AHN4
# plot(dtm_AHN4_tin, main="DTM AHN4 TIN", col=height.colors(500))
# plot(dtm_AHN4_idw, main="DTM AHN4 IDW", col=height.colors(500))
# plot(dtm_AHN4_kriging, main="DTM AHN4 Kriging", col=height.colors(500))
# plot(dtm_AHN4_mba, main="DTM AHN4 MBA", col=height.colors(500))
# 
# 
# ## 3D
# 
# # AHN3
# plot_dtm3d(dtm_AHN3_tin) 
# plot_dtm3d(dtm_AHN3_idw)
# plot_dtm3d(dtm_AHN3_kriging)
# plot_dtm3d(dtm_AHN3_mba)
# 
# # AHN4
# plot_dtm3d(dtm_AHN4_tin) 
# plot_dtm3d(dtm_AHN4_idw)
# plot_dtm3d(dtm_AHN4_kriging)
# plot_dtm3d(dtm_AHN4_mba)

# ## Histograms

# # AHN 3
# hist(dtm_AHN3_tin, main = "TIN", breaks = 500)
# hist(dtm_AHN3_idw, main = "IDW", breaks = 500)
# hist(dtm_AHN3_kriging, main = "Kriging", breaks = 500)
# hist(dtm_AHN3_mba, main = "MBA", breaks = 500)
# 
# # AHN4
# hist(dtm_AHN4_tin, main = "TIN", breaks = 500)
# hist(dtm_AHN4_idw, main = "IDW", breaks = 500)
# hist(dtm_AHN4_kriging, main = "Kriging", breaks = 500)
# hist(dtm_AHN4_mba, main = "MBA", breaks = 500)


### Create slope and aspect of DTM


## AHN3

# # TIN
# dtm_AHN3_tin_prod <- terra::terrain(dtm_AHN3_tin, v = c("slope", "aspect"), unit = "radians")
# dtm_AHN3_tin_hillshade <- shade(slope = dtm_AHN3_tin_prod$slope, aspect = dtm_AHN3_tin_prod$aspect)

# # KnnIDW
# dtm_AHN3_idw_prod <- terra::terrain(dtm_AHN3_idw, v = c("slope", "aspect"), unit = "radians")
# dtm_AHN3_idw_hillshade <- shade(slope = dtm_AHN3_idw_prod$slope, aspect = dtm_AHN3_idw_prod$aspect)
# 
# # Kriging
# dtm_AHN3_kriging_prod <- terra::terrain(dtm_AHN3_kriging, v = c("slope", "aspect"), unit = "radians")
# dtm_AHN3_kriging_hillshade <- shade(slope = dtm_AHN3_kriging_prod$slope, aspect = dtm_AHN3_kriging_prod$aspect)
# 
# # MBA
# dtm_AHN3_mba_prod <- terra::terrain(dtm_AHN3_mba, v = c("slope", "aspect"), unit = "radians")
# dtm_AHN3_mba_hillshade <- shade(slope = dtm_AHN3_mba_prod$slope, aspect = dtm_AHN3_mba_prod$aspect)


## AHN4

# # TIN
# dtm_AHN4_tin_prod <- terra::terrain(dtm_AHN4_tin, v = c("slope", "aspect"), unit = "radians")
# dtm_AHN4_tin_hillshade <- shade(slope = dtm_AHN4_tin_prod$slope, aspect = dtm_AHN4_tin_prod$aspect)
# 
# # KnnIDW
# dtm_AHN4_idw_prod <- terra::terrain(dtm_AHN4_idw, v = c("slope", "aspect"), unit = "radians")
# dtm_AHN4_idw_hillshade <- shade(slope = dtm_AHN4_idw_prod$slope, aspect = dtm_AHN4_idw_prod$aspect)
# 
# # Kriging
# dtm_AHN4_kriging_prod <- terra::terrain(dtm_AHN4_kriging, v = c("slope", "aspect"), unit = "radians")
# dtm_AHN4_kriging_hillshade <- shade(slope = dtm_AHN4_kriging_prod$slope, aspect = dtm_AHN4_kriging_prod$aspect)
# 
# # MBA
# dtm_AHN4_mba_prod <- terra::terrain(dtm_AHN4_mba, v = c("slope", "aspect"), unit = "radians")
# dtm_AHN4_mba_hillshade <- shade(slope = dtm_AHN4_mba_prod$slope, aspect = dtm_AHN4_mba_prod$aspect)


# ## Visualize slope and aspect of DTM
# 
# # AHN3
# plot(dtm_AHN3_tin_hillshade, main = "DTM AHN3 TIN", col =gray(0:30/30), legend = FALSE)
# plot(dtm_AHN3_idw_hillshade, main = "DTM AHN3 IDW", col =gray(0:30/30), legend = FALSE)
# plot(dtm_AHN3_kriging_hillshade, main = "DTM AHN3 Kriging",col =gray(0:30/30), legend = FALSE)
# plot(dtm_AHN3_mba_hillshade, main = "DTM AHN3 Spline", col =gray(0:30/30), legend = FALSE)
# 
# # AHN4
# plot(dtm_AHN4_tin_hillshade, main = "DTM AHN4 TIN", col =gray(0:30/30), legend = FALSE)
# plot(dtm_AHN4_idw_hillshade, main = "DTM AHN4 IDW", col =gray(0:30/30), legend = FALSE)
# plot(dtm_AHN4_kriging_hillshade, main = "DTM AHN4 Kriging",col =gray(0:30/30), legend = FALSE)
# plot(dtm_AHN4_mba_hillshade, main = "DTM AHN4 Spline", col =gray(0:30/30), legend = FALSE)


## Compare difference in DTM

# Get difference in DTM
# div_dtm_tin <- dtm_AHN4_tin - dtm_AHN3_tin
div_dtm_idw <- dtm_AHN4_idw - dtm_AHN3_idw
div_dtm_idw_mask <- dtm_AHN4_idw_mask - dtm_AHN3_idw_mask
# div_dtm_kriging <- dtm_AHN4_kriging - dtm_AHN3_kriging
# div_dtm_mba <- dtm_AHN4_mba - dtm_AHN3_mba

# # Visualize difference in DTM
# plot(div_dtm_tin, main = "Difference in DTM (TIN) between AHN versions", col = height.colors(500))
# plot(div_dtm_idw, main = "Difference in DTM (IDW) between AHN versions", col = height.colors(500))
# plot(div_dtm_kriging, main = "Difference in DTM (Kriging) between AHN versions", col = height.colors(500))
# plot(div_dtm_mba, main = "Difference in DTM (MBA) between AHN versions", col = height.colors(500))

# Write DTM to file
path <- "~/ahncanopygaps/InputData/"
writeRaster(dtm_AHN3_idw, paste0(path, "dtm_AHN3.tif"), overwrite = T)
writeRaster(dtm_AHN4_idw, paste0(path, "dtm_AHN4.tif"), overwrite = T)
writeRaster(dtm_AHN3_idw_mask, paste0(path, "dtm_AHN3_mask.tif"), overwrite = T)
writeRaster(dtm_AHN4_idw_mask, paste0(path, "dtm_AHN4_mask.tif"), overwrite = T)


# ## LAS tiles height normalization with DTM
# 
# # AHN3
# dir_AHN3 <- "~/ahncanopygaps/InputData/GeoTiles/AHN3/normalized"
# if(!dir.exists(dir_AHN3)){
#   dir.create(dir_AHN3)
# }
# 
# opt_output_files(AHN3_catalog) <- paste0(dir_AHN3, "/{*}_norm")
# AHN3_catalog_norm <- normalize_height(AHN3_catalog, dtm_AHN3_idw)
# 
# # AHN4
# dir_AHN4 <- "~/ahncanopygaps/InputData/GeoTiles/AHN4/normalized"
# if(!dir.exists(dir_AHN4)){
#   dir.create(dir_AHN4)
# }
# 
# opt_output_files(AHN4_catalog) <- paste0(dir_AHN4, "/{*}_norm")
# AHN4_catalog_norm <- normalize_height(AHN4_catalog, dtm_AHN4_idw)
