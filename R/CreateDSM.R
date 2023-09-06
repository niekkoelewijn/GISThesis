# Course: GRS-80436 / MSc Thesis
# Project: AHN canopy gap detection
# Author: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: L.C. Vreugdenhil MSc.
# Year: 2022

### Description ###
# Script to create DSM's from AHN version 3 and 4 using different algorithms 
# and to compare them in speed and output. 



### Create DSM with different algorithms


## AHN3

# # p2r, res = 1
# tic("DSM AHN3 p2r low resolution")
# dsm_AHN3_p2r <- rasterize_canopy(AHN3_catalog, res = 1, algorithm = p2r())
# toc() # DSM AHN3 p2r low resolution: 0.971 sec elapsed
# summary(values(dsm_AHN3_p2r)) # NA's: 92
# 
# # p2r, subcircle
# tic("DSM AHN3 p2r with subcircle")
# dsm_AHN3_p2r_subcircle <- rasterize_canopy(AHN3_catalog, res = 1, algorithm = p2r(subcircle = 0.2))
# toc() # DSM AHN3 p2r with subcircle: 2.355 sec elapsed
# summary(values(dsm_AHN3_p2r_subcircle)) # NA's: 21

# p2r, NA fill
tic("DSM AHN3 p2r with NA fill")
dsm_AHN3_p2r_nafill <- rasterize_canopy(AHN3_catalog, res = 1, algorithm = p2r(subcircle = 0.2, na.fill = tin()))
dsm_AHN3_p2r_nafill_mask <- terra::mask(dsm_AHN3_p2r_nafill, study_area_speulderbos)
toc() # DSM AHN3 p2r with NA fill: 435.416 sec elapsed

# # dsmtin, res = 1
# tic("DSM AHN3 dsmtin")
# dsm_AHN3_tin <- rasterize_canopy(AHN3_catalog, res = 1, algorithm = dsmtin())
# toc() # DSM AHN3 dsmtin: 2.259 sec elapsed
# summary(values(dsm_AHN3_tin)) # NA's: 4
# 
# # pitfree, res = 1
# tic("DSM AHN3 pitfree")
# dsm_AHN3_pitfree <- rasterize_canopy(AHN3_catalog, res = 1, algorithm = pitfree())
# toc() # DSM AHN3 pitfree: 7.076 sec elapsed
# summary(values(dsm_AHN3_pitfree)) # NA's: 4



## AHN4

# # p2r, res = 1
# tic("DSM AHN4 p2r low resolution")
# dsm_AHN4_p2r <- rasterize_canopy(AHN4_catalog, res = 1, algorithm = p2r())
# toc() # DSM AHN4 p2r low resolution: 1.674 sec elapsed
# summary(values(dsm_AHN4_p2r)) # NA's: 73
# 
# # p2r, subcircle
# tic("DSM AHN4 p2r with subcircle")
# dsm_AHN4_p2r_subcircle <- rasterize_canopy(AHN4_catalog, res = 1, algorithm = p2r(subcircle = 0.2))
# toc() # DSM AHN4 p2r with subcircle: 2.355 sec elapsed
# summary(values(dsm_AHN4_p2r_subcircle)) # NA's: 16

# p2r, NA fill
tic("DSM AHN4 p2r with NA fill")
dsm_AHN4_p2r_nafill <- rasterize_canopy(AHN4_catalog, res = 1, algorithm = p2r(subcircle = 0.2, na.fill = tin()))
dsm_AHN4_p2r_nafill_mask <- terra::mask(dsm_AHN4_p2r_nafill, study_area_speulderbos)
toc() # DSM AHN4 p2r with NA fill: 1036.854 sec elapsed

# # dsmtin, res = 1
# tic("DSM AHN4 dsmtin")
# dsm_AHN4_tin <- rasterize_canopy(AHN4_study_area_clip, res = 1, algorithm = dsmtin())
# toc() # DSM AHN4 dsmtin: 4.687 sec elapsed
# summary(values(dsm_AHN4_tin)) # NA's: 4
# 
# # pitfree, res = 1
# tic("DSM AHN4 pitfree")
# dsm_AHN4_pitfree <- rasterize_canopy(AHN4_study_area_clip, res = 1, algorithm = pitfree())
# toc() # DSM AHN4 pitfree: 7.4 sec elapsed
# summary(values(dsm_AHN4_pitfree)) # NA's: 4



# ### Visualize DSM's of different algorithmns 
# 
# 
# ## 2D
# 
# # AHN3
# plot(dsm_AHN3_p2r, main="DSM AHN3 p2r", col=height.colors(500))
# plot(dsm_AHN3_p2r_subcircle, main="DSM AHN3 p2r with subcircle", col=height.colors(500))
# plot(dsm_AHN3_p2r_nafill, main="DSM AHN3 p2r with NA fill", col=height.colors(500))
# plot(dsm_AHN3_tin, main="DSM AHN3 dsmtin", col=height.colors(500))
# plot(dsm_AHN3_pitfree, main="DSM AHN3 pitfree", col=height.colors(500))
# 
# # AHN4
# plot(dsm_AHN4_p2r, main="DSM AHN4 p2r", col=height.colors(500))
# plot(dsm_AHN4_p2r_subcircle, main="DSM AHN4 p2r with subcircle", col=height.colors(500))
# plot(dsm_AHN4_p2r_nafill, main="DSM AHN4 p2r with NA fill", col=height.colors(500))
# plot(dsm_AHN4_tin, main="DSM AHN4 dsmtin", col=height.colors(500))
# plot(dsm_AHN4_pitfree, main="DSM AHN4 pitfree", col=height.colors(500))
#
# 
# 
# ## 3D
# 
# # AHN3
# plot_dtm3d(dsm_AHN3_p2r) 
# plot_dtm3d(dsm_AHN3_p2r_subcircle) 
# plot_dtm3d(dsm_AHN3_p2r_nafill)
# plot_dtm3d(dsm_AHN3_tin) 
# plot_dtm3d(dsm_AHN3_pitfree) 
# 
# # AHN4
# plot_dtm3d(dsm_AHN4_p2r) 
# plot_dtm3d(dsm_AHN4_p2r_subcircle) 
# plot_dtm3d(dsm_AHN4_p2r_nafill)
# plot_dtm3d(dsm_AHN4_tin)
# plot_dtm3d(dsm_AHN4_pitfree)
# 
# ## Histograms
# 
# # AHN3
# hist(dsm_AHN3_p2r, main = "p2r, AHN3, res = 1", breaks = 500)
# hist(dsm_AHN3_p2r_subcircle, main = "p2r, AHN3, res = 1, subcircle = 0.2", breaks = 500)
# hist(dsm_AHN3_p2r_nafill, main = "p2r, AHN3, res = 1, subcircle = 0.2, na.fill = tin", breaks = 500)
# hist(dsm_AHN3_tin, main = "TIN, AHN3, res = 1", breaks = 500)
# hist(dsm_AHN3_pitfree, main = "Pitfree, AHN3, res = 1", breaks = 500)
# 
# # AHN4
# hist(dsm_AHN4_p2r, main = "p2r, AHN4, res = 1", breaks = 500)
# hist(dsm_AHN4_p2r_subcircle, main = "p2r, AHN4, res = 1, subcircle = 0.2", breaks = 500)
# hist(dsm_AHN4_p2r_nafill, main = "p2r, AHN4, res = 1, subcircle = 0.2, na.fill = tin", breaks = 500)
# hist(dsm_AHN4_tin, main = "TIN, AHN4, res = 1", breaks = 500)
# hist(dsm_AHN4_pitfree, main = "Pitfree, AHN4, res = 1", breaks = 500)

## Compare difference in DSM

# Get difference in DSM
# div_dsm_p2r <- dsm_AHN4_p2r - dsm_AHN3_p2r
# div_dsm_p2r_subcircle <- dsm_AHN4_p2r_subcircle - dsm_AHN3_p2r_subcircle
div_dsm_p2r_nafill <- dsm_AHN4_p2r_nafill - dsm_AHN3_p2r_nafill
div_dsm_p2r_nafill_mask <- dsm_AHN4_p2r_nafill_mask - dsm_AHN3_p2r_nafill_mask
# div_dsm_tin <- dsm_AHN4_tin - dsm_AHN3_tin
# div_dsm_pitfree <- dsm_AHN4_pitfree - dsm_AHN3_pitfree

# # Visualize difference in DSM
# plot(div_dsm_p2r, main = "Difference in DSM (p2r) between AHN versions", col = height.colors(500))
# plot(div_dsm_p2r_subcircle, main = "Difference in DSM (p2r, subcircle) between AHN versions", col = height.colors(500))
# plot(div_dsm_p2r_nafill, main = "Difference in DSM (p2r, nafill) between AHN versions", col = height.colors(500))
# plot(div_dsm_tin, main = "Difference in DSM (dsmtin) between AHN versions", col = height.colors(500))
# plot(div_dsm_pitfree, main = "Difference in DSM (pitfree) between AHN versions", col = height.colors(500))

# Write DSM to file
writeRaster(dsm_AHN3_p2r_nafill, paste0(path, "dsm_AHN3.tif"), overwrite = T)
writeRaster(dsm_AHN4_p2r_nafill, paste0(path, "dsm_AHN4.tif"), overwrite = T)
writeRaster(dsm_AHN3_p2r_nafill_mask, paste0(path, "dsm_AHN3_mask.tif"), overwrite = T)
writeRaster(dsm_AHN4_p2r_nafill_mask, paste0(path, "dsm_AHN4_mask.tif"), overwrite = T)
