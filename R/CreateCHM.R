# Course: GRS-80436 / MSc Thesis
# Project: AHN canopy gap detection
# Author: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: L.C. Vreugdenhil MSc.
# Year: 2022

### Description ###
# Script to create CHM's from AHN version 3 and 4 using different combinations 
# of algorithms of DTM and DSM. To create a DTM, 4 algorithms can be used: tin,
# idw, kriging and spline. The first three algorithms were shown to produce very
# simular outputs in the plots and histogram. The MBA seemed to have a more
# coarse resolution, and the difference between the DTM of AHN3 and AHN4 was 
# remarkebly high compared to the other 3 algorithms. Kriging takes way more time
# to compute than TIN and IDW, and does not or barely improve the interpolation
# result. Because IDW is simply by far the fastest algorithm, I will use this
# DTM in the CHM production.
# For the DSM, the pitfree algorithm is the slowest, even though the output is
# equal to the much quicker dsmtin algorithm. The p2r algorithm with a subcircle
# of 0.15 and a tin NA filter is slightly faster than the dsmtin algorithm. 
# Besides, the p2r algorithm produces less pits in the DSM, which increases the 
# realism. I will use a resolution of 1 instead of 0.5 to create the DSM. This
# makes the output slightly more coarse, but the computiation spead decreases
# significantly, and the difference in output is for my application not big
# enough to increase the resolution to 0.5. The speed of DSMTIN and p2r with
# NA filter is almost equal. The output of the p2r algorithm seems to be more
# realistic for my application, so I will choose this algorithm with NA filter.
# To conclude: I will make use of the IDW for the DTM and the p2r with a sub-
# circle and a NA-filter to create the CHM.


# Calculate the CHM from the choosen DTM and DSM
chm_AHN3 <- dsm_AHN3_p2r_nafill - dtm_AHN3_idw
chm_AHN4 <- dsm_AHN4_p2r_nafill - dtm_AHN4_idw
chm_AHN3_mask <- dsm_AHN3_p2r_nafill_mask - dtm_AHN3_idw_mask
chm_AHN4_mask <- dsm_AHN4_p2r_nafill_mask - dtm_AHN4_idw_mask

# Replace negative CHM values by 0
chm_AHN3 <- terra::classify(chm_AHN3, cbind(-Inf, 0, 0), right = F)
chm_AHN4 <- terra::classify(chm_AHN4, cbind(-Inf, 0, 0), right = F)
chm_AHN3_mask <- terra::classify(chm_AHN3_mask, cbind(-Inf, 0, 0), right = F)
chm_AHN4_mask <- terra::classify(chm_AHN4_mask, cbind(-Inf, 0, 0), right = F)

## Visualize CHM 

# # 2D
# plot(chm_AHN3, main = "CHM AHN3", col = height.colors(500))
# plot(chm_AHN4, main = "CHM AHN4", col = height.colors(500))
# 
# # 3D
# plot_dtm3d(chm_AHN3)
# plot_dtm3d(chm_AHN4)
# 
# # Histograms
# hist(chm_AHN3, main = "CHM, AHN3", breaks = 500)
# hist(chm_AHN4, main = "CHM, AHN4", breaks = 500)


## Investigate development CHM 

# Calculate difference between CHM AHN3 and AHN4
div_chm <- chm_AHN4 - chm_AHN3
div_chm_mask <- chm_AHN4_mask - chm_AHN3_mask

# # Plot difference 2D
# plot(div_chm, main = "Difference CHM AHN3 & AHN4", col = height.colors(500))

# # Plot difference 3D
# plot_dtm3d(div_chm)
# 
# # Hist difference CHNM
# hist(div_chm, main = "Difference CHM AHN3 & AHN4", breaks = 500)

# Write CHM to file
writeRaster(chm_AHN3, paste0(path, "CHM_AHN3.tif"), overwrite = T)
writeRaster(chm_AHN4, paste0(path, "CHM_AHN4.tif"), overwrite = T)
writeRaster(div_chm, paste0(path, "CHM_Difference.tif"), overwrite = T)
writeRaster(chm_AHN3_mask, paste0(path, "CHM_AHN3_mask.tif"), overwrite = T)
writeRaster(chm_AHN4_mask, paste0(path, "CHM_AHN4_mask.tif"), overwrite = T)
writeRaster(div_chm_mask, paste0(path, "CHM_Difference_mask.tif"), overwrite = T)
