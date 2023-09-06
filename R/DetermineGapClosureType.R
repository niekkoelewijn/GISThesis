# Course: GRS-80436 / MSc Thesis
# Project: AHN canopy gap detection
# Author: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: L.C. Vreugdenhil MSc.
# Year: 2022

### Description ###
# Two types of canopy gap closure exists: on a lateral (horizontal) closure
# or vertical closure. Lateral closure is the closure of a canopy gap due to the
# crown expansion of neighbouring trees. Vertical closure is the closure of a 
# canopy gap due to the regeneration of and growth of trees. In this script,
# it is aimed to distinguish the areas where gap closure has been observed into
# the two categories. If in a gap closure area the difference between the AHN3
# and AHN4 CHM exceeds 2 meter, it is assumed that the gap has been closed due to
# lateral growth, else it is assumed that the gap is closed due to vertical growth.


## Isolate areas of gap closure

# Investigate difference between methods output
#MethodsDifferenceVisual(div_methods)
#freq(div_methods)

# Write function to get gap closure areas
GetGapClosure <- function(gap_div_layer, size = c(1, 10^4)){
  gap_div_layer[gap_div_layer != -1] <- NA
  gaps <- terra::patches(gap_div_layer, directions = 8, allowGaps = F)
  rcl <- terra::freq(gaps)
  rcl[, 3] <- rcl[, 3] * terra::res(gap_div_layer)[1]^2
  z <- terra::classify(gaps, rcl = rcl, right = NA)
  z[is.na(gaps)] <- NA
  gaps[z > size[2]] <- NA
  gaps[z < size[1]] <- NA
  gaps <- terra::patches(gaps, directions = 8, allowGaps = F)
  names(gaps) <- "gaps"
  return(gaps)
}

# Call GetGapClosure function
GapClosureAreas <- GetGapClosure(div_methods)


## Determine CHM difference in gap closure areas

# Mask difference CHM to gap closure areas (GCA)
CHM_difference_GCA <- terra::mask(div_chm, GapClosureAreas)

# Create function to split GCA in lateral and vertical areas
IdentifyGCAType <- function(chm_dif_GCA) {
  m <- c(-Inf, 2, 0,  2, Inf, 1)
  rcl <- matrix(m, ncol = 3, byrow = T)
  chm_dif_GCA <- terra::classify(chm_dif_GCA, rcl = rcl, right = NA)
  return(chm_dif_GCA)
}

# Call IdentifyGCAType function on CHM_difference_GCA
ClassifiedGCA <- IdentifyGCAType(CHM_difference_GCA)

# Visualize output
# plot(ClassifiedGCA, legend = F, main = "Types of gap closure areas",
#      mar=c(2.1, 2.1, 2.1, 6.1), col = c("#111111", "#40bd40"))
# legend("topright", fill = c("#111111", "#40bd40"),
#        legend = c("Vertical gap closure", "Lateral gap closure"),
#        cex = 0.6, bg = "transparent", box.col = "transparent",
#        inset = c(-0.4,0), xpd = T)

# Clip div_methods to study area
ClassifiedGCA_mask <- terra::mask(ClassifiedGCA, study_area_speulderbos)

# Write output to file
path <- "~/ahncanopygaps/InputData/"
writeRaster(ClassifiedGCA, filename = paste0(path, "ClassifiedGCA.tif"), overwrite = T)
writeRaster(ClassifiedGCA_mask, filename = paste0(path, "ClassifiedGCA_mask.tif"), overwrite = T)
