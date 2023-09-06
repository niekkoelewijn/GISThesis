# Course: GRS-80436 / MSc Thesis
# Project: AHN canopy gap detection
# Author: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: L.C. Vreugdenhil MSc.
# Year: 2022

### Description ###
# Script to conduct two methods to identify canopy gaps from the AHN, and to
# try out different combinations of thresholds for canopy gaps. In the first
# method, a comparison is made between the canopy gaps in AHN3 and AHN4. First,
# an adaption of a function from the ForestGapR package (Silva, 2019) is used to
# derive canopy gaps in both versions of the AHN. Second, the canopy gaps of the
# different versions are compared to derive the canopy gap dynamics. In the second
# method, the difference raster of the CHM of AHN3 and AHN4 is used to derive
# newly formed canopy gaps. The first method will further be called the Silva
# method, the second will be called the CHM-subtraction method.
# While playing around with different sets of thresholds, I came to the 
# following conclusions: I think that a maximum for the canopy gap area negatively 
# influences the result, so I won't use this threshold. The maximum canopy gap
# area can make large clear-cuts invisible, and also the thinning operations that
# happened in neighbouring plots. The minimum gap area can reduce the noise in the
# output. A minimum gap area of 10m2 seems to exclude all the small areas with
# canopy height reduction where no tree was lost, while including the areas where
# one individual tree was removed. The maximum vegetation height also has a large
# influence on the gap detection outcome, this threshold determines when a canopy
# gap has become closed again. Using forestry tables, I found out that the average
# height for a tree to become 5cm in diameter is 5m, so that is the threshold that
# I will use. For the CHM-subtraction method, the threshold is difference is still
# to be dermined


### Method 1: Silva method


## Use function from Silva et al. (2019) to get forest gaps with different sets
## of thresholds for vegetation hight and gap size.

# getForestGaps function from the former ForestGapR package, adapted to the 
# "terra" package
getForestGaps <- function(chm_layer, threshold = 10, size = c(1, 10^4)) {
  chm_layer[chm_layer > threshold] <- NA
  chm_layer[chm_layer <= threshold] <- 1
  gaps <- terra::patches(chm_layer, directions = 8, allowGaps = F)
  rcl <- terra::freq(gaps)
  rcl[, 3] <- rcl[, 3] * terra::res(chm_layer)[1]^2
  z <- terra::classify(gaps, rcl = rcl, right = NA)
  z[is.na(gaps)] <- NA
  gaps[z > size[2]] <- NA
  gaps[z < size[1]] <- NA
  gaps <- terra::patches(gaps, directions = 8, allowGaps = F)
  names(gaps) <- "gaps"
  return(gaps)
}

# Call getForestGaps function on AHN3 and AHN4 data sets with default thresholds:
# # Max gap vegetation height = 10m, min gap size = 1 m2, and max gap size = 10*4 m2
# gaps_AHN3_default <- getForestGaps(chm_AHN3)
# gaps_AHN4_default <- getForestGaps(chm_AHN4)
# plot(gaps_AHN3_default, main = "Gaps AHN3, max height = 10m , size = 1-10^4m2")
# plot(gaps_AHN4_default, main = "Gaps AHN4, max height = 10m, size = 1-10^4m2")
# 
# # Max gap vegetation height = 10m, min gap size = 1 m2, and max gap size = 10*99 m2
# gaps_AHN3_10_1_noupper <- getForestGaps(chm_AHN3, threshold = 10, size = c(1, 10^99))
# gaps_AHN4_10_1_noupper <- getForestGaps(chm_AHN4, threshold = 10, size = c(1, 10^99))
# plot(gaps_AHN3_10_1_noupper, main = "Gaps AHN3, max height = 10m , size = 1-10^99m2")
# plot(gaps_AHN4_10_1_noupper, main = "Gaps AHN4, max height = 10m, size = 1-10^99m2")
# 
# # Max gap vegetation height = 10m, min gap size = 20 m2, and max gap size = 10*99 m2
# gaps_AHN3_10_20_noupper <- getForestGaps(chm_AHN3, threshold = 10, size = c(20, 10^99))
# gaps_AHN4_10_20_noupper <- getForestGaps(chm_AHN4, threshold = 10, size = c(20, 10^99))
# plot(gaps_AHN3_10_20_noupper, main = "Gaps AHN3, max height = 10m , size = 20-10^99m2")
# plot(gaps_AHN4_10_20_noupper, main = "Gaps AHN4, max height = 10m, size = 20-10^99m2")

# Max gap vegetation height = 5m, min gap size = 20 m2, and max gap size = 10*99 m2
# gaps_AHN3_5_20_noupper <- getForestGaps(chm_AHN3, threshold = 5, size = c(20, 10^99))
# gaps_AHN4_5_20_noupper <- getForestGaps(chm_AHN4, threshold = 5, size = c(20, 10^99))
# plot(gaps_AHN3_5_20_noupper, main = "Gaps AHN3, max height = 5m , size = 20-10^99m2")
# plot(gaps_AHN4_5_20_noupper, main = "Gaps AHN4, max height = 5m, size = 20-10^99m2")

# Max gap vegetation height = 5m, min gap size = 10 m2, and max gap size = 10*99 m2
gaps_AHN3_5_10_noupper <- getForestGaps(chm_AHN3, threshold = 5, size = c(10, 10^99))
gaps_AHN4_5_10_noupper <- getForestGaps(chm_AHN4, threshold = 5, size = c(10, 10^99))
# plot(gaps_AHN3_5_10_noupper, main = "Gaps AHN3, max height = 5m , size = 10-10^99m2")
# plot(gaps_AHN4_5_10_noupper, main = "Gaps AHN4, max height = 5m, size = 10-10^99m2")

# # Max gap vegetation height = 2m, min gap size = 20 m2, and max gap size = 10*99 m2
# gaps_AHN3_2_20_noupper <- getForestGaps(chm_AHN3, threshold = 2, size = c(20, 10^99))
# gaps_AHN4_2_20_noupper <- getForestGaps(chm_AHN4, threshold = 2, size = c(20, 10^99))
# plot(gaps_AHN3_2_20_noupper, main = "Gaps AHN3, max height = 2m , size = 20-10^99m2")
# plot(gaps_AHN4_2_20_noupper, main = "Gaps AHN4, max height = 2m, size = 20-10^99m2")


## Script to binary visualize which part of the forest is gap, and which is not

# Function to replace NA values in gap layer with 0, and to give all gaps the value 1
getGapsBinaryAHN3 <- function(gap_layer){
  gap_layer[is.na(gap_layer)] <- 0
  rcl <- matrix(c(0,1,0, 1,minmax(gap_layer)[2],1), nrow = 2, ncol = 3, byrow = TRUE)
  gap_layer_binary <- terra::classify(gap_layer, rcl = rcl, right = NA)
  return(gap_layer_binary)
}

# Function to replace NA values in gap layer with 0, and to give all gaps the value 2
getGapsBinaryAHN4 <- function(gap_layer){
  gap_layer[is.na(gap_layer)] <- 0
  rcl <- matrix(c(0,1,0, 1,minmax(gap_layer)[2],2), nrow = 2, ncol = 3, byrow = TRUE)
  gap_layer_binary <- terra::classify(gap_layer, rcl = rcl, right = NA)
  return(gap_layer_binary)
}

# Function to visualize difference between AHN3 and AHN4
gapDifferenceVisual <- function(gaps_div){
  plot(gaps_div, main = "Difference canopy gaps AHN3 and AHN4", legend = F, 
       col = c("#f2f2f2", "#e9b454", "#8acf00", "#00a700"),
       mar = c(2.1, 2.1, 2.1, 6.9))
  legend("topright", fill = c("#f2f2f2", "#e9b454", "#8acf00", "#00a700"),
         legend = c("Gap disapeared", "No gap in both timeframes",
                    "Gap in both timeframes", "New gap"),
         cex = 0.6, bg = "transparent", box.col = "transparent",
         inset = c(0,0), xpd = T)
}

# # Default gap detection thresholds
# gaps_AHN3_default_binary  <- getGapsBinaryAHN3(gaps_AHN3_default)
# plot(gaps_AHN3_default_binary, main = "Gaps AHN3, gap = green, no gap = grey", legend = F)
# 
# gaps_AHN4_default_binary <- getGapsBinaryAHN4(gaps_AHN4_default)
# plot(gaps_AHN4_default_binary, main = "Gaps AHN4, gap = green, no gap = grey", legend = F)
# 
# gaps_div_default <- gaps_AHN4_default_binary - gaps_AHN3_default_binary
# gapDifferenceVisual(gaps_div_default)

# # Max gap vegetation height = 10m, min gap size = 1 m2, and max gap size = 10*99 m2
# gaps_AHN3_10_1_noupper_binary <- getGapsBinaryAHN3(gaps_AHN3_10_1_noupper)
# plot(gaps_AHN3_10_1_noupper_binary, main = "Gaps AHN3, gap = green, no gap = grey", legend = F)
# 
# gaps_AHN4_10_1_noupper_binary <- getGapsBinaryAHN4(gaps_AHN4_10_1_noupper)
# plot(gaps_AHN4_10_1_noupper_binary, main = "Gaps AHN4, gap = green, no gap = grey", legend = F)
# 
# gaps_div_10_1_noupper <- gaps_AHN4_10_1_noupper_binary - gaps_AHN3_10_1_noupper_binary
# gapDifferenceVisual(gaps_div_10_1_noupper)

# # Max gap vegetation height = 10m, min gap size = 20 m2, and max gap size = 10*99 m2
# gaps_AHN3_10_20_noupper_binary <- getGapsBinaryAHN3(gaps_AHN3_10_20_noupper)
# plot(gaps_AHN3_10_20_noupper_binary, main = "Gaps AHN3, gap = green, no gap = grey", legend = F)
# 
# gaps_AHN4_10_20_noupper_binary <- getGapsBinaryAHN4(gaps_AHN4_10_20_noupper)
# plot(gaps_AHN4_10_20_noupper_binary, main = "Gaps AHN4, gap = green, no gap = grey", legend = F)
# 
# gaps_div_10_20_noupper <- gaps_AHN4_10_20_noupper_binary - gaps_AHN3_10_20_noupper_binary
# gapDifferenceVisual(gaps_div_10_20_noupper)

# Max gap vegetation height = 5m, min gap size = 20 m2, and max gap size = 10*99 m2
# gaps_AHN3_5_20_noupper_binary <- getGapsBinaryAHN3(gaps_AHN3_5_20_noupper)
# plot(gaps_AHN3_5_20_noupper_binary, main = "Gaps AHN3, gap = green, no gap = grey", legend = F)
# 
# gaps_AHN4_5_20_noupper_binary <- getGapsBinaryAHN4(gaps_AHN4_5_20_noupper)
# plot(gaps_AHN4_5_20_noupper_binary, main = "Gaps AHN4, gap = green, no gap = grey", legend = F)
# 
# gaps_div_5_20_noupper <- gaps_AHN4_5_20_noupper_binary - gaps_AHN3_5_20_noupper_binary
# gapDifferenceVisual(gaps_div_5_20_noupper)

# Max gap vegetation height = 5m, min gap size = 10 m2, and max gap size = 10*99 m2
gaps_AHN3_5_10_noupper_binary <- getGapsBinaryAHN3(gaps_AHN3_5_10_noupper)
#gaps_AHN3_5_10_noupper_binary <- terra::mask(gaps_AHN3_5_10_noupper_binary, study_area_speulderbos)
# plot(gaps_AHN3_5_10_noupper_binary, main = "Gaps AHN3, gap = green, no gap = grey", legend = F)

gaps_AHN4_5_10_noupper_binary <- getGapsBinaryAHN4(gaps_AHN4_5_10_noupper)
#gaps_AHN4_5_10_noupper_binary <- terra::mask(gaps_AHN4_5_10_noupper_binary, study_area_speulderbos)
# plot(gaps_AHN4_5_10_noupper_binary, main = "Gaps AHN4, gap = green, no gap = grey", legend = F)

gaps_div_5_10_noupper <- gaps_AHN4_5_10_noupper_binary - gaps_AHN3_5_10_noupper_binary
# gapDifferenceVisual(gaps_div_5_10_noupper)

# # Max gap vegetation height = 2m, min gap size = 20 m2, and max gap size = 10*99 m2
# gaps_AHN3_2_20_noupper_binary <- getGapsBinaryAHN3(gaps_AHN3_2_20_noupper)
# plot(gaps_AHN3_2_20_noupper_binary, main = "Gaps AHN3, gap = green, no gap = grey", legend = F)
# 
# gaps_AHN4_2_20_noupper_binary <- getGapsBinaryAHN4(gaps_AHN4_2_20_noupper)
# plot(gaps_AHN4_2_20_noupper_binary, main = "Gaps AHN4, gap = green, no gap = grey", legend = F)
# 
# gaps_div_2_20_noupper <- gaps_AHN4_2_20_noupper_binary - gaps_AHN3_2_20_noupper_binary
# gapDifferenceVisual(gaps_div_2_20_noupper)


## Get new gaps from difference between gaps AHN3 and AHN4

# Function to select new gaps, and to give each patch a seperate label
GetNewGaps <- function(gap_div_layer, size = c(1, 10^4)){
  gap_div_layer[gap_div_layer != 2] <- NA
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

# # Default settings
# NewGaps_default <- GetNewGaps(gap_div_layer = gaps_div_default)
# plot(NewGaps_default, legend = F, main = "New gaps, height = 10m, size = 1-10^4m2")

# # Max gap vegetation height = 10m, min gap size = 1 m2, and max gap size = 10*99 m2
# NewGaps_10_1_noupper <- GetNewGaps(gap_div_layer = gaps_div_10_1_noupper, size = c(1, 10^99))
# plot(NewGaps_10_1_noupper, legend = F, main = "New gaps, height = 10m, size = 1-10^99m2")

# # Max gap vegetation height = 10m, min gap size = 20 m2, and max gap size = 10*99 m2
# NewGaps_10_20_noupper <- GetNewGaps(gap_div_layer = gaps_div_10_20_noupper, size = c(20, 10^99))
# plot(NewGaps_10_20_noupper, legend = F, main = "New gaps, height = 10m, size = 20-10^99m2")

# Max gap vegetation height = 5m, min gap size = 20 m2, and max gap size = 10*99 m2
# NewGaps_5_20_noupper <- GetNewGaps(gap_div_layer = gaps_div_5_20_noupper, size = c(20, 10^99))
# plot(NewGaps_5_20_noupper, legend = F, main = "New gaps, height = 5m, size = 20-10^99m2")

# Max gap vegetation height = 5m, min gap size = 10 m2, and max gap size = 10*99 m2
NewGaps_5_10_noupper <- GetNewGaps(gap_div_layer = gaps_div_5_10_noupper, size = c(10, 10^99))
# plot(NewGaps_5_10_noupper, legend = F, main = "New gaps, height = 5m, size = 10-10^99m2")

# # Max gap vegetation height = 2m, min gap size = 20 m2, and max gap size = 10*99 m2
# NewGaps_2_20_noupper <- GetNewGaps(gap_div_layer = gaps_div_2_20_noupper, size = c(20, 10^99))
# plot(NewGaps_2_20_noupper, legend = F, main = "New gaps, height = 2m, size = 20-10^99m2")



### Method 2: CHM difference method


## Get canopy gap dynamics from difference in CHM

# Default
# div_gaps_default <- getForestGaps(div_chm, threshold = -10, size = c(1, 10^4))
# plot(div_gaps_default, legend = F, main = "Gaps from div CHM, height = 10m, size = 1-10^4m2")

# Min height difference == 10m, min gap size = 1 m2, and max gap size = 10*99 m2
# div_gaps_10_1_noupper <- getForestGaps(div_chm, threshold = -10, size = c(1, 10^99))
# plot(div_gaps_10_1_noupper, legend = F, main = "Gaps from div CHM, height = 10m, size = 1-10^99m2")

# Min height difference = 10m, min gap size = 20 m2, and max gap size = 10*99 m2
# div_gaps_10_20_noupper <- getForestGaps(div_chm, threshold = -10, size = c(20, 10^99))
# plot(div_gaps_10_20_noupper, legend = F, main = "Gaps from div CHM, height = 10m, size = 20-10^99m2")

# Min height difference = 0.4m, min gap size = 20 m2, and max gap size = 10*99 m2
# div_gaps_040_20_noupper <- getForestGaps(div_chm, threshold = -0.4, size = c(20, 10^99))
# plot(div_gaps_040_20_noupper, legend = F, main = "Gaps from div CHM, height = 0.4m, size = 20-10^99m2")

# Min height difference = 5m, min gap size = 20 m2, and max gap size = 10*99 m2
# div_gaps_5_20_noupper <- getForestGaps(div_chm, threshold = -5, size = c(20, 10^99))
# plot(div_gaps_5_20_noupper, legend = F, main = "Gaps from div CHM, height = 5m, size = 20-10^99m2")

# Min height difference = 5m, min gap size = 10 m2, and max gap size = 10*99 m2
# div_gaps_5_10_noupper <- getForestGaps(div_chm, threshold = -5, size = c(10, 10^99))
# plot(div_gaps_5_10_noupper, legend = F, main = "Gaps from div CHM, height = 5m, size = 10-10^99m2")

# Min height difference = 2m, min gap size = 20 m2, and max gap size = 10*99 m2
# div_gaps_2_20_noupper <- getForestGaps(div_chm, threshold = -2, size = c(20, 10^99))
# plot(div_gaps_2_20_noupper, legend = F, main = "Gaps from div CHM, height = 3m, size = 20-10^99m2")

# Min height difference = 2m, min gap size = 10 m2, and max gap size = 10*99 m2
div_gaps_2_10_noupper <- getForestGaps(div_chm, threshold = -2, size = c(10, 10^99))
# plot(div_gaps_2_10_noupper, legend = F, main = "Gaps from div CHM, height = 2m, size = 10-10^99m2")


## Get binary verion of output of method 2

# Function to replace NA values in gap layer with 0, and to give all gaps the value 6
getGapsBinaryDiv <- function(gap_layer){
  gap_layer[is.na(gap_layer)] <- 0
  rcl <- matrix(c(0,1,0, 1,minmax(gap_layer)[2],3), nrow = 2, ncol = 3, byrow = T)
  gap_layer_binary <- terra::classify(gap_layer, rcl = rcl, right = NA)
  return(gap_layer_binary)
}

# Create binary version of gaps identified with method 2
gaps_div_CHM_binary <- getGapsBinaryDiv(div_gaps_2_10_noupper)
# gaps_div_CHM_binary <- terra::mask(gaps_div_CHM_binary, study_area_speulderbos)
# plot(gaps_div_CHM_binary, main = "Leitold gaps, gap = green, no gap = grey", legend = F)



### Analyse difference between canopy gap detection method 1 and 2


## Write necessary functions to determine and to visualise difference between method 1 and 2 

# Function to determine difference between method 1 and 2 
GetMethodDifference <- function(output_method1, output_method2){
  div_methods <- output_method1 + output_method2
  return(div_methods)
}

# Function to visualize difference between method 1 and 2 
MethodsDifferenceVisual <- function(gaps_div){
  plot(gaps_div, main = "Difference between Silva method and \nCHM-subtraction method", legend = F, 
       col = c("#f2f2f2", "#e9b454", "#8acf00", "purple", "darkgreen", "pink", "black"),
       mar = c(2.1, 2.1, 2.1, 7.1))
  legend("topright", fill = c("#f2f2f2", "#e9b454", "#8acf00", "purple","darkgreen", "pink", "black"),
         legend = c("Gap disapeared in AHN4", "No gap detected in both \nAHN versions",
                    "Gap in both AHN versions", 
                    "New gap in AHN4 with \nSilva method", 
                    "New gap in AHN4 with \nCHM-subtraction method",
                    "Gap in both AHN versions, \nbut detected as gap with\nCHM-subtraction method",
                    "New gap in AHN4 with \nboth methods"),
         cex = 0.6, bg = "transparent", box.col = "transparent",
         inset = c(-0.55,0), xpd = T)
}

# Call functions
div_methods <- GetMethodDifference(gaps_div_5_10_noupper, gaps_div_CHM_binary)
# MethodsDifferenceVisual(div_methods)


## Script to replace incorrect purple cells that should be light green cells

# Create raster with cells with value 1 where the div_methods raster had value 2
div_methods_Only2 <- div_methods == 2
div_methods_Only2 <- terra::classify(div_methods_Only2, cbind(F, NA), right = F)

# Get the places where both CHMs are below 5m
chm_AHN3_below5 <- chm_AHN3 < 5
chm_AHN4_below5 <- chm_AHN4 < 5

# Remove False
chm_AHN3_below5 <- terra::classify(chm_AHN3_below5, cbind(F, NA), right = F)
chm_AHN4_below5 <- terra::classify(chm_AHN4_below5, cbind(F, NA), right = F)

# Mask chm_AHN4_below5 over chm_AHN3_below5 to get areas were both are below 5m
both_chm_below5 <- terra::mask(chm_AHN4_below5, chm_AHN3_below5)

# Mask div_methods_Only2 with both_chm_below5
incorrect_purple_to_green <- terra::mask(div_methods_Only2, both_chm_below5)
incorrect_purple_to_green <- terra::classify(incorrect_purple_to_green, cbind(NA, 0), right = F)

# Update div_methods
div_methods_fix1 <- div_methods - incorrect_purple_to_green


## Script to replace incorrect purple cells that should be blackm but are
## given the color orange, because they do not have an area of 10m2

# Get raster with only value 2
CorrectionRast <- chm_AHN3
CorrectionRastOnly2s <- terra::classify(CorrectionRast, cbind(-Inf, Inf, 2), right = F)

# Mask the correction raster to the places where the div_methods raster had value 2
div_methods_Only2 <- div_methods == 2
div_methods_Only2 <- terra::classify(div_methods_Only2, cbind(F, NA), right = F)
CorrectionRastOnly2s <- terra::mask(CorrectionRastOnly2s, div_methods_Only2)

# Get the places where the CHM of AHN3 is above 5, the CHM of AHN4 is below 5,
# and where the difference CHM is less than -2
chm_AHN3_above5 <- chm_AHN3 > 5
chm_AHN4_below5 <- chm_AHN4 < 5
div_chm_below2 <- div_chm < -2

# Remove False
chm_AHN3_above5 <- terra::classify(chm_AHN3_above5, cbind(F, NA), right = F)
chm_AHN4_below5 <- terra::classify(chm_AHN4_below5, cbind(F, NA), right = F)
div_chm_below2 <- terra::classify(div_chm_below2, cbind(F, NA), right = F)

# Mask chm_AHN4_below5 over chm_AHN3_above5 to get areas were Silva detected a gap
SilvaGap <- terra::mask(chm_AHN4_below5, chm_AHN3_above5)

# Mask SilvaGap over div_chm_below2 to get areas were both methods detected a gap
BothGap <- terra::mask(div_chm_below2, SilvaGap)

# Mask CorrectionRastOnly2s with both_chm_below5
incorrect_purple_to_orange <- terra::mask(CorrectionRastOnly2s, BothGap)
incorrect_purple_to_orange <- terra::classify(incorrect_purple_to_orange, cbind(NA, 0), right = F)

# Update div_methods
div_methods_fix2 <- div_methods_fix1 - incorrect_purple_to_orange


## Script to replace incorrect white cells that should be light green cells

# Get raster with only value 2
CorrectionRast <- chm_AHN3
CorrectionRastOnly2s <- terra::classify(CorrectionRast, cbind(-Inf, Inf, 2), right = F)

# Mask the correction raster to the places where the div_methods raster had value -1
div_methods_Onlyminus1 <- div_methods_fix2 == -1
div_methods_Onlyminus1 <- terra::classify(div_methods_Onlyminus1, cbind(F, NA), right = F)
CorrectionRastOnly2s <- terra::mask(CorrectionRastOnly2s, div_methods_Onlyminus1)

# Mask CorrectionRastOnly2s with both_chm_below5
incorrect_white_to_green <- terra::mask(CorrectionRastOnly2s, both_chm_below5)
incorrect_white_to_green <- terra::classify(incorrect_white_to_green, cbind(NA, 0), right = F)

# Update div_methods
div_methods_fix3 <- div_methods_fix2 + incorrect_white_to_green


## Script to replace incorrect darkgreen cells that should be black cells

# Get raster with only value 2
CorrectionRast <- chm_AHN3
CorrectionRastOnly2s <- terra::classify(CorrectionRast, cbind(-Inf, Inf, 2), right = F)

# Mask the correction raster to the places where the div_methods raster had value 
div_methods_Only3 <- div_methods_fix2 == 3
div_methods_Only3 <- terra::classify(div_methods_Only3, cbind(F, NA), right = F)
CorrectionRastOnly2s <- terra::mask(CorrectionRastOnly2s, div_methods_Only3)

# Mask CorrectionRastOnly2s with SilvaGap
incorrect_darkgreen_to_black <- terra::mask(CorrectionRastOnly2s, SilvaGap)
incorrect_darkgreen_to_black <- terra::classify(incorrect_darkgreen_to_black, cbind(NA, 0), right = F)

# Update div_methods
div_methods_fix4 <- div_methods_fix3 + incorrect_darkgreen_to_black

# Overwrite div_method with the optimized version
div_methods <- div_methods_fix4


## Analyse results

# Call the created funtions
# MethodsDifferenceVisual(div_methods)

# Get zonal stats
div_methods_CHMDifferenceStats <- terra::zonal(div_chm, div_methods, fun = mean)
div_methods_CHM3Stats <- terra::zonal(chm_AHN3, div_methods, fun = mean)
div_methods_CHM4Stats <- terra::zonal(chm_AHN4, div_methods, fun = mean)


## Make the div_methods SpatRaster a catergorical raster

# Assign categories to div_methods
div_methods_categories <- categories(div_methods, value = tibble(ID = freq(div_methods)$value,
                                                                 category = c("Disapeared gap",
                                                                              "No gap detected",
                                                                              "Remaining gap", 
                                                                              "New gap with Silva method", 
                                                                              "New gap with CHM-subtraction method",
                                                                              "Remaining gap and new gap with CHM-subtraction method",
                                                                              "New gap with both methods")))
                                                      
# Check levels
levels(div_methods_categories)

# Hist of zonal stats
# plot(div_methods_CHMDifferenceStats$gaps, div_methods_CHMDifferenceStats$Z, main = "mean difference in Z per class",
#      xlab = c("Gap disapeared in AHN4", "No gap detected in both \nAHN versions",
#               "Gap in both AHN versions", 
#               "New gap in AHN4 with \nSilva method", 
#               "New gap in AHN4 with \ndifference-CHM method",
#               "New gap in AHN4 with \nboth methods"))
# plot(div_methods_CHM3Stats$gaps, div_methods_CHM3Stats$Z, main = "mean Z in CHM of AHN3 per class")
# plot(div_methods_CHM4Stats$gaps, div_methods_CHM4Stats$Z, main = "mean Z in CHM of AHN4 per class")

# Clip div_methods to study area
div_methods <- rast("~/ahncanopygaps/InputData/difference_methods.tif")
div_methods_mask <- terra::mask(div_methods, study_area_speulderbos)

# # Write output to file
# path <- "~/ahncanopygaps/InputData/"
# terra::writeRaster(div_methods, paste0(path, "difference_methods.tif"), overwrite = T)
# terra::writeRaster(div_methods_mask, paste0(path, "difference_methods_mask.tif"), overwrite = T)
