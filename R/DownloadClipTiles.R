# Course: GRS-80436 / MSc Thesis
# Project: AHN canopy gap detection
# Author: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: L.C. Vreugdenhil MSc.
# Year: 2022

### Description ###
# The point cloud data that is used in this project is downloaded from GeoTiles,
# an interactive web application that helps to download chucks of different
# versions of the AHN. The data is downloaded as a LAZ file. To increase the 
# speed of the scripts in this project, I decompressed these files to las files
# (see LAZtoLAS.py). Tiles downloaded from Geotiles contain a buffer, and this
# buffer causes some problems in deriving plot metrics. Therefore, in this script,
# the buffer is removed from the tiles.


## Load required packages

# Function to install and load packages
CheckPackages <- function(packagelist){
  for (package in packagelist){
    if(!require(package, character.only = TRUE)){
      install.packages(package, character.only = TRUE)
      require(package, character.only = TRUE)
    }
    library(package, character.only = TRUE)
  }
}

# Call ChackPackages
CheckPackages(c("lidR", "sf", "dplyr", "ggmap", "sp", "rgdal", "future", "timeDate", 
                "terra", "rayshader", "tictoc", "colorRamps", "MBA", "magick",
                "viridis", "tidyverse", "VLSM", "exactextractr", "mapview",
                "randomForest", "caret", "car", "ggsignif", "ggpubr", "FSA",
                "Hmisc", "sjstats", "survey", "WeightIt", "ggstatsplot",
                "ggrepel"))

# Load study area into R
path_study_area <- "~/ahncanopygaps/InputData/NewStudyArea.gpkg"
study_area_speulderbos <- st_read(path_study_area) %>% 
  st_transform(7415)



# ### Remove buffer from tiles
# 
# 
# ## AHN3
# 
# # Create new directory for the filtered tiles
# no_overlap_dir <- "~/ahncanopygaps/InputData/GeoTiles/AHN3/filter"
# if (!dir.exists(no_overlap_dir)){
#   dir.create(no_overlap_dir, showWarnings = FALSE)
# }
# 
# # List the las tiles
# path = "~/ahncanopygaps/InputData/GeoTiles/AHN3/las/"
# AHN3_lasfiles <- list.files(path)
# 
# # Loop over the files
# for(i in seq_along(AHN3_lasfiles)){
#   print(paste0("starting with: ", AHN3_lasfiles[i]))
#   las_filtered <- readLAS(paste0(path, AHN3_lasfiles[i]), filter = "-drop_overlap")
#   writeLAS(las_filtered, paste0(no_overlap_dir, "/", AHN3_lasfiles[i]))
#   rm(las_filtered)
#   print(paste0("done with: ", AHN3_lasfiles[i]))
# }
# 
# 
# ## AHN4
# 
# # Create new directory for the filtered tiles
# no_overlap_dir <- "~/ahncanopygaps/InputData/GeoTiles/AHN4/filter"
# if (!dir.exists(no_overlap_dir)){
#   dir.create(no_overlap_dir, showWarnings = FALSE)
# }
# 
# # List the las tiles
# path = "~/ahncanopygaps/InputData/GeoTiles/AHN4/las/"
# AHN4_lasfiles <- list.files(path)
# 
# # Loop over the files to create filtered las tiles
# for(i in seq_along(AHN4_lasfiles)){
#   print(paste0("starting with: ", AHN4_lasfiles[i]))
#   las_filtered <- readLAS(paste0(path, AHN4_lasfiles[i]), filter = "-drop_overlap")
#   writeLAS(las_filtered, paste0(no_overlap_dir, "/", AHN4_lasfiles[i]))
#   rm(las_filtered)
#   print(paste0("done with: ", AHN4_lasfiles[i]))
# }



### Create LAS catalog for both AHN versions


## AHN3

# Create AHN3 catalog
AHN3_catalog <- lidR::readLAScatalog("~/ahncanopygaps/InputData/GeoTiles/AHN3/filter")

# # Visualize AHN3 catalog and study area
# plot(AHN3_catalog, chunk = T)
# plot(st_geometry(study_area_speulderbos), add = T)


## AHN4

# Create AHN4 catalog
AHN4_catalog <- lidR::readLAScatalog("~/ahncanopygaps/InputData/GeoTiles/AHN4/filter")



