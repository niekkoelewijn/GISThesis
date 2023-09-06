# Course: GRS-80436 / MSc Thesis
# Project: AHN canopy gap detection
# Author: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: L.C. Vreugdenhil MSc.
# Year: 2022

### Description ###
# This project aims to (1) identify canopy gaps from the Actueel Hoogtebestand 
# Nederland (AHN) dataset, and (2) to make a comparison between canopy gap
# dynamics in managed and unmanaged forests. The project consists out of four
# different steps, and through this main script all different steps can be 
# executed.


### Research question 1: canopy gap detection method comparison ###
library(tictoc)
tic("Speed scripts RQ1:")

# Step 1: Load and clip test tiles into R
tic("Step 1")
source("~/ahncanopygaps/R/DownloadClipTiles.R")
toc() # Step 1: 3.915 sec elapsed

# Step 2: Create DTM
tic("Step 2")
source("~/ahncanopygaps/R/CreateDTM.R")
toc() # Step 2: 1680.703 sec elapsed

# Step 3: Create DSM
tic("Step 3")
source("~/ahncanopygaps/R/CreateDSM.R")
toc() # Step 3: 1478.258 sec elapsed

# Step 4: Create CHM
tic("Step 4")
source("~/ahncanopygaps/R/CreateCHM.R")
toc() # Step 4: 9.793 sec elapsed

# Step 5: Identify canopy gaps
tic("Step 5")
source("~/ahncanopygaps/R/IdentifyCanopyGaps.R")
toc() # Step 5: 703.083 sec elapsed

# Step 6: Determine type of gap closure
tic("Step 6")
source("~/ahncanopygaps/R/DetermineGapClosureType.R")
toc() # Step 6: 22.492 sec elapsed

# Step 7: Vectorize new canopy gaps
tic("Step 7")
source("~/ahncanopygaps/R/VectorizeGaps.R")
toc() # Step 7: 87.618 sec elapsed

# Step 7: Validate identified canopy gaps
tic("Step 8")
source("~/ahncanopygaps/R/ValidateCanopyGaps.R")
toc()

# Step 8: Field visit small study area
# tic("Step 8")
# source("~/ahncanopygaps/R/FieldVisit.R")
# toc() # Step 7: 3.652 sec elapsed

toc() # Speed script RQ1: 3898.279 sec elapsed



### Research question 2: Identify canopy gap classes ###
tic("Speed scripts RQ2: ")

# Step 1: Add metrics from point clouds to new canopy gaps polygons
tic("Step 1")
source("~/ahncanopygaps/R/GetPointCloudMetrics.R")
toc() # Step 1: 1079.959 sec elapsed

# Step 2: Add metrics of shape to new canopy gaps polygons
tic("Step 2")
source("~/ahncanopygaps/R/GetPolygonShapeMetrics.R")
toc() # Step 2: 16.102 sec elapsed

# Step 3: Add metrics of relevant input rasters to new canopy gaps polygons
tic("Step 3")
source("~/ahncanopygaps/R/GetRasterMetrics.R")
toc() # Step 3: 3.819 sec elapsed

# Step 4: Pre-process State forestry data
tic("Step 4")
source("~/ahncanopygaps/R/TrainingPolygonData.R")
toc() # Speed step 4: 11.626 sec elapsed

# Step 5: Add metrics on a forest stand level to new canopy gaps
tic("Step 5")
source("~/ahncanopygaps/R/GetForestPlotMetrics.R")
toc() # Step 5: 13.288 sec elapsed

# Step 6: Create random forest model
tic("Step 6")
source("~/ahncanopygaps/R/Classification")
toc()

toc() # Speed script RQ2: 1398.138 sec elapsed



### Research Aim 3: Compare managed and unmanaged forests ###
tic("Speed scripts RQ3: ")

# Step 1: Create histograms to compare groups
tic("Step 1")
source("~/ahncanopygaps/R/Histograms.R")
toc() # Step 1: 1079.959 sec elapsed

# Step 2: Create boxplots to compare groups
tic("Step 2")
source("~/ahncanopygaps/R/Boxplots.R")
toc() # Step 2: 1079.959 sec elapsed

# Step 3: Retrieve statistics of gaps in managed and unmanaged plots
tic("Step 3")
source("~/ahncanopygaps/R/StatisticalTests.R")
toc() # Step 3: 1079.959 sec elapsed

# Step 4: Create barcharts to compare groups
tic("Step 4")
source("~/ahncanopygaps/R/Barcharts.R")
toc() # Step 4: 1079.959 sec elapsed

toc() # Speed script RQ3: 1398.138 sec elapsed
