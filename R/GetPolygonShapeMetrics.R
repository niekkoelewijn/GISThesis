# Course: GRS-80436 / MSc Thesis
# Project: AHN canopy gap detection
# Author: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: L.C. Vreugdenhil MSc.
# Year: 2022

### Description ###
# Derive shape metrics of the polygons, and add the metrics to the polygons of 
# the identified canopy gaps.


## Add 3D shape metrics to the new gap polygons

# AHN3
tic("AHN3 polygon shape metrics")
NewGapShapeMetricsAHN3 <- lidR::plot_metrics(las = AHN3_catalog_norm, func = .stdshapemetrics, geometry = new_gaps_polygons)
toc() # 17083 sec elapsed
st_write(NewGapShapeMetricsAHN3, paste0(path, "pointcloud_metrics_shape_norm_AHN3.gpkg"), append = F)

# AHN4
tic("AHN3 polygon shape metrics")
NewGapShapeMetricsAHN4 <- lidR::plot_metrics(las = AHN4_catalog_norm, func = .stdshapemetrics, geometry = new_gaps_polygons)
toc() # 49218 sec elapsed
st_write(NewGapShapeMetricsAHN4, paste0(path, "pointcloud_metrics_shape_norm_AHN4.gpkg"), append = F)

# Rename NewGapShapeMetricsAHN3 columns to prepare for join
RenamedNewGapShapeMetricsAHN3 <- NewGapShapeMetricsAHN3
colnames(RenamedNewGapShapeMetricsAHN3) <- paste("shape_AHN3",colnames(RenamedNewGapShapeMetricsAHN3),sep="_")
colnames(RenamedNewGapShapeMetricsAHN3)[1] <- "ID"
colnames(RenamedNewGapShapeMetricsAHN3)[length(colnames(RenamedNewGapShapeMetricsAHN3))] <- "geometry"
RenamedNewGapShapeMetricsAHN3 <- RenamedNewGapShapeMetricsAHN3 %>% 
  as_tibble() %>% 
  select(-geometry)

# Rename NewGapShapeMetricsAHN3 columns to prepare for join
RenamedNewGapShapeMetricsAHN4 <- NewGapShapeMetricsAHN4
colnames(RenamedNewGapShapeMetricsAHN4) <- paste("shape_AHN4",colnames(RenamedNewGapShapeMetricsAHN4),sep="_")
colnames(RenamedNewGapShapeMetricsAHN4)[1] <- "ID"
colnames(RenamedNewGapShapeMetricsAHN4)[length(colnames(RenamedNewGapShapeMetricsAHN4))] <- "geometry"
RenamedNewGapShapeMetricsAHN4 <- RenamedNewGapShapeMetricsAHN4 %>% 
  as_tibble() %>% 
  select(-geometry)


## Add 2D shape metrics to the new gap polygons

# Function to calculate GSCI (gap shape complexity index) of shape (Bonnet et al., 2015)
GSCI <- function(perimeter, area){
  result <- perimeter / (2 * sqrt(pi*area))
  return(result)
}

# Function to calculate roundness of shape
roundness <- function(perimeter, area){
  result <- perimeter**2 / (4* pi * area)
  return(result)
}

# Function to calculate circularity of shape
circularity <- function(perimeter, area){
  result <- perimeter**2 / area
  return(result)
}

# Function to calculate SI (shape index)
SI <- function(perimeter, area){
  result <- 4*pi* area/perimeter**2
  return(result)
}

# Function to calculate FD (fractal dimension) (Demetriou, See and Stillwell, 2013)
FD <- function(perimeter, area){
  result <- (2 * log(perimeter)) / log(area)
  return(result)
}

# Function to calculate AFF (areal form factor) (Demetriou, See and Stillwell, 2013)
AFF <- function(perimeter, area){
  result <- area / perimeter**2
  return(result)
}

# Function to calculate IER (internal edge ratio)
IER <- function(perimeter, area){
  result <- perimeter / area
  return(result)
}

# Function to calculate solidity of a shape
solidity <- function(polygons){
  result <- st_area(polygons) / st_area(st_convex_hull(polygons))
  return(result)
}

# Function to calculate convexity of a shape
convexity <- function(polygons){
  result <- VLSM::st_perimeter(st_convex_hull(polygons)) / VLSM::st_perimeter(polygons) 
  return(result)
}

# Function to calculate ECD (equivalent circular diameter)
ECD <- function(area){
  result <- 2 * sqrt(area/pi) 
  return(result)
}

# Function to calculate ESV (equivalent spherical volume)
ESV <- function(area){
  result <- (4/3) * pi * sqrt(area/pi)**3
  return(result)
}

# Add new columns using pipes
new_gaps_shape_metrics <- new_gaps_pointcloud_metrics %>% 
  
  # Join with AHN3 shape data
  dplyr::left_join(RenamedNewGapShapeMetricsAHN3,
                   by = "ID") %>% 
  
  # Join with AHN4 shape data
  dplyr::left_join(RenamedNewGapShapeMetricsAHN4,
                   by = "ID") %>% 

  # Add area 
  dplyr::mutate(shape_area = as.vector(sf::st_area(new_gaps_pointcloud_metrics))) %>% 
  
  # Add perimeter
  dplyr::mutate(shape_perimeter = VLSM::st_perimeter(new_gaps_pointcloud_metrics)) %>% 
  
  # Add shape metrics
  dplyr::mutate(shape_GSCI = GSCI(shape_perimeter, shape_area),
                shape_roundness = roundness(shape_perimeter, shape_area),
                shape_circularity = circularity(shape_perimeter, shape_area),
                shape_SI = SI(shape_perimeter, shape_area),
                shape_FD = FD(shape_perimeter, shape_area),
                shape_AFF = AFF(shape_perimeter, shape_area),
                shape_IER = IER(shape_perimeter, shape_area),
                shape_solidity = as.vector(solidity(new_gaps_polygons)),
                shape_convexity = convexity(new_gaps_polygons),
                shape_ECD = ECD(shape_area),
                shape_ESV = ESV(shape_area)) %>%
  
  # Add distance to nearest neighbour 
  dplyr::mutate(nearest_neighbour_1 = nngeo::st_nn(new_gaps_pointcloud_metrics, new_gaps_pointcloud_metrics, 
                                                   k = 2, returnDist = T, progress = T, sparse = T)$dist) %>%
  dplyr::mutate(dist_nn = sapply(nearest_neighbour_1, "[[", 2)) %>% 
  dplyr::select(-nearest_neighbour_1)


## Determine the share of the 5m and 10m buffers of the polygons that intersects 
## with other polygons

# Get percentage overlap of new canopy gap with the 5m buffer 
intersect_buffer_5m <- st_intersection(doughnut_buffer_5m, st_combine(new_gaps_polygons)) %>% 
  mutate(intersect_area_5m = as.vector(st_area(.))) %>%   # create new column with shape area
  dplyr::select(ID, intersect_area_5m) %>%   # only select columns needed to merge
  st_drop_geometry()

# Get percentage overlap of new canopy gap with the 10m buffer 
intersect_buffer_10m <- st_intersection(doughnut_buffer_10m, st_combine(new_gaps_polygons)) %>% 
  mutate(intersect_area_10m = as.vector(st_area(.))) %>%   # create new column with shape area
  dplyr::select(ID, intersect_area_10m) %>%   # only select columns needed to merge
  st_drop_geometry()

# Determine the share of the 5m buffer polygons that intersect with other polygons
new_gaps_polygons_with_bufferoverlap_5m <- new_gaps_polygons %>% 
  dplyr::left_join(intersect_buffer_5m) %>% 
  mutate(area = as.vector(st_area(doughnut_buffer_5m)),
         buffer5m_overlap_relative = intersect_area_5m/area) %>% 
  select(ID, buffer5m_overlap_relative) %>% 
  st_drop_geometry()

# Determine the share of the 10m buffer polygons that intersect with other polygons
new_gaps_polygons_with_bufferoverlap_10m <- new_gaps_polygons %>% 
  dplyr::left_join(intersect_buffer_10m) %>% 
  mutate(area = as.vector(st_area(doughnut_buffer_10m)),
         buffer10m_overlap_relative = intersect_area_10m/area) %>% 
  select(ID, buffer10m_overlap_relative) %>% 
  st_drop_geometry()

# Join the relative buffer overlap to the new_gaps_shape_metrics polygons
new_gaps_shape_metrics <- new_gaps_shape_metrics %>% 
  dplyr::left_join(new_gaps_polygons_with_bufferoverlap_5m) %>% 
  dplyr::left_join(new_gaps_polygons_with_bufferoverlap_10m)

# Write result to GeoPackage
st_write(new_gaps_shape_metrics, paste0(path, "new_gaps_shape_metrics.gpkg"), append = F)
