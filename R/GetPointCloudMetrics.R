# Course: GRS-80436 / MSc Thesis
# Project: AHN canopy gap detection
# Author: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: L.C. Vreugdenhil MSc.
# Year: 2022

### Description ###
# Derive metrics from the point clouds and add them as attributes to the polygons 
# of the identified canopy gaps.

# Reload catalogs
AHN3_catalog_norm <- readLAScatalog("~/ahncanopygaps/InputData/GeoTiles/AHN3/normalized")
AHN4_catalog_norm <- readLAScatalog("~/ahncanopygaps/InputData/GeoTiles/AHN4/normalized")


## AHN3

# Get metrics of new gap polygons
tic("AHN3 polygon metrics")
NewGapMetricsAHN3 <- lidR::plot_metrics(las = AHN3_catalog_norm, func = .stdmetrics, geometry = new_gaps_polygons)
toc() # 17115 sec elapsed
st_write(NewGapMetricsAHN3, paste0(path, "pointcloud_metrics_gappolygons_norm_AHN3.gpkg"), append = F)

# Get metrics of 5m buffer
tic("AHN3 5m buffer metrics")
BufferMetrics5mAHN3 <- lidR::plot_metrics(las = AHN3_catalog_norm, func = .stdmetrics, geometry = doughnut_buffer_5m)
toc() # 19520 sec elapsed
st_write(BufferMetrics5mAHN3, paste0(path, "pointcloud_metrics_5mbuffer_norm_AHN3.gpkg"), append = F)

# Get metrics of 10m buffer
tic("AHN3 10m buffer metrics")
BufferMetrics10mAHN3 <- lidR::plot_metrics(las = AHN3_catalog_norm, func = .stdmetrics, geometry = doughnut_buffer_10m)
toc() # 19228 sec elapsed
st_write(BufferMetrics10mAHN3, paste0(path, "pointcloud_metrics_10mbuffer_norm_AHN3.gpkg"), append = F)


## AHN4

# Get metrics of new gap polygons
tic("AHN4 polygon metrics")
NewGapMetricsAHN4 <- lidR::plot_metrics(las = AHN4_catalog_norm, func = .stdmetrics, geometry = new_gaps_polygons)
toc() # 45062 sec elapsed 
st_write(NewGapMetricsAHN4, paste0(path, "pointcloud_metrics_gappolygons_norm_AHN4.gpkg"), append = F)

# Get metrics of 5m buffer
tic("AHN4 5m buffer metrics")
BufferMetrics5mAHN4 <- lidR::plot_metrics(las = AHN4_catalog_norm, func = .stdmetrics, geometry = doughnut_buffer_5m)
toc() # 48165 sec elapsed 
st_write(BufferMetrics5mAHN4, paste0(path, "pointcloud_metrics_5mbuffer_norm_AHN4.gpkg"), append = F)

# Get metrics of 10m buffer
tic("AHN4 10m buffer metrics")
BufferMetrics10mAHN4 <- lidR::plot_metrics(las = AHN4_catalog_norm, func = .stdmetrics, geometry = doughnut_buffer_10m)
toc() # 48769 sec elapsed
st_write(BufferMetrics10mAHN4, paste0(path, "pointcloud_metrics_10mbuffer_norm_AHN4.gpkg"), append = F)


## Rename columns to prepare data for join

# NewGapMetricsAHN3
RenamedNewGapMetricsAHN3 <- NewGapMetricsAHN3
colnames(RenamedNewGapMetricsAHN3) <- paste(colnames(RenamedNewGapMetricsAHN3),"AHN3",sep="_")
colnames(RenamedNewGapMetricsAHN3)[1] <- "ID"
colnames(RenamedNewGapMetricsAHN3)[length(colnames(RenamedNewGapMetricsAHN3))] <- "geometry"

# BufferMetrics5mAHN3
RenamedBufferMetrics5mAHN3 <- BufferMetrics5mAHN3
colnames(RenamedBufferMetrics5mAHN3) <- paste(colnames(RenamedBufferMetrics5mAHN3),"AHN3",sep="_")
colnames(RenamedBufferMetrics5mAHN3)[1] <- "ID"
colnames(RenamedBufferMetrics5mAHN3)[length(colnames(RenamedBufferMetrics5mAHN3))] <- "geometry"

# BufferMetrics10mAHN3
RenamedBufferMetrics10mAHN3 <- BufferMetrics10mAHN3
colnames(RenamedBufferMetrics10mAHN3) <- paste(colnames(RenamedBufferMetrics10mAHN3),"AHN3",sep="_")
colnames(RenamedBufferMetrics10mAHN3)[1] <- "ID"
colnames(RenamedBufferMetrics10mAHN3)[length(colnames(RenamedBufferMetrics10mAHN3))] <- "geometry"

# NewGapMetricsAHN4
RenamedNewGapMetricsAHN4 <- NewGapMetricsAHN4
colnames(RenamedNewGapMetricsAHN4) <- paste(colnames(RenamedNewGapMetricsAHN4),"AHN4",sep="_")
colnames(RenamedNewGapMetricsAHN4)[1] <- "ID"
colnames(RenamedNewGapMetricsAHN4)[length(colnames(RenamedNewGapMetricsAHN4))] <- "geometry"

# BufferMetrics5mAHN4
RenamedBufferMetrics5mAHN4 <- BufferMetrics5mAHN4
colnames(RenamedBufferMetrics5mAHN4) <- paste(colnames(RenamedBufferMetrics5mAHN4),"AHN4",sep="_")
colnames(RenamedBufferMetrics5mAHN4)[1] <- "ID"
colnames(RenamedBufferMetrics5mAHN4)[length(colnames(RenamedBufferMetrics5mAHN4))] <- "geometry"

# BufferMetrics10mAHN4
RenamedBufferMetrics10mAHN4 <- BufferMetrics10mAHN4
colnames(RenamedBufferMetrics10mAHN4) <- paste(colnames(RenamedBufferMetrics10mAHN4),"AHN4",sep="_")
colnames(RenamedBufferMetrics10mAHN4)[1] <- "ID"
colnames(RenamedBufferMetrics10mAHN4)[length(colnames(RenamedBufferMetrics10mAHN4))] <- "geometry"


## Join on ID to combine data from AHN3 and AHN4

# NewGapMetrics
NewGapMetrics <- sf::st_join(RenamedNewGapMetricsAHN3, RenamedNewGapMetricsAHN4, by = ID, 
                             join = st_within, by = ID, left = ) %>% 
  
  # Remove redundant ID.y column
  select(-ID.y) %>% 
  
  # Rename ID.x to ID
  rename(ID = ID.x)

# BufferMetrics5m
BufferMetrics5m <- sf::st_join(x = RenamedBufferMetrics5mAHN3, y = RenamedBufferMetrics5mAHN4, 
                               join = st_within, by = ID, left = T)%>% 
  
  # Remove redundant ID.y column
  select(-ID.y) %>% 
  
  # Rename ID.x to ID
  rename(ID = ID.x)

# BufferMetrics10m
BufferMetrics10m <- sf::st_join(RenamedBufferMetrics10mAHN3, RenamedBufferMetrics10mAHN4, by = ID,
                               join = st_within, by = ID, left = T)%>% 
  
  # Remove redundant ID.y column
  select(-ID.y) %>% 
  
  # Rename ID.x to ID
  rename(ID = ID.x)


## Add buffer metrics to NewGapMetrics dataset

# Rename 5m buffer metrics to prepare for join
NewNamesBuffer5m <- BufferMetrics5m
colnames(NewNamesBuffer5m) <- paste("Buffer5m",colnames(NewNamesBuffer5m),sep="_")
colnames(NewNamesBuffer5m)[1] <- "ID"
colnames(NewNamesBuffer5m)[length(colnames(NewNamesBuffer5m))] <- "geometry"

# Remove geometry of the 5m buffer 
NewNamesBuffer5mTibble <- NewNamesBuffer5m %>% 
  as_tibble() %>% 
  select(-geometry)

# Rename 10m buffer metrics to prepare for join
NewNamesBuffer10m <- BufferMetrics10m
colnames(NewNamesBuffer10m) <- paste("Buffer10m",colnames(NewNamesBuffer10m),sep="_")
colnames(NewNamesBuffer10m)[1] <- "ID"
colnames(NewNamesBuffer10m)[length(colnames(NewNamesBuffer10m))] <- "geometry"

# Remove geometry of the 10m buffer 
NewNamesBuffer10mTibble <- NewNamesBuffer10m %>% 
  as_tibble() %>% 
  select(-geometry)

# Create one dataset with the 5m buffer information per polygon
new_gaps_pointcloud_metrics <- dplyr::left_join(NewGapMetrics, NewNamesBuffer5mTibble,
                                               by = "ID")

# Create one dataset with the 10m buffer information per polygon
new_gaps_pointcloud_metrics <- dplyr::left_join(new_gaps_pointcloud_metrics, NewNamesBuffer10mTibble,
                                               by = "ID")

# Write result to GeoPackage
st_write(new_gaps_pointcloud_metrics, paste0(path, "new_gaps_pointcloud_metrics.gpkg"), append = F)
