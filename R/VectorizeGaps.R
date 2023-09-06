# Course: GRS-80436 / MSc Thesis
# Project: AHN canopy gap detection
# Author: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: L.C. Vreugdenhil MSc.
# Year: 2022

### Description ###
# Vectorizing the identified canopy gaps. These polygons of the canopy gaps can
# later be used for further analyses; to derive canopy gap metrics.


## Select areas from the difference in methods raster that are new gaps

# Get purple, dark green, pink, and black areas from div_methods raster
new_gaps <- div_methods_mask
new_gaps[new_gaps < 2] <- NA
new_gaps[new_gaps >= 2] <- 1

# Give the new gaps a unique ID
new_gaps <- terra::patches(new_gaps, directions = 8, allowGaps = T)

# Remove patches smaller than 10m2
rcl <- terra::freq(new_gaps)
z <- terra::classify(new_gaps, rcl = rcl, right = NA)
z[is.na(new_gaps)] <- NA
new_gaps[z < 10] <- NA


## Vectorize the new gaps

# use the terra as.polygons to vectorize the new gaps
new_gaps_polygons <- terra::as.polygons(new_gaps) %>% 
  st_as_sf() %>% 
  dplyr::rename("ID" = "patches") %>% 
  st_transform(7415)

st_write(new_gaps_polygons, "~/ahncanopygaps/InputData/test/is_this_correct.gpkg")

# # Visualise polygons on difference between methods
# MethodsDifferenceVisual(div_methods)
# plot(st_geometry(new_gaps_polygons), add = T, border = "black")


## Create doughnut buffers around new gap polygons

# Create 2m buffer around new gap polygons
buffered_new_gaps_5m <- sf::st_buffer(new_gaps_polygons, dist = 5, 
                                      joinStyle = "BEVEL")

# A helper function that erases all of y from x: 
st_erase = function(x, y){
  # result <- st_difference(x, st_union(st_combine(y)))
  result <- st_difference(x, st_union(st_combine(y)))
  return(result) 
} 

# Remove the new gaps from the buffer to create doughnut buffers
doughnut_buffer_5m_newgeometry <- st_sfc(map2(st_geometry(buffered_new_gaps_5m), st_geometry(new_gaps_polygons), 
                                              ~ st_difference(.x, .y)), crs = 7415)

# Add doughnut buffer geometry while conserving ID values 
doughnut_buffer_5m <- st_erase(buffered_new_gaps_5m, new_gaps_polygons) %>% 
  st_set_geometry(doughnut_buffer_5m_newgeometry)

# Create 5m buffer around new gap polygons
buffered_new_gaps_10m <- sf::st_buffer(new_gaps_polygons, dist = 10, 
                                      joinStyle = "BEVEL")

# Remove the new gaps from the buffer to create doughnut buffers
doughnut_buffer_10m_newgeometry <- st_sfc(map2(st_geometry(buffered_new_gaps_10m), st_geometry(new_gaps_polygons), 
                                              ~ st_difference(.x, .y)), crs = 7415)

# Add doughnut buffer geometry while conserving ID values 
doughnut_buffer_10m <- st_erase(buffered_new_gaps_10m, new_gaps_polygons) %>% 
  st_set_geometry(doughnut_buffer_10m_newgeometry)

# # Write polygons to file
# st_write(new_gaps_polygons, paste0(path, "new_gaps_polygons.gpkg"), append = F)
# st_write(doughnut_buffer_5m, paste0(path, "doughnut_buffer_5m.gpkg"), append = F)
# st_write(doughnut_buffer_10m, paste0(path, "doughnut_buffer_10m.gpkg"), append = F)

