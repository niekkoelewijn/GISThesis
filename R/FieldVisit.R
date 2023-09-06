# Course: GRS-80436 / MSc Thesis
# Project: AHN canopy gap detection
# Author: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: L.C. Vreugdenhil MSc.
# Year: 2022

### Description ###
# Script to visualize the locations that I visited in the forest

# Organize visited points in a dataframe
FieldVisitTibble <- tibble(
  id = c("Location 1", "Location 2", "Location 3", "Location 4", "Location 5",
         "Location 6", "Location 7", "Location 8", "Location 9", "Location 10",
         "Location 11", "Location 12", "Location 13"),
  lat = c(52.26147, 52.25823, 52.25951, 52.25898, 52.25897, 52.25971, 52.26083,
          52.26133, 52.26214, 52.26209, 52.25870, 52.26028, 52.26124),
  lon = c(5.69346, 5.69150, 5.69343, 5.69054, 5.68822, 5.69424, 5.68861,
          5.68866, 5.69078, 5.68986, 5.69263, 5.68994, 5.69086)
)

# Convert the tibble to a sf class
FieldVisitPoints <- st_as_sf(FieldVisitTibble, coords = c("lon","lat")) %>% 
  
  # Assign WGS84 coordinate system to the points
  st_set_crs(4326) %>% 
  
  # Transform the coordinate system to the same CRS as the point clouds
  st_transform(crs = 28992)

# Plot locations on difference CHM
plot(div_chm, col = height.colors(500))
plot(st_geometry(FieldVisitPoints), add = T, pch = 1)
text(st_coordinates(FieldVisitPoints)[,1]-7, st_coordinates(FieldVisitPoints)[,2]-20, labels=FieldVisitPoints$id, cex=0.6)
                             
# Plot locations on CHM AHN4
plot(chm_AHN4, col = height.colors(500), main = "CHM AHN4 and visited locations in the forest")
plot(st_geometry(FieldVisitPoints), add = T, pch = 1, cex = 1.5)
text(st_coordinates(FieldVisitPoints)[,1]-7, st_coordinates(FieldVisitPoints)[,2]-20, labels=FieldVisitPoints$id, cex=0.6)

