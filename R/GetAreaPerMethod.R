## Get total area per method

# Silva
new_gaps_silva <- gaps_div_5_10_noupper
new_gaps_silva[new_gaps_silva < 2] <- NA
new_gaps_silva[new_gaps_silva >= 2] <- 1
new_gaps_silva <- terra::patches(new_gaps_silva, directions = 8, allowGaps = T)
rcl_silva <- terra::freq(new_gaps_silva)
z_silva <- terra::classify(new_gaps_silva, rcl = rcl_silva, right = NA)
z_silva[is.na(new_gaps_silva)] <- NA
new_gaps_silva[z_silva < 10] <- NA
new_gaps_polygons_silva <- terra::as.polygons(new_gaps_silva) %>% 
  st_as_sf() %>% 
  dplyr::rename("ID" = "patches") %>% 
  st_transform(7415)

# Leitold
new_gaps_leitold <- gaps_div_CHM_binary
new_gaps_leitold[new_gaps_leitold < 2] <- NA
new_gaps_leitold[new_gaps_leitold >= 2] <- 1
new_gaps_leitold <- terra::patches(new_gaps_leitold, directions = 8, allowGaps = T)
rcl_leitold <- terra::freq(new_gaps_leitold)
z_leitold <- terra::classify(new_gaps_leitold, rcl = rcl_leitold, right = NA)
z_leitold[is.na(new_gaps_leitold)] <- NA
new_gaps_leitold[z_leitold < 10] <- NA
new_gaps_polygons_leitold <- terra::as.polygons(new_gaps_leitold) %>% 
  st_as_sf() %>% 
  dplyr::rename("ID" = "patches") %>% 
  st_transform(7415)

# Get only gaps in forest plots
new_gaps_silva_in_plots <- new_gaps_polygons_silva[st_within(new_gaps_polygons_silva, st_union(forest_plots_in_study_area)) %>% lengths > 0,]
new_gaps_leitold_in_plots <- new_gaps_polygons_leitold[st_within(new_gaps_polygons_leitold, st_union(forest_plots_in_study_area)) %>% lengths > 0,]

# Get sum of area
sum(st_area(new_gaps_silva_in_plots)) # 79876 [m^2]
sum(st_area(new_gaps_leitold_in_plots)) # 150184 [m^2]

# Get share of all gaps
sum(st_area(new_gaps_complete)) # 150463 [m^2]
79876 / 150463 # 0.5308681 Silva
150184 / 150463 # 0.9981457 Leitold

