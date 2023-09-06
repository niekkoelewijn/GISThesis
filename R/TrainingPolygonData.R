# Course: GRS-80436 / MSc Thesis
# Project: AHN canopy gap detection
# Author: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: L.C. Vreugdenhil MSc.
# Year: 2022

### Description ###
# Script to couple the State forestry data of the logbook of the management 
# interventions to the correct forest sections


## Load State forestry data into R environment

# Load table with logbook of management interventions
path_logbook <- "ahncanopygaps/InputData/Speulderbos Oogstdata 2018 - 2019 - 2020.csv"
interventions_logbook <- read_csv(path_logbook)

# Load shapefile with forest plots
path_plots <- "ahncanopygaps/InputData/Speulderbos/boslaag nieuw Veluwe Achterhoek met objectnaam/vak afdeling veluwe achterhoek.shp"
forest_plots <- readOGR(path_plots)

# Load shapefile of the Speulderbos
path_speulderbos <- "ahncanopygaps/InputData/SpeulderbosClip.shp"
speulderbos_area <- readOGR(path_speulderbos)

# Convert forest_sections and speulderbos_area to sf
forest_plots <- st_as_sf(forest_plots) %>% 
  st_transform(7415)
speulderbos_area <-st_as_sf(speulderbos_area) %>% 
  st_transform(7415)

# Clip forest_plots to speulderbos_area extent
forest_plots_speulderbos <- sf::st_intersection(forest_plots, speulderbos_area)


## Join forest_sections_speulderbos with interventions_logbook

# # Investigate VAKAFD(2) column, the column with the forest plot codes
# interventions_logbook$`VAFAFD(2)`

# Join interventions_logbook with forest_sections_speulderbos
forest_plots_with_interventions <- interventions_logbook %>% 
  
  # Add a space between number and letter in VAKAFD(2) column to match style of
  # forest_sections_speulderbos COMBI column
  dplyr::mutate(`VAFAFD(2)` = gsub(`VAFAFD(2)`, pattern = "([0-9]*)([A-Z]*)([0-9]*)(.*)", replacement = "\\1 \\2\\3")) %>% 
  
  # Rename VAKAFD(2) to COMBI
  dplyr::rename(COMBI = `VAFAFD(2)`) %>% 
  
  # Add attributes of forest_sections_speulderbos on column COMBI
  dplyr::left_join(forest_plots_speulderbos, by = "COMBI") %>% 
  
  # Drop NA for ID
  drop_na(ID) %>% 
  
  # Select columns
  select(COMBI, Jaar, Boomsoort, AantalSt, DBH, SrtKap, geometry) %>% 
  
  # Make spatial
  st_as_sf()

# # Investigate output
# plot(forest_plots_with_interventions["Boomsoort"])

# # Write output to file
# path <- "~/ahncanopygaps/InputData/"
# st_write(forest_plots_with_interventions, paste0(path, "forest_plots_with_interventions.gpkg"), append = F)
