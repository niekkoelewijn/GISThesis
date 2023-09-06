### Script om attributen canopy gaps interactief te bekijken ###


## Laad de canopy gaps in je R environment

# Specificeer de working directory (aanpassen naar de folder waar de data staat)
setwd("/Users/niekkoelewijn/ahncanopygaps/InputData/niek_gaps")

# Specificeer de naam van de canopy gaps (aanpassen naar de folder waar de data staat)
name <- "new_gaps_all_metrics.gpkg"

# Laad de gaps
new_gaps_all_metrics <- st_read(name)


## Visualiseer een attribuut van de canopy gaps met mapview

# Kies uit de lijst een kolom om te visualiseren
colnames(new_gaps_all_metrics)

# Specificeer de naam van de desbetreffende kolom
zcol <- "zmean_AHN4"

# Maak standaard leaflet webmap
m <- mapview(new_gaps_all_metrics, zcol=zcol) # maken van standaard leaflet webmap met mapview

# Bekijk kaart in Rstudio Viewer
m 

# Specificeer de naam van de link naar de kaart
URL <- "canopy_gaps.html"

# Creeer webmap. Dubbelklip op dit bestand in je bestanden opent de kaart in je internet browser 
mapshot(m, url=paste0(getwd(),"/",URL)) 
