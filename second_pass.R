library(dplyr)
library(readr)
library(tibble)
library(leaflet)
library(purrr)
library(sf)
library(rgdal)
library(geosphere)
library(randomcoloR)

##### Functions
# Function to create Icons for map
createIcon <- function(color) {
  custom_icon <- awesomeIcons(
    icon = 'circle-o',
    iconColor = 'white',
    library = 'fa',
    markerColor = color
  )
  return(custom_icon)
}
# Function to calculate angle between two points
angle <- function(pt1, pt2) {
  bearing <- bearing(as(pt1, "Spatial"), pt2)
  #bearing <- bearing(pt2, as(pt1, "Spatial"))
  return(bearing)
}

# Import data
volume_raw <- read_csv('Data/volumes/det_data_20171003.csv',
                       col_names = c('END_TIME', 'LON', 'LAT', 'ANGLE', 'STA', 'OCC', 'VOL'))
streets <- read_sf('Data/streets/Streets.shp')
# https://stackoverflow.com/questions/50844693/simple-feature-linestring-collection-not-visible-in-r-leaflet
names(streets$geometry) <- NULL
sig_int <- read_sf('Data/signalized_intersections/atsacint_base.shp', crs=4326)

# testing
volume <- volume_raw %>%
  group_by(LON, LAT, lubridate::hour(END_TIME)) %>%
  summarise(hourlyVol = sum(VOL))

# Roll up into summary volumes

volumeR <- volume_raw %>%
  mutate(id = paste0(LON, LAT)) %>%
  group_by(id, LON, LAT, lubridate::hour(END_TIME)) %>%
  summarise(hourlyVol = sum(VOL))

# Get nearest streets to each detector point
nearest_street_id <- st_nearest_feature(detectors, streets)
nearest_streets <- streets[nearest_street_id,]

# Filter only those signalized intersections near each point
detectors_nad83 <- st_transform(detectors, 2229) # Convert to NAD83
detectors_buff_nad83 <- st_buffer(detectors_nad83, 2000) # Buffer 2k ft
detectors_buff <- st_union(st_transform(detectors_buff_nad83, 4326)) # Convert back to wgs84
sig_int <- rownames_to_column(sig_int[detectors_buff,]) 

# Get nearest signal to each detector point
nearest_sig_id <- st_nearest_feature(detectors, sig_int)
# st_geometry(sig_int) <- NULL
sig_int_df <- sig_int %>% st_set_geometry(NULL)
detectors <- detectors %>%
  add_column(nearest_sig_id) %>%
  mutate(nearest_sig_id = as.character(nearest_sig_id)) %>%
  left_join(sig_int_df, by = c('nearest_sig_id' = 'rowname' )) %>%
  select(vol, angle, nearest_sig_id, LON, LAT, mrkrcolor) %>%
  rowwise %>%
  mutate(sig_coord = paste0(toString(LON), ', ', toString(LAT))) %>%
  mutate(angle_cal = angle(geometry, c(LON, LAT))) %>%
  select(-LAT, -LON, -sig_coord) %>%
  ungroup() %>%
  st_as_sf(.)

  
# volume <- left_join(volume, sig_int, by = ('nearest_sig_id' = 'rowname' ))

# Plot
map <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolylines(data = nearest_streets) %>%
  #addMarkers(data = volume, popup = ~as.character(angle_cal)) %>%
  addAwesomeMarkers(data=volume, icon=createIcon(~mrkrcolor)) %>%
  addPolygons(data=volume_buff) %>%
  addMarkers(data=sig_int) 
map
saveWidget(map, file="m.html")

