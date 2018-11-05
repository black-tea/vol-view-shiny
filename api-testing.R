library(httr)
library(data.table)
library(sf)
library(geosphere)
library(dplyr)


calculateBearing <- function(pt1, pt2) {
  # Calculate angle between 2 points
  if(!is.na(pt1) && !is.na(pt2)){
    pt1 <- unlist(lapply(strsplit(pt1, ", "), as.numeric))
    pt2 <- unlist(lapply(strsplit(pt2, ", "), as.numeric))
    bearing <- bearing(pt1,pt2)
    return(bearing)
  } else {return(NA)}}

hi <- toString(c(13.3888, 52.517033))
hi2 <- unlist(lapply(strsplit(hi, ", "), as.numeric))

prepCoords <- function(pt) {
  # Prepare coordinates for osm API call
  #
  # Args:
  #   pt: sf point
  #
  # Returns:
  #   string of concatenated coordinates
  coords <- pt %>% st_coordinates()
  coords <- paste0(coords[1], ',', coords[2])
  return(coords)}

osmAPIcall <- function (coords) {
  # Sumbit GET request to osm routing API
  #
  # Args: 
  #   coords: String of coordinate pairs
  #
  # Returns:
  #   sf point dataframe
  #
  baseURL <- 'http://router.project-osrm.org/route/v1/driving/'
  getURL <- paste0(baseURL, coords, '?geometries=geojson')
  response <- httr::GET(getURL)
  routeList <- content(response)$routes[[1]]$geometry
  routedf <- rbindlist(routeList$coordinates)
  routesf <- st_as_sf(routedf, coords = c("V1", "V2"), crs = 4326)
  return(routesf)
}

point1 <- '13.388860,52.517037'
point2 <- '13.397634,52.529407'
coords <- paste0(point1,';',point2)
routePts <- osmAPIcall(coords)
routeLine <- routePts %>% st_combine() %>% st_cast("LINESTRING") %>% st_set_crs(4326)
routeLine <- routePts %>% sf::summarise() %>% st_cast("LINESTRING")

m <- leaflet() %>% addTiles() %>% addMarkers(data=routesf) %>% addPolylines(data = routes_linestring)

routePts <- routePts %>%
  rowwise() %>%
  mutate(coord1 = toString(geometry)) %>%
  ungroup() %>%
  mutate(coord2 = lead(coord1)) %>%
  rowwise() %>%
  mutate(angle = calculateBearing(coord1, coord2)) %>%
  ungroup() %>%
  select(-coord1, -coord2)

df <- data.frame(year = 2000:2005, value = (0:5) ^ 2)
df <- df %>%
  mutate(valuelag = lag(value))
