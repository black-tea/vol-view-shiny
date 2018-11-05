###################################
# ATSAC Volume Viewer Server Code #
###################################

library(shiny)
library(sf)
library(tidyverse)
library(datasets) # temporary!
library(httr)
library(data.table)

##### Functions ######
CreateIcon <- function(color) {
  # Create icon for mapping, using the awesomeIcons library
  #
  # Args:
  #   color: desired color for the map marker
  #
  # Returns:
  #   Map marker with the 'circle-o' icon in the desired color
  custom_icon <- awesomeIcons(
    icon = 'circle-o',
    iconColor = '#ffffff',
    library = 'fa',
    markerColor = color)
  return(custom_icon)}

searchbyDistance <- function(filterFeat, buffFeat, buffdist){
  # Buffers a feature, and filters another feature
  # by the buffer
  featNad83 <- st_transform(buffFeat, 2229) 
  featBuffNad83 <- st_buffer(featNad83, buffdist) 
  featBuffwgs <- st_union(st_transform(featBuffNad83, 4326)) 
  result <- filterFeat[featBuffwgs,]
  return(result)
}

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

##### Data Prep ######
# Import data
volume_raw <- read_csv('Data/volumes/det_data_20171003.csv', col_names = c('END_TIME', 'LON', 'LAT', 'ANGLE', 'STA', 'OCC', 'VOL'))
streets <- read_sf('Data/streets/Streets.shp')
names(streets$geometry) <- NULL # for leaflet loading
sig_int <- read_sf('data/signalized_intersections/atsacint_base.shp', crs=4326)

# Roll up into summary volumes
detectors <- volume_raw %>%
  mutate(id = paste0(LON,LAT)) %>%
  group_by(LON, LAT) %>%
  summarize(id = max(id),
            angle = max(ANGLE)) %>%
  st_as_sf(coords = c('LON', 'LAT'), crs=4326)
names(detectors$geometry) <- NULL # for leaflet loading

# Get nearest streets to each detector point
nearest_street_id <- st_nearest_feature(detectors, streets)
nearest_streets <- streets[nearest_street_id,]

# Filter only those signalized intersections near each point
sig_int <- rownames_to_column(searchbyDistance(sig_int, detectors, 2000))

# Get nearest signal to each detector point
nearest_sig_id <- st_nearest_feature(detectors, sig_int)
sig_int_df <- sig_int %>% st_set_geometry(NULL)
detectors <- detectors %>%
  add_column(nearest_sig_id) %>%
  mutate(nearest_sig_id = as.character(nearest_sig_id)) %>%
  left_join(sig_int_df, by = c('nearest_sig_id' = 'rowname' )) %>%
  select(id, nearest_sig_id, LOC)

##### Server ######
shinyServer(function(input, output) {
  
  ### UI Output 
  output$int_select <- renderUI({
    selectizeInput(inputId = "int",
                   label = "Intersection",
                   choices = sig_int$LOC,
                   options = list(
                     placeholder = 'Intersection Name',
                     onInitialize = I('function() { this.setValue(""); }')),
                   multiple = FALSE)
  })
  # Intersection 1
  output$corridor_select1 <- renderUI({
    selectizeInput(inputId = 'corridorInt1',
                   label = 'From',
                   choices = sig_int$LOC,
                   options = list(
                     placeholder = 'Intersection Name',
                     onInitialize = I('function() { this.setValue(""); }')),
                   multiple = FALSE)})
  # Intersection 2
  output$corridor_select2 <- renderUI({
    selectizeInput(inputId = 'corridorInt2',
                   label = 'To',
                   choices = sig_int$LOC,
                   options = list(
                     placeholder = 'Intersection Name',
                     onInitialize = I('function() { this.setValue(""); }')),
                   multiple = FALSE)  
  })

  # RV for location objects
  locationRV <- reactiveValues(Intersection=list(), Segment=list())
  
  # Reactive expression to grab intersection data based on user selection
  intersectionR <- reactive({
    if(input$int != ""){
      intersectionR <- sig_int %>% filter(LOC == toString(input$int))
    } else {return(NULL)}})
  
  # Reactive expression to grab first intersection of corridor query
  corridorInt1R <- reactive({
    if(input$corridorInt1 != "" && !is.null(input$corridorInt1)){
      corridorInt1R <- sig_int %>% filter(LOC == toString(input$corridorInt1))
    } else {return(NULL)}})
  
  # Reactive expression to grab first intersection of corridor query
  corridorInt2R <- reactive({
    if(input$corridorInt2 != "" && !is.null(input$corridorInt2)){
      corridorInt2R <- sig_int %>% filter(LOC == toString(input$corridorInt2))
    } else {return(NULL)}})
  
  # Reactive expression to create coordinate pairs for corridor
  coordPair <- reactive({
    if(!is.null(corridorInt1R()) && !is.null(corridorInt2R())){
      coord1 <- prepCoords(corridorInt1R())
      coord2 <- prepCoords(corridorInt2R())
      coords <- paste0(coord1, ';', coord2)
      return(coords)
    } else {return(NULL)}})
  
  # Reactive expression to grab detectors related to selected intersection
  detectorsR <- reactive({
    if(input$int != ""){
      intersection <- detectors %>%
        filter(LOC == toString(input$int))}})
  
  # Reactive expression to filter volume data
  volumeR <- reactive({
    volumeR <- volume_raw %>%
      mutate(id = paste0(LON, LAT)) %>% # id is combination of lat/lon
      left_join(detectors, by = c('id' = 'id' )) %>%
      filter(LOC == input$int) %>%
      mutate(hour = lubridate::hour(END_TIME)) %>%
      # filter by UI time slider
      filter(lubridate::date(END_TIME) == as.Date(input$date)) %>%
      filter(hour >= input$time_slider[[1]] &
             hour <= input$time_slider[[2]]) %>%
      # group & summarize by hour
      group_by(LOC, hour) %>%
      summarise(hourlyVol = sum(VOL))
  })
  
  corridorVolumeR <- reactive({
    if(!is.null(routeLineR())){
      corridorSigInts <- searchbyDistance(sig_int, routeLineR(), 50)
      
      volume <- volume_raw %>%
        mutate(id = paste0(LON, LAT)) %>%
        left_join(detectors, by = c('id' = 'id')) %>%
        filter(LOC %in% corridorSigInts$LOC) %>%
        mutate(hour = lubridate::hour(END_TIME)) %>%
        # filter by UI time slider
        filter(lubridate::date(END_TIME) == as.Date(input$date)) %>%
        filter(hour >= input$time_slider[[1]] &
               hour <= input$time_slider[[2]]) %>%
        # group & summarize by hour
        group_by(LOC) %>%
        summarise(volume=sum(VOL))
      
      return(volume)
    } else {return(NULL)}}) 
  
  # Route line reactive
  routeLineR <- reactive({
    if(!is.null(coordPair())){
      routePts <- osmAPIcall(coordPair())
      routeLine <- routePts %>% st_combine() %>% st_cast("LINESTRING") %>% st_set_crs(4326)
      return(routeLine)
    } else { return(NULL) }
  })
  
  ### Map Output
  output$map <- renderLeaflet({
    # Map object
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(minZoom = 10, maxZoom = 18)) %>%
      setView(lng = -118.329327,
              lat = 34.0546143,
              zoom = 11)})
  
  # Map observer updates based on corridor query
  observeEvent(routeLineR(), {
    if(!is.null(routeLineR())){
      print(corridorVolumeR())
      routeLineR <- routeLineR()
      route_sig_int <- searchbyDistance(sig_int, routeLineR, 50)
      print(route_sig_int)
      
      proxy <- leafletProxy("map") %>%
        clearMarkers() %>%
        clearShapes() %>%
        addPolylines(data = routeLineR) %>%
        addAwesomeMarkers(
          data = route_sig_int,
          icon = CreateIcon('darkblue')) %>%
        fitBounds(lng1 = as.double(st_bbox(routeLineR)[1]),
                  lat1 = as.double(st_bbox(routeLineR)[2]),
                  lng2 = as.double(st_bbox(routeLineR)[3]),
                  lat2 = as.double(st_bbox(routeLineR)[4]))
    }})
  
  # Map observer that updates based on the intersection
  observeEvent(input$int, {
    print(volumeR())
    # Clear markers if no int select
    if(input$int == ""){
      proxy <- leafletProxy("map") %>%
        clearMarkers() %>%
        clearShapes() %>%
        setView(lng = -118.329327,
                lat = 34.0546143,
                zoom = 11)}
    # Add marker for selected int
    if(input$int != ""){
      # Get intersection reactive var, clear markers, clear RV
      intersectionR <- intersectionR()
      detectorsR <- detectorsR()
      locationRV$Segment <- NULL
      proxy <- leafletProxy("map") %>%
        clearMarkers() %>%
        clearShapes() %>%
        addAwesomeMarkers(
          data = intersectionR,
          icon = CreateIcon('darkblue')) %>%
        addCircleMarkers(
          data = detectorsR,
          fillColor = "#0E5D99",
          color = "#ffffff",
          radius = 3,
          weight = 0.5,
          #stroke = FALSE,
          fillOpacity = 1) %>%
        # Update the map zoom bounds
        fitBounds(lng1 = as.double(st_bbox(intersectionR)[1]),
                  lat1 = as.double(st_bbox(intersectionR)[2]),
                  lng2 = as.double(st_bbox(intersectionR)[3]),
                  lat2 = as.double(st_bbox(intersectionR)[4]))}})
  
  # Fill in the spot we created for a plot
  output$volPlot <- renderPlot({
    if(input$type_select == 'Intersection' && input$int != ""){
      # Render a barplot
      volumeR <- volumeR()
      barplot(volumeR$hourlyVol, 
              main=paste0(toString(volumeR$LOC[1]), " (", input$date, ")"),
              ylab="Volume",
              xlab="Hour",
              col="#0E5D99",
              names.arg=seq(input$time_slider[[1]], input$time_slider[[2]]),
              border=NA)
    } else if(input$type_select == 'Corridor' && !is.null(corridorVolumeR())){

      corridorVolume <- corridorVolumeR()
      par(mar = c(5,12,4,2) + 0.1)  
      barplot(corridorVolume$volume,
                main=paste0("Corridor Volumes (", input$date," ",
                            input$time_slider[[1]], " to ",input$time_slider[[2]],  ")"),
                col="#0E5D99",
                #ylab="Volume",
                #xlab="Intersection",
                names.arg=corridorVolume$LOC,
                border=NA,
                las=2,
                horiz=TRUE,
                cex.names=0.7)
        #par(mar = c(10,4,4,2) + 0.1)
      } else {return(NULL)}
    })

  
})
