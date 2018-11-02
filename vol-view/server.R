###################################
# ATSAC Volume Viewer Server Code #
###################################

library(shiny)
library(sf)
library(tidyverse)
library(datasets) # temporary!

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
    # The markercolor is from a fixed set of color choices
    markerColor = color)
  return(custom_icon)}

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
  
    # Selection Input
    selectizeInput(inputId = "int",
                   label = "Intersection",
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
  
  # Reactive expression to filter volume data
  volumeR <- reactive({
    volumeR <- volume_raw %>%
      mutate(id = paste0(LON, LAT)) %>% # id is combination of lat/lon
      left_join(detectors, by = c('id' = 'id' )) %>%
      filter(LOC == input$int) %>%
      # filter by UI time slider
      filter(lubridate::hour(END_TIME) >= input$time_slider[1] &&
             lubridate::hour(END_TIME) <= input$time_slider[2]) %>%
      # group & summarize by hour
      group_by(id, LON, LAT, lubridate::hour(END_TIME)) %>%
      summarise(hourlyVol = sum(VOL))
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
      locationRV$Segment <- NULL
      proxy <- leafletProxy("map") %>%
        clearMarkers() %>%
        clearShapes() %>%
        addAwesomeMarkers(
          data = intersectionR,
          icon = CreateIcon('darkblue')) %>%
        # Update the map zoom bounds
        fitBounds(lng1 = as.double(st_bbox(intersectionR)[1]),
                  lat1 = as.double(st_bbox(intersectionR)[2]),
                  lng2 = as.double(st_bbox(intersectionR)[3]),
                  lat2 = as.double(st_bbox(intersectionR)[4]))}})
  
  # Fill in the spot we created for a plot
  output$volPlot <- renderPlot({
    # Render a barplot
    barplot(WorldPhones[,'S.Amer']*1000, 
            main='S.Amer',
            ylab="Number of Telephones",
            xlab="Year")})
  
})
