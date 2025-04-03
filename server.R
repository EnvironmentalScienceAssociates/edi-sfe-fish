
function(input, output, session) {
  
  dt1Sub1 <- reactive({
    dt1[dt1[["Source"]] %in% input$sources,]
  })
  
  observe({
    dfx = dt1Sub1()
    req(nrow(dfx) > 0)
    freezeReactiveValue(input, "years")
    updateSliderInput(session, "years", value = c(min(dfx$Year, na.rm = TRUE),
                                                  max(dfx$Year, na.rm = TRUE)))
  })
  
  dt1Sub2 <- reactive({
    dfx = dt1Sub1()
    dfx[dfx[["Year"]] >= input$years[1] & dfx[["Year"]] <= input$years[2],]
  })
  
  stations <- reactive({
    # not exactly stations b/c same station label can have many points
    req(nrow(dt1Sub2()) > 0)
    dt1Sub2() |> 
      select(Source, Station, SourceStation, LatRound, LonRound, Latitude, Longitude) |> 
      distinct() |> 
      filter(!(is.na(Latitude) | is.na(Longitude)))
  })
  
  stationPoints <- reactive({
    st_as_sf(stations(), coords = c("Longitude", "Latitude"), crs = 4326)
  })
  
  sourcePoints <- reactive({
    stations() |>
      group_by(Source, LatRound, LonRound) |>
      summarise(N = n(),
                Latitude = mean(Latitude, na.rm = TRUE),
                Longitude = mean(Longitude, na.rm = TRUE)) |>
      filter(!(is.na(Latitude) | is.na(Longitude))) |>
      st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  })
  
  output$map = renderLeaflet({
    leaflet(options = leafletOptions(attributionControl = FALSE)) |>
      setView(lng = -121.75, lat = 38.36, zoom = 8) |>
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") |> 
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |> 
      addLayersControl(baseGroups = c("Topo", "Satellite"),
                       options = layersControlOptions(collapsed = FALSE)) |> 
      addDrawToolbar(
        targetGroup = "draw",
        singleFeature = TRUE,
        polylineOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) |> 
      addPolygons(data = boundary, 
                  weight = 3,
                  fillOpacity = 0)
  })
  
  proxy <- leafletProxy("map") 
  
  observe({
    proxy |> 
      clearGroup("sources") |> 
      clearGroup("stations") |> 
      clearControls()
    
    if (nrow(dt1Sub2()) > 0){
      proxy |> 
        addCircleMarkers(data = sourcePoints(), 
                         label = ~ paste("N =", N),
                         radius = 6,
                         color = "black",
                         weight = 1,
                         opacity = 1,
                         fillColor = ~pal(Source),
                         fillOpacity = 0.8,
                         group = "sources") |> 
        addCircleMarkers(data = stationPoints(), 
                         label = ~Station, 
                         radius = 4,
                         color = "black",
                         weight = 1,
                         opacity = 1,
                         fillColor = ~pal(Source),
                         fillOpacity = 0.8,
                         group = "stations") |> 
        groupOptions("sources", zoomLevels = 8:11) |>   
        groupOptions("stations", zoomLevels = 12:20) |> 
        addLegend("bottomright", pal = pal, values = input$sources, 
                  title = "Data Source", opacity = 1)
    }
  })
  
  observeEvent(input$map_draw_new_feature, {
    shape <- input$map_draw_new_feature
    cat("Draw event triggered. Layer type:", shape$layerType, "\n")
    out <<- shape
  })
  
  # initialize with empty list
  rv <- reactiveValues(dt2 = setNames(vector("list", length(sources)), sources))

  observeEvent(input$get_species,{
    rv$dt2 = lapply(input$sources, function(x){
      if (is.null(rv$dt2[[x]])){
        rv$dt2[[x]] = readRDS(paste0("dt2-", gsub(" ", "", x), ".rds"))
      }
    }) |> 
      setNames(input$sources)
    test <<- rv$dt2
  })
  
}
