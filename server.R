
function(input, output, session) {
  
  rv <- reactiveValues(last_sources = sources, shape = NULL, summ = NULL,
                       # initialize dt2 with empty list
                       dt2 = setNames(vector("list", length(sources)), sources))
  
  dt1SubYear <- reactive({
    dt1[dt1[["Year"]] >= input$years[1] & dt1[["Year"]] <= input$years[2],]
  })
  
  observe({
    dt1_sub = dt1SubYear()
    req(nrow(dt1_sub) > 0)
    freezeReactiveValue(input, "sources")
    opts = sort(unique(dt1_sub$Source))
    overlap = rv$last_sources[rv$last_sources %in% opts]
    rv$last_sources = overlap
    updatePickerInput(session, "sources", choices = opts, selected = overlap)
  })
  
  dt1SubSource <- reactive({
    dt1_sub = dt1SubYear()
    dt1_sub[dt1_sub[["Source"]] %in% input$sources,]
  })
  
  stations <- reactive({
    # not exactly stations b/c same station label can have many points
    req(nrow(dt1SubSource()) > 0)
    dt1SubSource() |> 
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
        circleOptions = FALSE,
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
      clearControls() |> 
      addLegend("bottomright", pal = pal, values = input$sources, 
                title = "Data Source", opacity = 1)
  })
  
  observe({
    dt1_sub = dt1SubSource()
    
    proxy |> 
      clearGroup("sources") |> 
      clearGroup("stations") |> 
      clearControls()
    
    if (nrow(dt1_sub) > 0){
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
        groupOptions("stations", zoomLevels = 11:20) |> 
        groupOptions("sources", zoomLevels = 5:10)  |> 
        addLegend("bottomright", pal = pal, values = unique(dt1_sub$Source), 
                  title = "Data Source", opacity = 1)
    }
  })
  
  output$messageButton <- renderUI({
    if (is.null(rv$shape)){
      helpText("Use map drawing tools to select samples to include in data summary.")
    } else {
      validate(need(nrow(dt1SubSpatial()) > 0, "No data in selected area"))
      input_task_button("tally_fish", "Tally Fish Abundance")
    }
  })
  
  observeEvent(input$map_draw_new_feature, {
    rv$shape = geojsonsf::geojson_sf(jsonify::to_json(input$map_draw_new_feature, unbox = TRUE))
  })
  
  observeEvent(input$map_draw_edited_features, {
    rv$shape = geojsonsf::geojson_sf(jsonify::to_json(input$map_draw_edited_features, unbox = TRUE))
  })
  
  observeEvent(input$map_draw_deleted_features, {
    rv$shape = NULL
  })
  
  dt1SubSpatial <- reactive({
    req(rv$shape)
    dt1_sub = dt1SubSource()
    stations_selected = st_join(stationPoints(), rv$shape, join = st_within) |> 
      filter(!is.na(feature_type))
    dt1_sub[dt1_sub[["SourceStation"]] %in% stations_selected[["SourceStation"]],]
  })
  
  sourcesSpatial <- reactive({
    unique(dt1SubSpatial()$Source)
  })
  
  observeEvent(rv$shape, {
    withProgress(message = "Gathering data...", value = 0, {
      for (x in sourcesSpatial()){
        if (is.null(rv$dt2[[x]])){
          incProgress(1/length(sourcesSpatial()), detail = x)
          rv$dt2[[x]] = readRDS(file.path("data", paste0("dt2-", gsub(" ", "", x), ".rds"))) |> 
            # for now, the app is focused on counts of present species
            # it reduces the size of the dataset to drop the zero counts
            filter(Count > 0)
        }
      }
    })
  })
  
  output$sourceMessage <- renderUI({
    req(rv$shape)
    HTML(paste("Sources in selected area:<br>",
               paste(sourcesSpatial(), collapse = ", ")))
  })
  
  observeEvent(input$tally_fish,{
    req(rv$shape, nrow(dt1SubSpatial()) > 0)
    dt1_sub = dt1SubSpatial()
    dt2_sub = lapply(rv$dt2, function(dfx){
      if (!is.null(dfx)){
        dfx |> 
          filter(SampleID %in% dt1_sub$SampleID) |> 
          group_by(SampleID, Taxa) |> 
          summarise(Count = sum(Count, na.rm = TRUE))
      }
    }) |> 
      bind_rows()
    
    rv$summ = left_join(dt2_sub, select(dt1_sub, SampleID, Source, Year, Month, Date),
                        by = join_by(SampleID)) |> 
      group_by(across(all_of(input$group_by))) |> 
      summarise(Count = sum(Count, na.rm = TRUE))
    
    updateTabsetPanel(session, "nav", selected = "Table")
  })
  
  output$taxa <- renderUI({
    req("Taxa" %in% input$group_by, rv$summ)
    taxa = sort(unique(rv$summ$Taxa))
    pickerInput(inputId = "taxa", label = "Taxa", multiple = TRUE,
                choices = taxa, selected = taxa,
                options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 10))
  })
  
  output$months <- renderUI({
    req("Month" %in% input$group_by, rv$summ)
    m = setNames(1:12, month.abb)
    pickerInput(inputId = "months", label = "Month", multiple = TRUE, 
                choices = m, selected = m,
                options = list(`actions-box` = TRUE, `live-search` = TRUE))
  })
  
  output$dateRange <- renderUI({
    req("Date" %in% input$group_by, rv$summ)
    mn = min(rv$summ$Date, na.rm = TRUE)
    mx = max(rv$summ$Date, na.rm = TRUE)
    dateRangeInput("date_range", label = "Dates", start = mn, end = mx, min = mn, max = mx)
  })
  
  table <- reactive({
    req(rv$summ)
    out = rv$summ
    if ("Taxa" %in% input$group_by){
      req(input$taxa)
      out = out[out[["Taxa"]] %in% input$taxa, ]
    }
    if ("Month" %in% input$group_by){
      req(input$months)
      out = out[out[["Month"]] %in% input$months, ]
    }
    if ("Date" %in% input$group_by){
      req(input$date_range)
      out = out[out[["Date"]] >= input$date_range[1] & 
                  out[["Date"]] <= input$date_range[2], ]
    }
    out
  })
  
  output$table <- DT::renderDataTable({
    req(rv$summ)
    mutate(table(), Count = round(Count))
  }, rownames = FALSE)
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("EDI-SFE-Fish-Abundance-", round(as.numeric(Sys.time())), ".csv")
    },
    content = function(file) {
      write.csv(table(), file, row.names = FALSE)
    }
  )
  
}
