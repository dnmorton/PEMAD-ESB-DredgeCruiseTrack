#GEMINI created
library(shiny)
library(sf)
library(leaflet)
library(leaflet.extras)
library(dplyr)

# ---- Data Initialization ----
tryCatch({
  points <- read.csv("Base_Dredge_Stations_w_drops.csv")
  if(!"COMMENTS" %in% names(points)) points$COMMENTS <- "Normal"
  points_sf_raw <- st_as_sf(points, coords = c("DEC_LON", "DEC_LAT"), crs = 4326)
}, error = function(e) {
  set.seed(42)
  stem <- data.frame(id = 1:6, STATION_NAME = paste("Stem-Station", 1:6), COMMENTS = "Normal", lon = -70.0 + runif(6, -0.02, 0.02), lat = seq(41.0, 41.4, length.out = 6))
  left_bar <- data.frame(id = 7:9, STATION_NAME = paste("Rock-West", 1:3), COMMENTS = "Rock-Chain", lon = seq(-70.4, -70.05, length.out = 3), lat = 41.4 + runif(3, -0.02, 0.02))
  right_bar <- data.frame(id = 10:12, STATION_NAME = paste("Rock-East", 1:3), COMMENTS = "Rock-Chain", lon = seq(-69.95, -69.6, length.out = 3), lat = 41.4 + runif(3, -0.02, 0.02))
  points_sf_raw <<- st_as_sf(rbind(stem, left_bar, right_bar), coords = c("lon", "lat"), crs = 4326)
})

points_sf_raw <- st_transform(points_sf_raw, 4326) %>% 
  st_make_valid() %>% 
  mutate(id = row_number())

# ---- UI ----
ui <- fluidPage(
  titlePanel("Multi-Segment Cruise Planner with Port Routing"),
  sidebarLayout(
    sidebarPanel(
      h4("Instructions"),
      helpText("1. Click 'Set Starting Port' then click your departure harbor on the map."),
      helpText("2. Use the 'Draw a polyline' map tool to trace your survey track segments sequentially."),
      helpText("3. Click 'Set Ending Port' then click your final destination on the map."),
      hr(),
      
      # Port Set Controls
      actionButton("add_start_port", "Set Starting Port", class = "btn-success btn-sm"),
      actionButton("add_end_port", "Set Ending Port", class = "btn-primary btn-sm"),
      br(), br(),
      actionButton("clear_track", "Wipe All & Restart", class = "btn-danger btn-sm"),
      hr(),
      
      # Live Distance Dashboard
      wellPanel(
        style = "background-color: #f7f9fa; border: 1px solid #dcdfe1;",
        h4("Total Port-to-Port Track", style = "margin-top: 0; font-weight: bold;"),
        h3(textOutput("total_distance"), style = "color: #28a745; margin: 5px 0; font-weight: bold;"),
        textOutput("station_count")
      ),
      
      hr(),
      h4("Export Survey Cruise"),
      downloadButton("download_gpx", "Export Port-to-Port GPX", class = "btn-info")
    ),
    mainPanel(
      leafletOutput("map", height = 600)
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # Reactive values for ports and accumulated survey points
  start_port <- reactiveVal(NULL)
  end_port <- reactiveVal(NULL)
  survey_route_pts <- reactiveVal(NULL)
  
  # Click mode toggles
  start_port_mode <- reactiveVal(FALSE)
  end_port_mode <- reactiveVal(FALSE)
  
  output$map <- renderLeaflet({
    bounds <- st_bbox(points_sf_raw)
    leaflet() %>%
      addTiles() %>%
      fitBounds(
        lng1 = as.numeric(bounds["xmin"]), lat1 = as.numeric(bounds["ymin"]),
        lng2 = as.numeric(bounds["xmax"]), lat2 = as.numeric(bounds["ymax"])
      ) %>%
      addDrawToolbar(
        targetGroup = "manual_line",
        polylineOptions = drawPolylineOptions(metric = FALSE, repeatMode = FALSE),
        polygonOptions = FALSE, rectangleOptions = FALSE, circleOptions = FALSE, markerOptions = FALSE,
        editOptions = editToolbarOptions(remove = TRUE)
      )
  })
  
  # Render Base Reference Stations
  observe({
    data <- points_sf_raw %>% 
      mutate(is_rock = grepl("Rock-Chain", COMMENTS, ignore.case = TRUE),
             pt_color = ifelse(is_rock, "purple", "blue"))
    
    leafletProxy("map", data = data) %>%
      clearGroup("base_stations") %>%
      addCircleMarkers(
        color = ~pt_color, radius = 5, fillOpacity = 0.7,
        label = ~paste0(STATION_NAME, " (", COMMENTS, ")"), group = "base_stations"
      )
  })
  
  # Set Port Click Listeners
  observeEvent(input$add_start_port, { start_port_mode(TRUE); end_port_mode(FALSE) })
  observeEvent(input$add_end_port, { end_port_mode(TRUE); start_port_mode(FALSE) })
  
  # Map Clicks for Ports
  observeEvent(input$map_click, {
    click <- input$map_click
    if (start_port_mode()) {
      pt <- st_sf(STATION_NAME = "Starting Port", COMMENTS = "Harbor", geometry = st_sfc(st_point(c(click$lng, click$lat)), crs = 4326))
      start_port(pt)
      start_port_mode(FALSE)
      leafletProxy("map") %>% clearGroup("start_port_layer") %>% 
        addMarkers(data = pt, group = "start_port_layer", label = "Departure Port")
    } else if (end_port_mode()) {
      pt <- st_sf(STATION_NAME = "Ending Port", COMMENTS = "Harbor", geometry = st_sfc(st_point(c(click$lng, click$lat)), crs = 4326))
      end_port(pt)
      end_port_mode(FALSE)
      leafletProxy("map") %>% clearGroup("end_port_layer") %>% 
        addMarkers(data = pt, group = "end_port_layer", label = "Arrival Port")
    }
  })
  
  # ---- CATCH SURVEY SEGMENTS & APPEND ----
  observeEvent(input$map_draw_new_feature, {
    feat <- input$map_draw_new_feature
    req(feat$geometry$type == "LineString")
    
    raw_coords <- do.call(rbind, lapply(feat$geometry$coordinates, as.numeric))
    
    current_survey <- survey_route_pts()
    start_counter <- if(is.null(current_survey)) 0 else nrow(current_survey)
    
    new_snapped_list <- list()
    for(i in 1:nrow(raw_coords)) {
      click_pt <- st_sfc(st_point(c(raw_coords[i, 1], raw_coords[i, 2])), crs = 4326)
      distances <- st_distance(click_pt, points_sf_raw)
      closest_idx <- as.numeric(which.min(distances))
      
      new_snapped_list[[i]] <- points_sf_raw[closest_idx, ] %>%
        mutate(sequence_order = start_counter + i)
    }
    
    new_segment_df <- do.call(rbind, new_snapped_list)
    
    if(is.null(current_survey)) {
      survey_route_pts(new_segment_df)
    } else {
      survey_route_pts(rbind(current_survey, new_segment_df))
    }
    
    # Remove the crude drawn sketch lines to keep map clean
    leafletProxy("map") %>% removeDrawToolbar(clearFeatures = TRUE)
  })
  
  # ---- MASTER TRACK COMPILER ----
  # Combines: Start Port -> Survey Points -> End Port into one master object
  master_combined_pts <- reactive({
    survey <- survey_route_pts()
    start_p <- start_port()
    end_p <- end_port()
    
    std_columns <- function(x) {
      if(is.null(x)) return(NULL)
      x %>% dplyr::select(STATION_NAME, COMMENTS, geometry)
    }
    
    df <- data.frame()
    if(!is.null(start_p)) df <- rbind(df, std_columns(start_p))
    if(!is.null(survey)) df <- rbind(df, std_columns(survey))
    if(!is.null(end_p)) df <- rbind(df, std_columns(end_p))
    
    if(nrow(df) == 0) return(NULL)
    return(df)
  })
  
  # ---- RENDER COMBINED TRACK LINE & LABELS ----
  observe({
    combined <- master_combined_pts()
    req(combined)
    if(nrow(combined) < 2) return()
    
    route_line <- st_cast(st_combine(combined), "LINESTRING")
    
    leafletProxy("map") %>% 
      clearGroup("snapped_layer") %>%
      addPolylines(
        data = route_line, color = "#28a745", weight = 4, opacity = 0.8, group = "snapped_layer"
      )
    
    # Generate sequential step counter bubbles over the path vertices
    leafletProxy("map") %>% clearGroup("sequence_labels")
    for(i in 1:nrow(combined)) {
      leafletProxy("map") %>%
        addLabelOnlyMarkers(
          lng = st_coordinates(combined[i, ])[1], lat = st_coordinates(combined[i, ])[2],
          label = as.character(i), group = "sequence_labels",
          labelOptions = labelOptions(
            noHide = TRUE, direction = "center", textOnly = FALSE,
            style = list("font-weight" = "bold", "color" = "white", 
                         "background-color" = "#0275d8", "border-color" = "#025aa5",
                         "padding" = "2px 6px", "border-radius" = "50%")
          )
        )
    }
  })
  
  # Wipe Memory and Canvas
  observeEvent(input$clear_track, {
    master_route_pts(NULL)
    survey_route_pts(NULL)
    start_port(NULL)
    end_port(NULL)
    leafletProxy("map") %>% 
      clearGroup("snapped_layer") %>% clearGroup("sequence_labels") %>% 
      clearGroup("start_port_layer") %>% clearGroup("end_port_layer") %>% 
      clearShapes() %>% removeDrawToolbar(clearFeatures = TRUE)
  })
  
  # ---- LIVE PORT-TO-PORT METRICS ----
  output$total_distance <- renderText({
    combined <- master_combined_pts()
    req(combined)
    if(nrow(combined) < 2) return("0.00 NM")
    
    total_meters <- 0
    for(i in 1:(nrow(combined)-1)) {
      total_meters <- total_meters + as.numeric(st_distance(combined[i,], combined[i+1,]))
    }
    return(paste0(sprintf("%.2f", total_meters / 1852), " NM"))
  })
  
  output$station_count <- renderText({
    survey <- survey_route_pts()
    if(is.null(survey)) return("Dredge stations selected: 0")
    paste("Dredge stations selected:", nrow(survey))
  })
  
  # ---- MASTER GPX EXPORT HANDLER ----
  output$download_gpx <- downloadHandler(
    filename = function() { paste0("port_to_port_dredge_track_", Sys.Date(), ".gpx") },
    content = function(file) {
      combined <- master_combined_pts()
      req(combined)
      
      gpx_data <- combined %>% 
        dplyr::select(name = STATION_NAME)
      
      st_write(gpx_data, dsn = file, driver = "GPX", layer = "waypoints", dataset_options = "FORCE_GPX=YES", quiet = TRUE)
    }
  )
}

shinyApp(ui, server)


