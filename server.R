# Load all libraries
library(xml2)
library(rvest)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(leaflet)
library(highcharter)
library(tidyverse)
library(tmap)
library(sf)
library(RColorBrewer)
library(cartogram)
library(DT)
library(forcats)
library(shinycssloaders)

# Read all required data and shapefiles
df <- readRDS("Rodent_Inspection_clean_R1.rds")
nycnew <- st_read("nyu_2451_34509.shp")

# Convert data types
df$INSPECTION_TYPE <- as.factor(df$INSPECTION_TYPE)
df$BOROUGH <- as.factor(df$BOROUGH)
df$INSPECTION_DATE <- as.Date(df$INSPECTION_DATE)
df$RESULT <- as.factor(df$RESULT)
df$YEAR <- as.factor(df$YEAR)
df$MONTH <- as.factor(df$MONTH)
df$DAY <- as.integer(df$DAY)

# Make a copy of data frame
df_copy <- data.frame(df)

# New datatable data frame for outputting render data table
dt_df <-
  df[, c(
    'JOB_ID',
    'JOB_PROGRESS',
    'STREET_NAME',
    'INSPECTION_TYPE',
    'RESULT',
    "INSPECTION_DATE",
    "BOROUGH",
    "YEAR",
    "MONTH"
  )]

# Change the levels of the df_copy
df_copy$RESULT <-
  fct_relevel(
    df_copy$RESULT,
    "Rat Activity",
    "Failed for Other R",
    "Cleanup done",
    "Passed",
    "Monitoring visit",
    "Bait applied",
    "Stoppage done"
  )

# Create a month vector
months <-
  c(
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December"
  )

# Create 2 dataframes for group by of Passed & Rat Activity
top_10_p <- df %>%
  filter(RESULT == "Passed") %>%
  group_by(BOROUGH, STREET_NAME, RESULT) %>%
  summarise(count = n()) %>%
  arrange(-count)
top_10_p <- top_10_p[1:10,]

top_10_r <- df %>%
  filter(RESULT == "Rat Activity") %>%
  group_by(BOROUGH, STREET_NAME, RESULT) %>%
  summarise(count = n()) %>%
  arrange(-count)
top_10_r <- top_10_r[1:10,]

# Separate the data for graphs use - Inspection Type (Tab 3)
dfInCo <- df %>%
  filter(INSPECTION_TYPE == 'Initial' |
           INSPECTION_TYPE == 'Compliance')
dfActions <- df %>%
  filter(INSPECTION_TYPE != 'Initial' &
           INSPECTION_TYPE != 'Compliance')

############### Server for this application ###############
server <- function(input, output, session) {
  ############### Tab 1 ###############
  
  # Create our filtered data for plotting map
  geo_df <- reactive({
    geo_df <- df_copy %>%
      filter(
        BOROUGH == input$borough &
          YEAR == input$year &
          MONTH == input$month &
          !RESULT %in% c("Monitoring visit", "Bait applied", "Stoppage done")
      ) %>%
      select(BOROUGH, STREET_NAME, RESULT, LATITUDE, LONGITUDE) %>%
      group_by(BOROUGH, STREET_NAME, RESULT) %>%
      filter(row_number() == 1)
  })
  
  # colorfactor() conveniently maps factor/char data values to colors according to a given palette.
  pal = reactive({
    pal <-
      colorFactor(
        palette = c("#FB6962", "#FCFC99", "#79DE79", "#0CC078"),
        domain = geo_df()$RESULT
      )
  })
  
  # Create another one for the toggle button
  toggle <- reactiveValues()
  
  # Leaflet map with 2 markers
  output$map <- renderLeaflet({
    # Change the map initial view based on user input borough
    if (input$borough == "Manhattan") {
      long <- -73.96625
      lat <- 40.78343
    } else if (input$borough == "Bronx") {
      long <- -73.865433
      lat <- 40.837048
    } else if (input$borough == "Brooklyn") {
      long <- -73.949997
      lat <- 40.650002
    } else if (input$borough == "Queens") {
      long <- -73.769417
      lat <- 40.742054
    } else if (input$borough == "Staten Island") {
      long <- -74.151535
      lat <- 40.579021
    }
    
    map <- leaflet(data = geo_df()) %>%
      setView(long, lat, zoom = 12) %>%
      addProviderTiles("CartoDB.Positron")
    
    # Legend HTML generator:
    markerLegendHTML <- function(IconSet) {
      # Container div:
      legendHtml <-
        "<div style='padding: 10px; padding-bottom: 10px;'><h4 style='padding-top:0; padding-bottom:10px; margin: 0;'> Inspection Result </h4>"
      
      n <- 1
      
      # Add each icon for glyphicon icons:
      for (Icon in IconSet) {
        if (Icon[["library"]] == "glyphicon") {
          legendHtml <-
            paste0(
              legendHtml,
              "<div style='width: auto; height: 45px'>",
              "<div style='position: relative; display: inline-block; width: 36px; height: 45px' class='awesome-marker-icon-",
              Icon[["markerColor"]],
              " awesome-marker'>",
              "<i style='margin-left: 5px; margin-top: 11px; 'class= 'glyphicon glyphicon-",
              Icon[["icon"]],
              " '></i>",
              "</div>",
              "<p style='position: relative; top: 15px; display: inline-block; ' >",
              names(IconSet)[n] ,
              "</p>",
              "</div>"
            )
        }
        n <- n + 1
      }
      paste0(legendHtml, "</div>")
    }
    
    # Define icon list
    IconSet <- awesomeIconList(
      "Rat Activity" = makeAwesomeIcon(
        icon = "exclamation-sign",
        markerColor = "red",
        iconColor = "black"
      ),
      "Passed" = makeAwesomeIcon(
        icon = "ok-sign",
        markerColor = "lightgreen",
        iconColor = "black"
      ),
      "Failed for Other R" = makeAwesomeIcon(
        icon = "remove-sign",
        markerColor = "orange",
        iconColor = "black"
      ),
      "Cleanup done" = makeAwesomeIcon(
        icon = "trash",
        markerColor = "green",
        iconColor = "black"
      )
    )
    
    # Subset data for the result group layers
    for (r in unique(geo_df()$RESULT)) {
      sub = geo_df()[geo_df()$RESULT == r, ]
      map = map %>%
        addAwesomeMarkers(
          data = sub,
          lng = ~ LONGITUDE,
          lat = ~ LATITUDE,
          layerId =  ~ STREET_NAME,
          icon = ~ IconSet[r],
          popup =  ~ STREET_NAME,
          group = r
        )
    }
    
    # Add control layers and legend to the map.
    map %>%
      addLayersControl(
        overlayGroups = unique(geo_df()$RESULT),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      
      # Add legend to map
      addControl(html = markerLegendHTML(IconSet = IconSet),
                 position = 'bottomright')
    
  })
  
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker = NULL)
  
  # store the click
  observeEvent(input$map_marker_click, {
    data_of_click$clickedMarker <- input$map_marker_click
  })
  
  # If user change the borough, clear any clicked marker
  observeEvent(input$borough, {
    data_of_click$clickedMarker <- NULL
  })
  
  # Reset with observe event
  observeEvent(input$alt_table, {
    if (input$alt_table) {
      toggle$value <- TRUE
    } else {
      toggle$value <- FALSE
    }
    
  })
  
  # Output a table
  output$table <- DT::renderDataTable({
    table_obj <- DT::datatable(
      if (toggle$value == FALSE) {
        if (is.null(data_of_click$clickedMarker)) {
          dt_df[dt_df$BOROUGH == input$borough &
                  dt_df$YEAR == input$year &
                  dt_df$MONTH == input$month,-c(3, 7:9)][order(dt_df[dt_df$BOROUGH == input$borough &
                                                                       dt_df$YEAR == input$year &
                                                                       dt_df$MONTH == input$month,-c(3, 7:9)][, 1],-dt_df[dt_df$BOROUGH == input$borough &
                                                                                                                            dt_df$YEAR == input$year &
                                                                                                                            dt_df$MONTH == input$month,-c(3, 7:9)][, 2]), ]
        } else {
          dt_df[dt_df$STREET_NAME == data_of_click$clickedMarker$id &
                  dt_df$BOROUGH == input$borough &
                  dt_df$YEAR == input$year &
                  dt_df$MONTH == input$month,-c(3, 7:9)][order(dt_df[dt_df$STREET_NAME == data_of_click$clickedMarker$id &
                                                                       dt_df$BOROUGH == input$borough &
                                                                       dt_df$YEAR == input$year &
                                                                       dt_df$MONTH == input$month,-c(3, 7:9)][, 1],-dt_df[dt_df$STREET_NAME == data_of_click$clickedMarker$id &
                                                                                                                            dt_df$BOROUGH == input$borough &
                                                                                                                            dt_df$YEAR == input$year &
                                                                                                                            dt_df$MONTH == input$month,-c(3, 7:9)][, 2]), ]
        }
      } else if (toggle$value == TRUE) {
        validate(need(
          !is.null(data_of_click$clickedMarker),
          'Please select a street by clicking the marker'
        ))
        if (!is.null(data_of_click$clickedMarker)) {
          dt_df[dt_df$STREET_NAME == data_of_click$clickedMarker$id,-c(3, 7:9)][order(dt_df[dt_df$STREET_NAME == data_of_click$clickedMarker$id,-c(3, 7:9)][, 1],-dt_df[dt_df$STREET_NAME == data_of_click$clickedMarker$id,-c(3, 7:9)][, 2]), ]
        }
      },
      options = list(
        lengthMenu = c(10, 25, 50, 100),
        pageLength = 15
      ),
      filter = 'top',
      rownames = FALSE
    ) %>%
      formatStyle(
        'RESULT',
        target = 'row',
        color = styleEqual('Rat Activity', "#FFFFFF"),
        backgroundColor = styleEqual('Rat Activity', '#FB6962'),
        fontWeight = styleEqual('Rat Activity', 'bold')
      )
  })
  
  # Output the street name on top of table
  output$outputStreet <- renderText({
    if (is.null(data_of_click$clickedMarker)) {
      paste("Borough: ", input$borough)
    }
    else
      (paste("Street Name: ", data_of_click$clickedMarker$id))
  })
  
  # Output month and year
  output$date <- renderUI({
    HTML(paste(HTML('&nbsp;'), HTML('&nbsp;'), months[input$month], input$year))
  })
  
  ############### Tab 2 ###############
  
  # A reactive df for the icons
  icon_df <- reactive({
    icon_df <-
      data.frame(
        result = c(
          "Rat Activity",
          "Failed for Other R",
          "Passed",
          "Monitoring visit",
          "Stoppage done",
          "Cleanup done",
          "Bait applied"
        ),
        icon_match = c(
          "glyphicon-exclamation-sign",
          "glyphicon-remove-sign",
          "glyphicon-ok-sign",
          "glyphicon-user",
          "glyphicon-wrench",
          "glyphicon-trash",
          "glyphicon-compressed"
        ),
        stringsAsFactors = FALSE
      )
  })
  
  # Create a dynamic output using observe to update the checkbox input
  observe({
    if (input$type == "Compliance") {
      updatePickerInput(
        session,
        "result",
        choices = as.list(c(
          "Rat Activity", "Failed for Other R", "Passed"
        )),
        choicesOpt = list(icon = icon_df()[icon_df()$result %in% unique(as.character(df[df$INSPECTION_TYPE == input$type, ]$RESULT)), ]$icon_match)
      )
    } else {
      updatePickerInput(
        session,
        "result",
        choices = as.list(unique(as.character(df[df$INSPECTION_TYPE == input$type, ]$RESULT))),
        choicesOpt = list(icon = icon_df()[icon_df()$result %in% unique(as.character(df[df$INSPECTION_TYPE == input$type, ]$RESULT)), ]$icon_match)
      )
    }
  })
  
  # Create an if else for filtered zipcode group by that is reactive
  zipcode_group <- reactive({
    if (is.null(input$type) & (input$result == "")) {
      zipcode_group <- df %>%
        group_by(ZIP_CODE) %>%
        summarise(inspection_count = n()) %>%
        arrange(-inspection_count)
    } else if ((input$result == "") & !is.null(input$type)) {
      zipcode_group <- df %>%
        filter(INSPECTION_TYPE == input$type) %>%
        group_by(ZIP_CODE) %>%
        summarise(inspection_count = n()) %>%
        arrange(-inspection_count)
    } else if (!is.null(input$result) &
               (input$result != "") & !is.null(input$type)) {
      zipcode_group <- df %>%
        filter(INSPECTION_TYPE == input$type &
                 RESULT == input$result) %>%
        group_by(ZIP_CODE) %>%
        summarise(inspection_count = n()) %>%
        arrange(-inspection_count)
    }
  })
  
  # Another group by for top street name
  zcmax_group <- reactive({
    if (is.null(input$type) & (input$result == "")) {
      zcmax_group <- df %>%
        group_by(ZIP_CODE, STREET_NAME) %>%
        summarise(count = n()) %>%
        group_by(ZIP_CODE) %>%
        top_n(1, count)
    } else if ((input$result == "") & !is.null(input$type)) {
      zcmax_group <- df %>%
        filter(INSPECTION_TYPE == input$type) %>%
        group_by(ZIP_CODE, STREET_NAME) %>%
        summarise(count = n()) %>%
        group_by(ZIP_CODE) %>%
        top_n(1, count)
    } else if (!is.null(input$result) &
               (input$result != "") & !is.null(input$type)) {
      zcmax_group <- df %>%
        filter(INSPECTION_TYPE == input$type &
                 RESULT == input$result) %>%
        group_by(ZIP_CODE, STREET_NAME) %>%
        summarise(count = n()) %>%
        group_by(ZIP_CODE) %>%
        top_n(1, count)
    }
  })
  
  # Create our filtered data for plotting map
  nycmap <- reactive({
    nycnew$count <- as.numeric(0)
    for (i in 1:length(nycnew$zcta)) {
      nycnew$count[i] <-
        zipcode_group()[zipcode_group()$ZIP_CODE == nycnew$zcta[i], 'inspection_count']
    }
    
    # Replace all integer(0) values to real value of zero
    for (i in 1:length(nycnew$count)) {
      if (length(nycnew$count[[i]]) < 1) {
        nycnew$count[[i]] <- as.integer(0)
      }
    }
    
    # Change the data type of zip code
    nycnew$count <- unlist(nycnew$count)
    
    # Second addition of feature (top street)
    nycnew$top_street <- as.character("")
    nycnew$top_count <- as.numeric(0)
    
    for (i in 1:length(nycnew$zcta)) {
      nycnew$top_street[i] <-
        zcmax_group()[zcmax_group()$ZIP_CODE == nycnew$zcta[i], 'STREET_NAME']
      nycnew$top_count[i] <-
        zcmax_group()[zcmax_group()$ZIP_CODE == nycnew$zcta[i], 'count']
    }
    
    # Replace all character(0) values to real value of string "None" (Not sure yet)
    for (i in 1:length(nycnew$top_street)) {
      if (length(nycnew$top_street[[i]]) < 1) {
        nycnew$top_street[[i]] <- as.character("None")
      } else if (length(nycnew$top_street[[i]]) > 1) {
        nycnew$top_street[[i]] <-
          paste(nycnew$top_street[[i]][1], ",", nycnew$top_street[[i]][2])
      }
    }
    
    # Replace all integer(0) for top_count column
    for (i in 1:length(nycnew$top_count)) {
      if (length(nycnew$top_count[[i]]) < 1) {
        nycnew$top_count[[i]] <- as.integer(0)
      } else if (length(nycnew$top_count[[i]]) > 1) {
        nycnew$top_count[[i]] <- nycnew$top_count[[i]][1]
      }
    }
    
    # Change the data type of top street and top count
    nycnew$top_street <- unlist(nycnew$top_street)
    nycnew$top_count <- unlist(nycnew$top_count)
    
    # Define the new data
    nycmap <- nycnew
    
  })
  
  # Create a reactive value that will store the click position
  data_of_click_rat <- reactiveValues(clickedStreet = NULL)
  data_of_click_pass <- reactiveValues(clickedStreet = NULL)
  
  # Define the bar chart hcontainer
  # Bar chart for rat activity
  output$hcrat <- renderHighchart({
    top_10_rat <- df %>%
      filter(RESULT == "Rat Activity") %>%
      group_by(BOROUGH, STREET_NAME, RESULT) %>%
      summarise(count = n()) %>%
      arrange(-count)
    top_10_rat <- top_10_rat[1:10, ]
    
    barClickedFunction1 <-
      JS(
        "function(event) {Shiny.onInputChange('barClickedRat', [this.name, event.point.category]);}"
      )
    
    highchart() %>%
      hc_xAxis(categories = top_10_rat$STREET_NAME,
               title = list(text = 'STREET NAME')) %>%
      hc_yAxis(title = list(text = 'COUNT')) %>%
      hc_title(text = 'Rat Activity') %>%
      hc_add_series(
        name = "Top Rat Activity Streets",
        data = top_10_rat,
        mapping = hcaes(x = STREET_NAME, y = count),
        type = "bar",
        color = "#FB6962",
        tooltip = list(pointFormat = "Borough: {point.BOROUGH}, Count = {point.count}")
      ) %>%
      hc_plotOptions(series = list(
        stacking = FALSE,
        allowPointSelect = TRUE,
        events = list(click = barClickedFunction1)
      )) %>%
      hc_tooltip(crosshairs = TRUE)
  })
  
  # Bar chart for passed
  output$hcpass <- renderHighchart({
    top_10_pass <- df %>%
      filter(RESULT == "Passed") %>%
      group_by(BOROUGH, STREET_NAME, RESULT) %>%
      summarise(count = n()) %>%
      arrange(-count)
    top_10_pass <- top_10_pass[1:10, ]
    
    barClickedFunction2 <-
      JS(
        "function(event) {Shiny.onInputChange('barClickedPass', [this.name, event.point.category]);}"
      )
    
    highchart() %>%
      hc_xAxis(categories = top_10_pass$STREET_NAME,
               title = list(text = 'STREET NAME')) %>%
      hc_yAxis(title = list(text = 'COUNT')) %>%
      hc_title(text = 'Passed') %>%
      hc_add_series(
        name = "Top Passed Streets",
        data = top_10_pass,
        mapping = hcaes(x = STREET_NAME, y = count),
        type = "bar",
        color = "lightgreen",
        tooltip = list(pointFormat = "Borough: {point.BOROUGH}, Count = {point.count}")
      ) %>%
      hc_plotOptions(series = list(
        stacking = FALSE,
        allowPointSelect = TRUE,
        events = list(click = barClickedFunction2)
      )) %>%
      hc_tooltip(crosshairs = TRUE)
  })
  
  # Store the click item for hcrat
  observeEvent(input$barClickedRat, {
    data_of_click_rat$clickedStreet <- input$barClickedRat
  })
  
  # Store the click item for hcpass
  observeEvent(input$barClickedPass, {
    data_of_click_pass$clickedStreet <- input$barClickedPass
  })
  
  # Create a reactive temp df for Passed
  temp_pass <- reactive({
    if (!is.null(data_of_click_pass$clickedStreet)) {
      temp_pass <- df %>%
        filter(
          RESULT == "Passed" &
            STREET_NAME == data_of_click_pass$clickedStreet[2] &
            BOROUGH == top_10_p[top_10_p$STREET_NAME == data_of_click_pass$clickedStreet[2], ]$BOROUGH
        ) %>%
        select(BOROUGH, STREET_NAME, RESULT, LATITUDE, LONGITUDE) %>%
        group_by(BOROUGH, STREET_NAME, RESULT) %>%
        filter(row_number() == 1) %>%
        st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
                 crs = 4326)
    }
  })
  
  # Create a reactive temp df for Rat activity
  temp_rat <- reactive({
    if (!is.null(data_of_click_rat$clickedStreet)) {
      temp_rat <- df %>%
        filter(
          RESULT == "Rat Activity" &
            STREET_NAME == data_of_click_rat$clickedStreet[2] &
            BOROUGH == top_10_r[top_10_r$STREET_NAME == data_of_click_rat$clickedStreet[2], ]$BOROUGH
        ) %>%
        select(BOROUGH, STREET_NAME, RESULT, LATITUDE, LONGITUDE) %>%
        group_by(BOROUGH, STREET_NAME, RESULT) %>%
        filter(row_number() == 1) %>%
        st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
                 crs = 4326)
    }
  })
  
  # Tmap library map with blue themed
  output$mapT <- renderTmap({
    if (is.null(data_of_click_pass$clickedStreet) &
        is.null(data_of_click_rat$clickedStreet)) {
      if (input$type == "Stoppage") {
        tm_shape(nycmap()) +
          tm_polygons(
            "count",
            style = "fixed",
            breaks = c(0, 1, 5, 10, 20, 40),
            palette = "Blues",
            popup.vars = c(
              "Most Inspected Street: " = "top_street",
              "Count for Individual Street: " = "top_count",
              "Total Inspection Count: " = "count"
            ),
            id = "zcta"
          )
      } else if (input$type == "Clean Ups") {
        tm_shape(nycmap()) +
          tm_polygons(
            "count",
            style = "fixed",
            breaks = c(0, 1, 5, 10, 20, 40),
            palette = "Blues",
            popup.vars = c(
              "Most Inspected Street: " = "top_street",
              "Count for Individual Street: " = "top_count",
              "Total Inspection Count: " = "count"
            ),
            id = "zcta"
          )
      } else {
        tm_shape(nycmap()) +
          tm_polygons(
            "count",
            style = "quantile",
            palette = "Blues",
            popup.vars = c(
              "Most Inspected Street: " = "top_street",
              "Count for Individual Street: " = "top_count",
              "Total Inspection Count: " = "count"
            ),
            id = "zcta"
          )
      }
    } else if (!is.null(data_of_click_pass$clickedStreet) &
               is.null(data_of_click_rat$clickedStreet)) {
      if (input$type == "Stoppage") {
        tm_shape(nycmap()) +
          tm_polygons(
            "count",
            style = "fixed",
            breaks = c(0, 1, 5, 10, 20, 40),
            palette = "Blues",
            popup.vars = c(
              "Most Inspected Street: " = "top_street",
              "Count for Individual Street: " = "top_count",
              "Total Inspection Count: " = "count"
            ),
            id = "zcta"
          ) +
          tm_shape(temp_pass()) + tm_text(
            "STREET_NAME",
            size = 1.75,
            col = "black",
            bg.alpha = 0.7
          )
      } else if (input$type == "Clean Ups") {
        tm_shape(nycmap()) +
          tm_polygons(
            "count",
            style = "fixed",
            breaks = c(0, 1, 5, 10, 20, 40),
            palette = "Blues",
            popup.vars = c(
              "Most Inspected Street: " = "top_street",
              "Count for Individual Street: " = "top_count",
              "Total Inspection Count: " = "count"
            ),
            id = "zcta"
          ) +
          tm_shape(temp_pass()) + tm_text(
            "STREET_NAME",
            size = 1.75,
            col = "black",
            bg.alpha = 0.7
          )
      } else {
        tm_shape(nycmap()) +
          tm_polygons(
            "count",
            style = "quantile",
            palette = "Blues",
            popup.vars = c(
              "Most Inspected Street: " = "top_street",
              "Count for Individual Street: " = "top_count",
              "Total Inspection Count: " = "count"
            ),
            id = "zcta"
          ) +
          tm_shape(temp_pass()) + tm_text(
            "STREET_NAME",
            size = 1.75,
            col = "white",
            bg.alpha = 0.7
          )
      }
    } else if (is.null(data_of_click_pass$clickedStreet) &
               !is.null(data_of_click_rat$clickedStreet)) {
      if (input$type == "Stoppage") {
        tm_shape(nycmap()) +
          tm_polygons(
            "count",
            style = "fixed",
            breaks = c(0, 1, 5, 10, 20, 40),
            palette = "Blues",
            popup.vars = c(
              "Most Inspected Street: " = "top_street",
              "Count for Individual Street: " = "top_count",
              "Total Inspection Count: " = "count"
            ),
            id = "zcta"
          ) +
          tm_shape(temp_rat()) + tm_text(
            "STREET_NAME",
            size = 1.75,
            col = "black",
            bg.alpha = 0.7
          )
      } else if (input$type == "Clean Ups") {
        tm_shape(nycmap()) +
          tm_polygons(
            "count",
            style = "fixed",
            breaks = c(0, 1, 5, 10, 20, 40),
            palette = "Blues",
            popup.vars = c(
              "Most Inspected Street: " = "top_street",
              "Count for Individual Street: " = "top_count",
              "Total Inspection Count: " = "count"
            ),
            id = "zcta"
          ) +
          tm_shape(temp_rat()) + tm_text(
            "STREET_NAME",
            size = 1.75,
            col = "black",
            bg.alpha = 0.7
          )
      } else {
        tm_shape(nycmap()) +
          tm_polygons(
            "count",
            style = "quantile",
            palette = "Blues",
            popup.vars = c(
              "Most Inspected Street: " = "top_street",
              "Count for Individual Street: " = "top_count",
              "Total Inspection Count: " = "count"
            ),
            id = "zcta"
          ) +
          tm_shape(temp_rat()) + tm_text(
            "STREET_NAME",
            size = 1.75,
            col = "white",
            bg.alpha = 0.7
          )
      }
    } else if (!is.null(data_of_click_pass$clickedStreet) &
               !is.null(data_of_click_rat$clickedStreet)) {
      if (input$type == "Stoppage") {
        tm_shape(nycmap()) +
          tm_polygons(
            "count",
            style = "fixed",
            breaks = c(0, 1, 5, 10, 20, 40),
            palette = "Blues",
            popup.vars = c(
              "Most Inspected Street: " = "top_street",
              "Count for Individual Street: " = "top_count",
              "Total Inspection Count: " = "count"
            ),
            id = "zcta"
          ) +
          tm_shape(temp_pass()) + tm_text(
            "STREET_NAME",
            size = 1.75,
            col = "black",
            bg.alpha = 0.7
          ) +
          tm_shape(temp_rat()) + tm_text(
            "STREET_NAME",
            size = 1.75,
            col = "black",
            bg.alpha = 0.7
          )
      } else if (input$type == "Clean Ups") {
        tm_shape(nycmap()) +
          tm_polygons(
            "count",
            style = "fixed",
            breaks = c(0, 1, 5, 10, 20, 40),
            palette = "Blues",
            popup.vars = c(
              "Most Inspected Street: " = "top_street",
              "Count for Individual Street: " = "top_count",
              "Total Inspection Count: " = "count"
            ),
            id = "zcta"
          ) +
          tm_shape(temp_pass()) + tm_text(
            "STREET_NAME",
            size = 1.75,
            col = "black",
            bg.alpha = 0.7
          ) +
          tm_shape(temp_rat()) + tm_text(
            "STREET_NAME",
            size = 1.75,
            col = "black",
            bg.alpha = 0.7
          )
      } else {
        tm_shape(nycmap()) +
          tm_polygons(
            "count",
            style = "quantile",
            palette = "Blues",
            popup.vars = c(
              "Most Inspected Street: " = "top_street",
              "Count for Individual Street: " = "top_count",
              "Total Inspection Count: " = "count"
            ),
            id = "zcta"
          ) +
          tm_shape(temp_pass()) + tm_text(
            "STREET_NAME",
            size = 1.75,
            col = "white",
            bg.alpha = 0.7
          ) +
          tm_shape(temp_rat()) + tm_text(
            "STREET_NAME",
            size = 1.75,
            col = "white",
            bg.alpha = 0.7
          )
      }
    }
  })
  
  # Reset with observe event
  observeEvent(input$reset, {
    data_of_click_pass$clickedStreet <- NULL
    data_of_click_rat$clickedStreet <- NULL
  })
  
  
  ############### Tab 3 ###############
  
  # Filter street name based on borough selected
  streets <- reactive({
    filterStreet <- df %>%
      select(STREET_NAME, BOROUGH) %>%
      filter(BOROUGH == input$boroughBD)
    diff_streets <- unique(filterStreet$STREET_NAME)
  })
  
  # Show the street names in dropdown list
  output$streetName <- renderUI({
    if (input$incStr == TRUE) {
      selectInput('streetName',
                  div(style = "font-size:22px;", "Street Name"),
                  choices = streets())
    }
  })
  
  # All the output for each element
  output$RA1 <- renderValueBox({
    vbInsR('Rat Activity')
  })
  output$PA1 <- renderValueBox({
    vbInsR('Passed')
  })
  output$FA1 <- renderValueBox({
    vbInsR('Failed for Other R')
  })
  output$graph1 <- renderHighchart({
    plotInsR('Rat Activity')
  })
  output$graph2 <- renderHighchart({
    plotInsR('Passed')
  })
  output$graph3 <- renderHighchart({
    plotInsR('Failed for Other R')
  })
  output$graph4 <-
    renderHighchart({
      plotInsA('Bait applied', 'Bait')
    })
  output$graph5 <-
    renderHighchart({
      plotInsA('Monitoring visit', 'Bait')
    })
  output$graph6 <-
    renderHighchart({
      plotInsA('Cleanup done', 'Clean Ups')
    })
  output$graph7 <-
    renderHighchart({
      plotInsA('Stoppage done', 'Stoppage')
    })
  output$BA1 <- renderValueBox({
    vbInsA('Bait applied')
  })
  output$BM1 <- renderValueBox({
    vbInsA('Monitoring visit')
  })
  output$CU1 <- renderValueBox({
    vbInsA('Cleanup done')
  })
  output$ST1 <- renderValueBox({
    vbInsA('Stoppage done')
  })
  
  # Generate counts for value box in Inspection Type - General
  vbInsR <- function(x) {
    # Validate - user need to check at least 1 checkbox
    validate(need(
      input$chkIni == TRUE |
        input$chkCom == TRUE,
      'Please select an inspection type'
    ))
    
    # Different icons and colors for different type
    if (x == 'Rat Activity') {
      colorBg = 'red'
      iconVB = 'exclamation-sign'
    }
    else if (x == 'Passed') {
      colorBg = 'green'
      iconVB = 'ok-sign'
    }
    else {
      colorBg = 'orange'
      iconVB = 'remove-sign'
    }
    
    # Generate the count according to the type, borough and street selected
    if (input$chkIni == TRUE & input$chkCom == TRUE) {
      if (input$incStr == FALSE) {
        counterBar1 <- dfInCo %>%
          filter(BOROUGH %in% input$boroughBD & RESULT %in% x) %>%
          group_by(RESULT) %>%
          count()
        if (nrow(counterBar1 != 0)) {
          valueBox(
            counterBar1$n,
            x,
            icon = icon(iconVB, lib = "glyphicon"),
            color = colorBg
          )
        }
        else{
          valueBox("0",
                   x,
                   icon = icon(iconVB, lib = "glyphicon"),
                   color = colorBg)
        }
        
      }
      else {
        counterBar1 <- dfInCo %>%
          filter(BOROUGH %in% input$boroughBD &
                   STREET_NAME %in% input$streetName &
                   RESULT %in% x) %>%
          group_by(RESULT) %>%
          count()
        if (nrow(counterBar1 != 0)) {
          valueBox(
            counterBar1$n,
            x,
            icon = icon(iconVB, lib = "glyphicon"),
            color = colorBg
          )
        }
        else{
          valueBox("0",
                   x,
                   icon = icon(iconVB, lib = "glyphicon"),
                   color = colorBg)
        }
        
      }
    }
    else if (input$chkIni == TRUE) {
      if (input$incStr == FALSE) {
        counterBar1 <- dfInCo %>%
          filter(BOROUGH %in% input$boroughBD &
                   RESULT %in% x &
                   INSPECTION_TYPE %in% 'Initial') %>%
          group_by(RESULT) %>%
          count()
        if (nrow(counterBar1 != 0)) {
          valueBox(
            counterBar1$n,
            x,
            icon = icon(iconVB, lib = "glyphicon"),
            color = colorBg
          )
        }
        else{
          valueBox("0",
                   x,
                   icon = icon(iconVB, lib = "glyphicon"),
                   color = colorBg)
        }
        
      }
      else {
        counterBar1 <- dfInCo %>%
          filter(
            BOROUGH %in% input$boroughBD &
              STREET_NAME %in% input$streetName &
              RESULT %in% x & INSPECTION_TYPE %in% 'Initial'
          ) %>%
          group_by(RESULT) %>%
          count()
        if (nrow(counterBar1 != 0)) {
          valueBox(
            counterBar1$n,
            x,
            icon = icon(iconVB, lib = "glyphicon"),
            color = colorBg
          )
        }
        else{
          valueBox("0",
                   x,
                   icon = icon(iconVB, lib = "glyphicon"),
                   color = colorBg)
        }
        
      }
    }
    else if (input$chkCom == TRUE) {
      if (input$incStr == FALSE) {
        counterBar1 <- dfInCo %>%
          filter(BOROUGH %in% input$boroughBD &
                   RESULT %in% x &
                   INSPECTION_TYPE %in% 'Compliance') %>%
          group_by(RESULT) %>%
          count()
        if (nrow(counterBar1 != 0)) {
          valueBox(
            counterBar1$n,
            x,
            icon = icon(iconVB, lib = "glyphicon"),
            color = colorBg
          )
        }
        else{
          valueBox("0",
                   x,
                   icon = icon(iconVB, lib = "glyphicon"),
                   color = colorBg)
        }
        
      }
      else {
        counterBar1 <- dfInCo %>%
          filter(
            BOROUGH %in% input$boroughBD &
              STREET_NAME %in% input$streetName &
              RESULT %in% x & INSPECTION_TYPE %in% 'Compliance'
          ) %>%
          group_by(RESULT) %>%
          count()
        if (nrow(counterBar1 != 0)) {
          valueBox(
            counterBar1$n,
            x,
            icon = icon(iconVB, lib = "glyphicon"),
            color = colorBg
          )
        }
        else{
          valueBox("0",
                   x,
                   icon = icon(iconVB, lib = "glyphicon"),
                   color = colorBg)
        }
      }
    }
    
  }
  
  # Plot area graphs for each inspection type
  plotInsR <- function(x) {
    validate(need(
      input$chkIni == TRUE |
        input$chkCom == TRUE,
      'Please select an inspection type'
    ))
    if (x == 'Rat Activity') {
      colorBg = '#FB6962'
    }
    else if (x == 'Passed') {
      colorBg = 'lightgreen'
    }
    else {
      colorBg = '#EFA721'
    }
    if (input$chkIni == TRUE & input$chkCom == TRUE) {
      if (input$incStr == FALSE) {
        a <- dfInCo %>%
          filter(BOROUGH %in% input$boroughBD & RESULT %in% x) %>%
          group_by(YEAR, MONTH, RESULT) %>%
          count()
        a$YM <-
          as.Date(with(a, paste(YEAR, MONTH, 1, sep = "-")), "%Y-%m-%d")
        validate(need(nrow(a) != 0, c('No data to display for ', x)))
        a %>% hchart('area', hcaes(
          x = as.Date(YM),
          y = n,
          group = RESULT
        )) %>%
          hc_title(text = x) %>%
          hc_plotOptions(area = list(color = colorBg, marker = list(enabled = FALSE))) %>%
          hc_xAxis(title = list(text = 'YEAR')) %>%
          hc_yAxis(title = list(text = 'COUNT'),
                   plotLines = list(
                     list(
                       color = '#FF0000',
                       value = median(a$n),
                       width = 2,
                       label = list(text = 'Median')
                     )
                   ))
      }
      else {
        a <- dfInCo %>%
          filter(BOROUGH %in% input$boroughBD &
                   STREET_NAME %in% input$streetName &
                   RESULT %in% x) %>%
          group_by(YEAR, MONTH, RESULT) %>%
          count()
        a$YM <-
          as.Date(with(a, paste(YEAR, MONTH, 1, sep = "-")), "%Y-%m-%d")
        validate(need(nrow(a) != 0, c('No data to display for ', x)))
        a %>% hchart('area', hcaes(
          x = as.Date(YM),
          y = n,
          group = RESULT
        )) %>%
          hc_title(text = x) %>%
          hc_plotOptions(area = list(color = colorBg, marker = list(enabled = FALSE))) %>%
          hc_xAxis(title = list(text = 'YEAR')) %>%
          hc_yAxis(title = list(text = 'COUNT'),
                   plotLines = list(
                     list(
                       color = '#FF0000',
                       value = median(a$n),
                       width = 2,
                       label = list(text = 'Median')
                     )
                   ))
      }
    }
    else if (input$chkIni == TRUE) {
      if (input$incStr == FALSE) {
        a <- dfInCo %>%
          filter(BOROUGH %in% input$boroughBD &
                   RESULT %in% x &
                   INSPECTION_TYPE %in% 'Initial') %>%
          group_by(YEAR, MONTH, INSPECTION_TYPE) %>%
          count()
        a$YM <-
          as.Date(with(a, paste(YEAR, MONTH, 1, sep = "-")), "%Y-%m-%d")
        validate(need(nrow(a) != 0, c('No data to display for ', x)))
        a %>% hchart('area', hcaes(
          x = as.Date(YM),
          y = n,
          group = INSPECTION_TYPE
        )) %>%
          hc_title(text = x) %>%
          hc_plotOptions(area = list(color = colorBg, marker = list(enabled = FALSE))) %>%
          hc_xAxis(title = list(text = 'YEAR')) %>%
          hc_yAxis(title = list(text = 'COUNT'),
                   plotLines = list(
                     list(
                       color = '#FF0000',
                       value = median(a$n),
                       width = 2,
                       label = list(text = 'Median')
                     )
                   ))
      }
      else {
        a <- dfInCo %>%
          filter(
            BOROUGH %in% input$boroughBD &
              STREET_NAME %in% input$streetName &
              RESULT %in% x & INSPECTION_TYPE %in% 'Initial'
          ) %>%
          group_by(YEAR, MONTH, INSPECTION_TYPE) %>%
          count()
        a$YM <-
          as.Date(with(a, paste(YEAR, MONTH, 1, sep = "-")), "%Y-%m-%d")
        validate(need(nrow(a) != 0, c('No data to display for ', x)))
        a %>% hchart('area', hcaes(
          x = as.Date(YM),
          y = n,
          group = INSPECTION_TYPE
        )) %>%
          hc_title(text = x) %>%
          hc_plotOptions(area = list(color = colorBg, marker = list(enabled = FALSE))) %>%
          hc_xAxis(title = list(text = 'YEAR')) %>%
          hc_yAxis(title = list(text = 'COUNT'),
                   plotLines = list(
                     list(
                       color = '#FF0000',
                       value = median(a$n),
                       width = 2,
                       label = list(text = 'Median')
                     )
                   ))
      }
    }
    else if (input$chkCom == TRUE) {
      if (input$incStr == FALSE) {
        a <- dfInCo %>%
          filter(BOROUGH %in% input$boroughBD &
                   RESULT %in% x &
                   INSPECTION_TYPE %in% 'Compliance') %>%
          group_by(YEAR, MONTH, INSPECTION_TYPE) %>%
          count()
        a$YM <-
          as.Date(with(a, paste(YEAR, MONTH, 1, sep = "-")), "%Y-%m-%d")
        validate(need(nrow(a) != 0, c('No data to display for ', x)))
        a %>% hchart('area', hcaes(
          x = as.Date(YM),
          y = n,
          group = INSPECTION_TYPE
        )) %>%
          hc_title(text = x) %>%
          hc_plotOptions(area = list(color = colorBg, marker = list(enabled = FALSE))) %>%
          hc_xAxis(title = list(text = 'YEAR')) %>%
          hc_yAxis(title = list(text = 'COUNT'),
                   plotLines = list(
                     list(
                       color = '#FF0000',
                       value = median(a$n),
                       width = 2,
                       label = list(text = 'Median')
                     )
                   ))
      }
      else {
        a <- dfInCo %>%
          filter(
            BOROUGH %in% input$boroughBD &
              STREET_NAME %in% input$streetName &
              RESULT %in% x & INSPECTION_TYPE %in% 'Compliance'
          ) %>%
          group_by(YEAR, MONTH, INSPECTION_TYPE) %>%
          count()
        a$YM <-
          as.Date(with(a, paste(YEAR, MONTH, 1, sep = "-")), "%Y-%m-%d")
        validate(need(nrow(a) != 0, c('No data to display for ', x)))
        a %>% hchart('area', hcaes(
          x = as.Date(YM),
          y = n,
          group = INSPECTION_TYPE
        )) %>%
          hc_title(text = x) %>%
          hc_plotOptions(area = list(color = colorBg, marker = list(enabled = FALSE))) %>%
          hc_xAxis(title = list(text = 'YEAR')) %>%
          hc_yAxis(title = list(text = 'COUNT'),
                   plotLines = list(
                     list(
                       color = '#FF0000',
                       value = median(a$n),
                       width = 2,
                       label = list(text = 'Median')
                     )
                   ))
      }
    }
  }
  
  # Generate value box for Inspection Type - Actions
  vbInsA <- function(x) {
    # Different colors and messages shown for different type
    if (x == 'Bait applied') {
      msg = 'Bait (Applied)'
      iconVB = 'compressed'
    }
    else if (x == 'Monitoring visit') {
      msg = 'Bait (Monitor)'
      iconVB = 'user'
    }
    else if (x == 'Cleanup done') {
      msg = 'Cleanup'
      iconVB = 'trash'
    }
    else {
      msg = 'Stoppage'
      iconVB = 'wrench'
    }
    
    # Generate count based on borough and street name selected
    if (input$incStr == FALSE) {
      counterBar1 <- dfActions %>%
        filter(BOROUGH %in% input$boroughBD & RESULT %in% x) %>%
        group_by(RESULT) %>%
        count()
      if (nrow(counterBar1 != 0)) {
        valueBox(
          counterBar1$n,
          msg,
          icon = icon(iconVB, lib = "glyphicon"),
          color = "aqua"
        )
      }
      else{
        valueBox("0",
                 msg,
                 icon = icon(iconVB, lib = "glyphicon"),
                 color = "aqua")
      }
      
    }
    else {
      counterBar1 <- dfActions %>%
        filter(BOROUGH %in% input$boroughBD &
                 STREET_NAME %in% input$streetName &
                 RESULT %in% x) %>%
        group_by(RESULT) %>%
        count()
      if (nrow(counterBar1 != 0)) {
        valueBox(
          counterBar1$n,
          msg,
          icon = icon(iconVB, lib = "glyphicon"),
          color = "aqua"
        )
      }
      else{
        valueBox("0",
                 msg,
                 icon = icon(iconVB, lib = "glyphicon"),
                 color = "aqua")
      }
    }
  }
  
  # Plot area graphs for each inspection type - actions
  plotInsA <- function(x, y) {
    if (x == 'Bait applied') {
      msg = 'Bait - Bait Applied'
    }
    else if (x == 'Monitoring visit') {
      msg = 'Bait - Monitoring Visit'
    }
    else if (x == 'Cleanup done') {
      msg = 'Clean Ups'
    }
    else {
      msg = 'Stoppage'
    }
    
    if (input$incStr == FALSE) {
      a <- dfActions %>%
        filter(BOROUGH %in% input$boroughBD &
                 INSPECTION_TYPE %in% y & RESULT %in% x) %>%
        group_by(YEAR, MONTH, INSPECTION_TYPE, RESULT) %>%
        count()
      a$YM <-
        as.Date(with(a, paste(YEAR, MONTH, 1, sep = "-")), "%Y-%m-%d")
      validate(need(nrow(a) != 0, c('No data to display for ', msg)))
      a %>% hchart('area', hcaes(
        x = as.Date(YM),
        y = n,
        group = INSPECTION_TYPE
      )) %>%
        hc_title(text = msg) %>%
        hc_plotOptions(area = list(color = 'lightblue', marker = list(enabled = FALSE))) %>%
        hc_xAxis(title = list(text = 'YEAR')) %>%
        hc_yAxis(title = list(text = 'COUNT'),
                 plotLines = list(
                   list(
                     color = '#FF0000',
                     value = median(a$n),
                     width = 2,
                     label = list(text = 'Median')
                   )
                 ))
    }
    else {
      a <- dfActions %>%
        filter(
          BOROUGH %in% input$boroughBD &
            STREET_NAME %in% input$streetName &
            INSPECTION_TYPE %in% y & RESULT %in% x
        ) %>%
        group_by(YEAR, MONTH, INSPECTION_TYPE, RESULT) %>%
        count()
      a$YM <-
        as.Date(with(a, paste(YEAR, MONTH, 1, sep = "-")), "%Y-%m-%d")
      validate(need(nrow(a) != 0, c('No data to display for ', msg)))
      a %>% hchart('area', hcaes(
        x = as.Date(YM),
        y = n,
        group = INSPECTION_TYPE
      )) %>%
        hc_title(text = msg) %>%
        hc_plotOptions(area = list(color = 'lightblue', marker = list(enabled = FALSE))) %>%
        hc_xAxis(title = list(text = 'YEAR')) %>%
        hc_yAxis(title = list(text = 'COUNT'),
                 plotLines = list(
                   list(
                     color = '#FF0000',
                     value = median(a$n),
                     width = 2,
                     label = list(text = 'Median')
                   )
                 ))
    }
  }
}