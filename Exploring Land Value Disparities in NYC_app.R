

library(shiny)
library(leaflet)
library(ggplot2)
library(plotly)
        
library(dplyr)        # data wrangling
library(cartogram)    # for the cartogram
library(broom)        # from geospatial format to data frame
library(tweenr)       # to create transition dataframe between 2 states
#library(gganimate)    # To realize the animation
library(maptools)     # world boundaries coordinates
library(viridis) 
library(mapproj)
library(leaflet)
library(plotly)
library(tidyverse)
library(mgcv)
library(gratia)
library(htmltools)
library(maps)
library(mapproj)
library(mapdata)
library(leaflet.extras)
library(ggmap)
library(tmaptools)
# library(leaflet.extras)
library(rlang)
library(RColorBrewer)


Data_filtered <- read.csv("dataset_group2.csv")
Data_filtered  = Data_filtered  %>% mutate(ave_land_scal = (ave_land-mean(ave_land))/sd(ave_land), .after = assesstot)


Sys.setenv("MAPBOX_TOKEN" = "pk.eyJ1Ijoic2FuZGFuaWsiLCJhIjoiY2xnZTh3cHZnMmlkdDNncXJlMzQwM3E0diJ9.cDHPG8d-JP7rY2Je4DHwBg")

borough_icons <- iconList(
  BK = makeIcon("https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png", iconWidth = 11, iconHeight = 22),
  BX = makeIcon("https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-green.png", iconWidth = 11, iconHeight = 22),
  MN = makeIcon("https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-orange.png", iconWidth = 11, iconHeight = 22),
  QN = makeIcon("https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-black.png", iconWidth = 11, iconHeight = 22),
  SI = makeIcon("https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png", iconWidth = 11, iconHeight = 22)
)

# Define UI for shiny app
ui <- fluidPage(
  # Sidebar panel
  sidebarPanel(
    # Select input for intensity column
    selectInput(inputId = "intensity_col",
                label = "Select intensity column:",
                choices = colnames(Data_filtered),
                selected = "ave_land"),
    # Select input for facet_wrap
    selectInput(inputId = "facet_col",
                label = "Select facet_wrap column (optional):",
                choices = c(NULL, colnames(Data_filtered)),
                selected = NULL),
    # Numeric input for radius
    numericInput(inputId = "radius",
                 label = "Radius:",
                 value = 20),
    # Numeric input for blur
    numericInput(inputId = "blur",
                 label = "Blur:",
                 value = 25),
    # Numeric input for min opacity
    numericInput(inputId = "min_opacity",
                 label = "Min Opacity:",
                 value = 0.09),
    # Select input for borough
    selectInput(inputId = "borough",
                label = "Select borough (optional):",
                choices = unique(Data_filtered$borough),
                multiple = TRUE,
                selected =  "MN" )
  ),
  # Main panel
  mainPanel(
    # Leaflet map
    leafletOutput(outputId = "map"),
    # ggplot plot
    plotlyOutput(outputId = "plot")
  )
)

# Define server logic for shiny app
server <- function(input, output) {
  # Render leaflet map
  output$map <- renderLeaflet({
    filtered_data <- Data_filtered
    if (!is.null(input$borough)) {
      filtered_data <- filtered_data[filtered_data$borough %in% input$borough, ]
    }
    p1 <- leaflet(data = filtered_data) %>% 
      addTiles() %>%
      addMarkers(
        ~longitude,
        ~latitude,
        icon = ~borough_icons[borough],
        label = ~paste(borough, ", Borough ",
                       ",Average_Land Value per sq foot:",scales::comma(ave_land, accuracy = 1),
                       ",Police Prcnt:",scales::comma(policeprct, accuracy = 1),
                       ",No. of Health Center:",scales::comma(healthcenterdistrict, accuracy = 1),
                       ",No. of Schools :",scales::comma(schooldist, accuracy = 1)
        )
      ) 
    addHeatmap(p1,
               lng =~filtered_data$longitude,
               lat =~filtered_data$latitude,
               intensity = filtered_data[[input$intensity_col]],
               max = max(filtered_data[[input$intensity_col]]),
               radius = input$radius,
               blur = input$blur,
               minOpacity = input$min_opacity)
  })
  
  # Render ggplot plot
  output$plot <- renderPlotly({
    filtered_data <- Data_filtered
    if (!is.null(input$borough)) {
      filtered_data <- filtered_data[filtered_data$borough %in% input$borough, ]
    }
    gg <- ggplot(filtered_data, aes(x = longitude, y = latitude, color = .data[[input$intensity_col]])) +
      geom_point(size = 10, contour = FALSE, alpha = 1) +
      scale_color_gradientn(colors = c(low = "blue", high = "red")) +
      theme_void()
    if (!is.null(input$facet_col)) {
      gg <- gg + facet_wrap(~ .data[[input$facet_col]],nrow=3)
    }
    ggplotly(gg)
  })
}


shinyApp(ui = ui, server = server)
