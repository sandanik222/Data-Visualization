library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(countrycode)
library(leaflet)
library(leaflet.extras)
library(readr)
library(Polychrome)

# Read data from CSV
setwd("/Users/sandanikumanayake/Desktop/STAT 697/FP/")
layoffs <- read_csv("layoffs_data.csv") %>% filter(!is.na(Percentage))
countries <- layoffs %>% group_by(Country) %>% summarise(mean_percentage = mean(Percentage))
industries <- layoffs %>% group_by(Industry) %>% summarise(mean_percentage = mean(Percentage))
world_cities <- read_csv("worldcities.csv")
coord_df <- layoffs
coord_df[coord_df$Location_HQ == "SF Bay Area", 2] <- "San Francisco"
colnames(coord_df) <- c("Company", "city", "Industry", "Laid_Off_Count", "Percentage", "Date", "Source", "Funds_Raised", "Stage", "Date_Added", "country", "List_of_Employees_Laid_Off")
coord_df <- inner_join(coord_df, world_cities, by = c("city", "country")) %>% 
  rename(Population = population)


layoffs1 <- layoffs %>%  filter(!is.na(Laid_Off_Count))

countries1 <- layoffs1 %>% 
  group_by(Country) %>% 
  dplyr::summarise(count_ct = n(),
                   layoff_count_ct = sum(Laid_Off_Count, na.rm= T))

industries1 <- layoffs1 %>% 
  group_by(Industry) %>% 
  summarise(ind_ct = n())


layoffs2 <- layoffs1 %>% 
  left_join(countries1)  %>%
  filter(count_ct >= 10) %>% 
  left_join(industries1) %>%
  filter(ind_ct > 40)

country_ind <- layoffs2 %>% 
  group_by(Country, Industry) %>%
  summarise(laid_off = sum(Laid_Off_Count, na.rm = T))

worstOne <-  layoffs2 %>% 
  group_by(Country, Industry, Company) %>%
  summarise(laid_off_comp = sum(Laid_Off_Count, na.rm = T))

worstCount <- worstOne %>% 
  group_by(Country, Industry) %>%
  summarise(laid_off_comp = max(laid_off_comp)) %>%
  left_join(worstOne)

country_ind2 <- country_ind %>% left_join(worstCount) %>% 
  left_join(countries1)  %>%
  mutate(perc = laid_off/layoff_count_ct)

Sys.setenv("MAPBOX_TOKEN" = "pk.eyJ1Ijoic2FuZGFuaWsiLCJhIjoiY2xnZTh3cHZnMmlkdDNncXJlMzQwM3E0diJ9.cDHPG8d-JP7rY2Je4DHwBg")

 



# Define UI for shiny app
ui <- fluidPage(
  # Sidebar panel
  sidebarPanel(
    
    # Select input for country
    selectInput(inputId = "country",
                label = "Select Country:",
                choices = c("All the Countries" = "", unique(coord_df$country)),
                multiple = TRUE),
    
    # Select input for industry
    selectInput(inputId = "Industry",
                label = "Select Industry:",
                choices = c("All the Industries" = "", unique(coord_df$Industry)),
                multiple = TRUE),
    
    # Select input for intensity column
    selectInput(inputId = "intensity_col",
                label = "Select Intensity column:",
                choices = colnames(coord_df)[c(4:5,8)],
                selected = "Percentage"),
    
    # Numeric input for radius
    numericInput(inputId = "radius",
                 label = "Radius:",
                 value = 3)
  ),
  # Main panel
  mainPanel(
    tabsetPanel(type = "tabs",
    # Leaflet map
    tabPanel("Map",leafletOutput(outputId = "map"), plotlyOutput(outputId = "stackedbar")),
    # Heatmap
    tabPanel("Heatmaps",  plotlyOutput(outputId = "heatmap"), plotlyOutput(outputId = "heatmap_stage")))
    
   
  )
)

# Define server logic for shiny app
server <- function(input, output, session) {
  # Render leaflet map
  output$map <- renderLeaflet({
    
    filtered_data <- coord_df
    if (!is.null(input$country) && !("" %in% input$country)) {
      filtered_data <- filtered_data[filtered_data$country %in% input$country, ]
    }
    if (!is.null(input$Industry)) {
      filtered_data <- filtered_data[filtered_data$Industry %in% input$Industry, ]
    }
    
    # Map country names to continents
    filtered_data$continent <- countrycode(sourcevar = as.character(filtered_data$country),
                                           origin = "country.name",
                                           destination = "continent"
    )
    
    # Define a color palette based on continents
    continent_palette <- colorFactor(
      palette = c("red", "green", "orange", "blue", "black"),
      domain = c("Americas", "Asia", "Africa", "Europe", "Oceania")
    )
    
    leaflet(data = filtered_data) %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addProviderTiles("Stamen.TonerLite") %>%
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        radius = input$radius,
        color = ~continent_palette(continent),
        popup = ~paste0(country, ', ', Company, ', ', Industry, " (",Laid_Off_Count,")"),
        label = ~paste0(country, ', ', Company, ', ', Industry, " (",Laid_Off_Count,")"),
      ) %>%
      addLegend(
        "bottomright",
        pal = continent_palette,
        values = filtered_data$continent,
        title = "Continent",
        opacity = 1
      ) %>%
      addHeatmap(
        lng = ~lng,
        lat = ~lat,
        intensity = ~get(input$intensity_col),
        radius = input$radius,
        minOpacity = 0.5
      )
  })
  
  output$stackedbar <- renderPlotly({
    
    filtered_df <- layoffs2
    if (!is.null(input$country) && !("" %in% input$country)) {
      filtered_df <- filtered_df[filtered_df$Country %in% input$country, ]
    }
    if (!is.null(input$Industry)) {
      filtered_df <- filtered_df[filtered_df$Industry %in% input$Industry, ]
    }
    
    ggplotly(ggplot(filtered_df, aes(x = Industry, y = !!rlang::sym(input$intensity_col), fill = Country), y) + 
    geom_col() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_fill_manual(values=as.vector(polychrome(13))))})
 

   output$heatmap <- renderPlotly({
    
    breaks <- c(0,.05,.1,.25,.5,1)
    
    fig <- ggplot(country_ind2, aes(x = Country, 
                                      y= Industry,
                                      fill = perc, 
                                      text = paste("Perc: ", round(perc*100), "% \n", "Layoffs: ",laid_off, "\n","Largest: ", Company, " (",laid_off_comp,")",sep = ""))) +
               geom_tile() +
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
               scale_fill_gradient2(low = "yellow", mid = "forestgreen", high = "purple", 
                                    midpoint = .15, breaks = breaks) +
               labs(fill = "Laid Off") 
   
  #Create a source for plotly plot for which event_data should be used  
  ggplotly(fig, tooltip = c("text"), source = "heatmapSource") %>% event_register("plotly_click") 
   
  })  
  
   
  output$heatmap_stage <- renderPlotly({
     
    #Get event_data info from other heatmap
     d <- event_data("plotly_click", source = "heatmapSource")
     
     if(!is.null(d)) {
     
     #Get unique list of countries in heatmap DF  
     mycountries <- unique(country_ind2$Country)
     
     #Pull clicked country from event_data
     clicked_country <- mycountries[d$x]
      
     #Filter DF to clicked country
     layoffs_by_stage <- layoffs2 %>% 
     filter(Country == clicked_country) %>%
       group_by(Stage) %>%
       mutate(stage_layoffs = sum(Laid_Off_Count)) %>%
       group_by(Industry) %>%
        mutate(ind_pct = stage_layoffs/sum(stage_layoffs)) %>%
        summarise(industry = unique(Industry), stage = unique(Stage), ind_pct = unique(ind_pct))
   
     
     breaks <- c(0,.05,.1,.25,.5,1)
     
     ggplotly(ggplot(layoffs_by_stage, aes(x = stage, y= industry, fill = ind_pct, text = paste("% of ", industry, " Layoffs: ",100*round(ind_pct, 2), "%"))) +
                geom_tile() +
                ggtitle(paste("Layoffs: ", clicked_country)) +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
                scale_fill_gradient2(low = "yellow", mid = "forestgreen", high = "purple", 
                                     midpoint = .15, breaks = breaks) +
                labs(fill = "% Laid Off"),
              tooltip = c("text")) 
     

     
     }
   
           })


    
}

shinyApp(ui = ui, server = server)

         