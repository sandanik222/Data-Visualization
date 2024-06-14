#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(usdata)
library(plotly)
library(ggpubr)


#named vectors for explanatory and response variables
explanatory_choices <-
  c(
    "Total Cost" = "out_of_state_total",
    "% STEM" = "stem_percent",
    "% Women" = "pct_women",
    "% Minority" = "pct_minority"
  )

response_choices <-
  c(
    "Early-Career Earnings" = "early_career_pay",
    "Mid-Career Earnings" = "mid_career_pay"
  )



#load the data
df <- readRDS("df.rds")

#convert deomgraphic data to percentages
df$pct_minority <- df$`Total Minority`/df$total_enrollment
df$pct_women <- df$Women/df$total_enrollment

#add variable for region
df$region <- setNames(state.region, state.name)[df$state]

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Payoff of College"),
  
  # Sidebar with inputs
  sidebarLayout(
    sidebarPanel(
      
      #response variable
      selectInput("response",
                  "Response Variable:",
                  choices = response_choices,
                  selected = "Early-Career Earnings"),
      
      #explanatory variable
      selectInput("explanatory",
                  "Explanatory Variable:",
                  choices = explanatory_choices,
                  selected = "Total Cost"),
      
      #slider for student body size
      sliderInput("size",
                  "Size of Student Body:",
                  min=100, 
                  max=70000,
                  value = c(100, 70000)
      ),
      
      #public or private selection
      checkboxGroupInput("type",
                         "Type:",
                         choices=c("Public", "Private"),
                         selected=c("Public", "Private")),
      
      #region selection
      checkboxGroupInput("region",
                         "Region:",
                         choices=c("Northeast", "South", "North Central", "West"),
                         selected=c("Northeast", "South", "North Central", "West"))
    ),
    
    # Show scatterplot
    mainPanel(
      plotlyOutput("college_plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #using plotly and ggplotly to render plot
  output$college_plot <- renderPlotly({
    filtered_df <- 
      df %>% 
      
      #filter by inputs
      filter(
        type %in% input$type,
        region %in% input$region,
        between(total_enrollment,input$size[1], input$size[2])
      )
    
    g <-
      ggplotly(
        
        #make ggplot
        ggplot(filtered_df, 
    
               aes_string(
                 text = "name",
                 x = input$explanatory,
                 y = input$response,
                 size = "total_enrollment", 
                 color = "type", 
                 state_label = "state"
               )
        )+
          
          #make scatterplot
          geom_point(show.legend = FALSE)+
          
          #fix axes according to input values
          ylim(if(input$response=="early_career_pay"){c(25000, 100000)}
               else{c(50000, 175000)})+
          
          xlim(if(input$explanatory=="out_of_state_total"){c(0, 80000)}
               else if(input$explanatory=="stem_percent"){c(0, 100)}
               else if(input$explanatory=="pct_women"){c(0, 1)}
               else if(input$explanatory=="pct_minority"){c(0, 1)}
                 )+
          
          #label axes
          xlab(names(explanatory_choices[explanatory_choices == input$explanatory]))+
          ylab(names(response_choices[response_choices == input$response]))+
          
          #put y-scale in USD
          scale_y_continuous(labels=scales::dollar_format()) +
          
          #choose x-scale units according to explanatory input
          scale_x_continuous(labels= if(input$explanatory=="out_of_state_total"){scales::dollar_format()}
                             else if(input$explanatory=="stem_percent"){scales::percent_format(scale = 1)}
                             else if(input$explanatory=="pct_women"){scales::percent_format(scale = 100)}
                             else if(input$explanatory=="pct_minority"){scales::percent_format(scale = 100)}
                             )+
          
          scale_color_manual(values=c(
            "Private" = "red",
            "Public" = "blue"
          ))
      
      ,
      tooltip = c("text", "state_label", "total_enrollment")
      
      
      
      
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
