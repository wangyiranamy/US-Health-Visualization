#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
summary_measure_state <- read.csv("summary_measure_state.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("US Health Status"),
  plotlyOutput("plot"),
  verbatimTextOutput("click"),
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with 
               information from the US Health Status Summary."),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Average Life Expectancy", "Unhealthy Days",
                              "Health Status", "All Death"),
                  selected = "Average Life Expectancy")
      
      # sliderInput("range", 
      #             label = "Range of interest:",
      #             min = 0, max = 100, value = c(0, 100))
    ),
    
    mainPanel(plotOutput("map"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, var, session) {
  
  output$plot <- renderPlotly({
    data <- switch(input$var, 
                   "Average Life Expectancy" = summary_measure_state$ALE,
                   "Unhealthy Days" = summary_measure_state$Unhealthy_Days,
                   "Healthy Status" = summary_measure_state$Healthy_Status,
                   "All Death" = summary_measure_state$All_Death)
    l <- list(color = toRGB("white"), width = 2)
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    plot_geo(summary_measure_state, locationmode = 'USA-states') %>%
      add_trace(
        z = ~data, text = ~data, locations = ~CHSI_State_Abbr,
        color = ~data, colors = 'Purples'
      ) %>%
      colorbar(title = input$var) %>%
      layout(
        title = '2011 US Average Life Expectation by State<br>(Hover for breakdown)',
        geo = g
      )
    # plot_ly(z = state.area, text = state.name, locations = state.abb,
    #         type = 'choropleth', locationmode = 'USA-states') %>%
    #   layout(geo = g)
  })
  
  # output$click <- renderPrint({
  #   d <- event_data("plotly_click")
  #   if (is.null(d)) "Click on a state to view event data" else d
  # })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

