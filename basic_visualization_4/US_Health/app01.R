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
summary_measure_state <- read.csv("/Users/yiranwang/US-Health-Visualization/basic_visualization_4/US_Health/summary_measure_state.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("US Health Status"),
  # plotlyOutput("plot"),
  # plotlyOutput("plot"),
  # fluidRow(
  #   column(10,
  #          verbatimTextOutput("text"))
  # ),
  tags$ol(
    tags$li("Create demographic maps with 
               information from the US Health Status Summary.",
            tags$code("plotly_selected"))
  ),
  fixedRow(
    column(5, selectInput(inputId = "var", 
                          label = "Choose a variable to display", 
                          choices = c("Average Life Expectancy", "Unhealthy Days",
                                      "Health Status", "All Death"), 
                          selected = "Average Life Expectancy"))
),
  # sidebarLayout(
  #   sidebarPanel(
  #     helpText("Create demographic maps with 
  #              information from the US Health Status Summary."),
  #     
  #     selectInput("var", 
  #                 label = "Choose a variable to display",
  #                 choices = c("Average Life Expectancy", "Unhealthy Days",
  #                             "Health Status", "All Death"),
  #                 selected = "Average Life Expectancy")
  #     
  #     # sliderInput("range", 
  #     #             label = "Range of interest:",
  #     #             min = 0, max = 100, value = c(0, 100))
  #   ),
    tags$hr(),
    fixedRow(
      column(12, plotlyOutput("Plot1", height = "600px"))),
    fixedRow(
        column(12, plotlyOutput("Plot2", height = "300px")))
  )


# Define server logic required to draw a histogram
server <- function(input, output, var, session) {
  output$Plot1 <- renderPlotly({
    data <- switch(input$var, 
                   "Average Life Expectancy" = summary_measure_state$ALE,
                   "Unhealthy Days" = summary_measure_state$Unhealthy_Days,
                   "Healthy Status" = summary_measure_state$Health_Status,
                   "All Death" = summary_measure_state$All_Death)
    summary_measure_state$hover <- with(summary_measure_state, paste(State_Name, '<br>', "ALE", ALE, "Unhealthy_Days", Unhealthy_Days, "<br>",
                               "Healthy_Status", Health_Status))
    l <- list(color = toRGB("white"), width = 2)
    g <- list(
      scope = 'usa',
      showcountries = TRUE,
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    plot_geo(summary_measure_state, locationmode = 'USA-states') %>%
      add_trace(
        z = ~data, text = ~hover, locations = ~CHSI_State_Abbr,
        color = ~data, colors = 'Oranges'
      ) %>%
      colorbar(title = input$var) %>%
      layout(
        title = 'US Health Summary by State',
        geo = g
      )
  })
 
  output$Plot2 <- renderPlotly({
    var <- switch(input$var, 
                   "Average Life Expectancy" = summary_measure_state$ALE,
                   "Unhealthy Days" = summary_measure_state$Unhealthy_Days,
                   "Healthy Status" = summary_measure_state$Health_Status,
                   "All Death" = summary_measure_state$All_Death)
    p <- ggplot(summary_measure_state, aes(CHSI_State_Abbr,var,fill = "orange"))+
      geom_bar(stat = "identity")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

