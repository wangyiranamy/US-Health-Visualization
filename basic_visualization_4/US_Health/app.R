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
library(leaflet)

summary_measure_state <- read.csv("/Users/yiranwang/US-Health-Visualization/basic_visualization_4/US_Health/summary_measure_state.csv")
preventive_df1 <- read.csv("/Users/yiranwang/US-Health-Visualization/data/Clean data/preventive_df1.csv")

# Define UI for application that draws a histogram
ui <- navbarPage("US Health Status", id="ush",
      tabPanel("US Health Summary",
               titlePanel("US Health Status Summary"),
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
               tags$hr(),
               fixedRow(
                 column(8, plotlyOutput("Plot1", height = "600px")),
                 column(4, plotlyOutput("Plot2", height = "300px"))),
     tabPanel("PreventiveDisease",
              titlePanel("US Health preventive diseases Status"),
              verbatimTextOutput("click"),
              sidebarLayout(
                sidebarPanel(
                  helpText("Create demographic maps with 
                           information from the US Preventive Diseases Summary from 1994-2003."),
                  
                  selectInput("var_xinxin", 
                              label = "Choose a variable to display",
                              choices = c("Haemophilus Influenzae B", "Hepatitis A",
                                          "Hepatitis B", "Measles", "Pertussis","Congenital Rubella Syndrome","Syphilis"),
                              selected = "Haemophilus Influenzae B")
                  ),
                
                mainPanel(
                plotlyOutput("plot"))
                ))))
                
      


# Define server logic required to draw a histogram
server <- function(input, output, var, session) {
  output$Plot1<- renderPlotly({
    var_yiran <- switch(input$var, 
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
        z = ~var_yiran, text = ~hover, locations = ~CHSI_State_Abbr,
        color = ~var_yiran, colors = 'Oranges'
      ) %>%
      colorbar(title = input$var) %>%
      layout(
        title = 'US Health Summary by State',
        geo = g
      )
  })
 
  output$Plot2 <- renderPlotly({
    var_yiran <- switch(input$var,
                   "Average Life Expectancy" = summary_measure_state$ALE,
                   "Unhealthy Days" = summary_measure_state$Unhealthy_Days,
                   "Healthy Status" = summary_measure_state$Health_Status,
                   "All Death" = summary_measure_state$All_Death)
    p <- ggplot(summary_measure_state, aes(CHSI_State_Abbr,var_yiran))+
      geom_bar(stat = "identity", fill = "orange")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
     ggplotly(p)
  })
  output$plot <- renderPlotly({
    datainput <- switch(input$var_xinxin, 
                        "Haemophilus Influenzae B" = preventive_df1$sumFluB_Rpt,
                        "Hepatitis A" = preventive_df1$sumHepA_Rpt,
                        "Hepatitis B" = preventive_df1$sumHepB_Rpt,
                        "Measles" = preventive_df1$sumMeas_Rpt,
                        "Pertussis"=preventive_df1$sumPert_Rpt,
                        "Congenital Rubella Syndrome"=preventive_df1$sumCRS_Rpt,
                        "Syphilis"=preventive_df1$sumSyphilis_Rpt)
    l <- list(color = toRGB("white"), width = 2)
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    plot_geo(preventive_df1, locationmode = 'USA-states') %>%
      add_trace(
        z = ~datainput, text = ~datainput, locations = ~CHSI_State_Abbr,
        color = ~datainput, colors = 'Oranges'
      ) %>%
      colorbar(title = input$var) %>%
      layout(
        title = '1994-2003 US Preventive Diseases Summary by State<br>(Hover for breakdown)',
        geo = g
      )
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

