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

summary_measure_state <- read.csv("/Users/xinxinhuang/Desktop/DataVisualization/US-Health-Visualization/basic_visualization_4/US_Health/summary_measure_state.csv")
preventive_df1 <- read.csv("/Users/xinxinhuang/Desktop/DataVisualization/US-Health-Visualization/data/Clean data/preventive_df1.csv")
measurebirth<-read.csv("/Users/xinxinhuang/Desktop/DataVisualization/US-Health-Visualization/data/Clean data/measureBirth_clean.csv")

# Define UI for application that draws a histogram
ui <- navbarPage("US Health Status", id="ush",
      tabPanel("US Health Summary",
               titlePanel("US Health Status Summary"),
               tags$ol(
                 tags$li("Create demographic maps with
                         information from the US Health Status Summary.")
               ),
               fixedRow(
                 column(5, selectInput(inputId = "var_yiran",
                                       label = "Choose a variable to display",
                                       choices = c("Average Life Expectancy", "Unhealthy Days",
                                                   "Health Status", "All Death"),
                                       selected = "Average Life Expectancy"))
               ),
               tags$hr(),
               fixedRow(
                 column(8, plotlyOutput("Plot1")),
                 column(4, plotlyOutput("Plot2", height = "300px"))),
               fixedRow(
                 column(8, verbatimTextOutput("info"))
               )
               ),
              
     # tabPanel("PreventiveDisease",
     #          titlePanel("US Health preventive diseases Status"),
     #          verbatimTextOutput("click"),
     #          sidebarLayout(
     #            sidebarPanel(
     #              helpText("Create demographic maps with
     #                       information from the US Preventive Diseases Summary from 1994-2003."),
     # 
     #              selectInput("var_xinxin",
     #                          label = "Choose a variable to display",
     #                          choices = c("Haemophilus Influenzae B", "Hepatitis A",
     #                                      "Hepatitis B", "Measles", "Pertussis","Congenital Rubella Syndrome","Syphilis"),
     #                          selected = "Haemophilus Influenzae B")
     #              ),
     # 
     #            mainPanel(
     #            plotlyOutput("plot02"))
     #            )),
     tabPanel("Measure of Birth",
              titlePanel("US Birth Measure"),
              verbatimTextOutput("click"),
              tags$ol(
                tags$li("Get a geographical summary of Total Birth of US in 1994-2003.")
                ),
              
              fixedRow(column(12, plotlyOutput("plot03"))
              ),
              fixedRow(column(7, helpText("Get to know the relationship between Unhealthy Birth with situations of Mothers of US in 1994-2003"))),
              
              fixedRow(
                column(6,selectInput("var_xinxin03",label = "Choose the X variable to show their relationship",
                            choices = c("Births to women under 18", "Births to women over 40","Births to unmarried women", 
                                        "No care in first trimester"))),column(6,selectInput("var_xinxin02",
                                                                                             label = "Choose the Y variable to show their relationship",
                                                                                             choices = c("Low birth weight.(<2500 g)", "Very low birth weight.(<1500 g)",
                                                                                                         "Premature births","Infant mortality")))),
              fixedRow(column(12, plotlyOutput("plot04"))
              )
                
                
                
              
              
              # sidebarLayout(sidebarPanel(
              #   selectInput("var_xinxin02",
              #               label = "Choose the first variable to show their relationship",
              #               choices = c("Low birth weight.(<2500 g)", "Very low birth weight.(<1500 g)",
              #                           "Premature births","Infant mortality")),
              #   helpText("Get to know the relationship between Unhealthy Birth with situations of Mothers of US in 1994-2003"),
              # 
              #   selectInput("var_xinxin03",
              #               label = "Choose the second variable to show their relationship",
              #               choices = c("Births to women under 18", "Births to women over 40",
              #                           "Births to unmarried women", "No care in first trimester")
              #               )),
                


              )
     
     
     
)
                
      


# Define server logic required to draw a histogram
server <- function(input, output, var_yiran, var_xinxin, session) {
  output$Plot1<- renderPlotly({
    data <- switch(input$var_yiran,
                   "Average Life Expectancy" = summary_measure_state$ALE,
                   "Unhealthy Days" = summary_measure_state$Unhealthy_Days,
                   "Health Status" = summary_measure_state$Health_Status,
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
  
  ### Yiran Bar
 
  output$info <- renderText({

    as.character(summary_measure_state[event_data("plotly_hover")$pointNumber,"State_Name"])
  })
  output$Plot2 <- renderPlotly({
    data <- switch(input$var_yiran,
                   "Average Life Expectancy" = summary_measure_state$ALE,
                   "Unhealthy Days" = summary_measure_state$Unhealthy_Days,
                   "Health Status" = summary_measure_state$Health_Status,
                   "All Death" = summary_measure_state$All_Death)
    p <- ggplot(summary_measure_state, aes(reorder(CHSI_State_Abbr,data),data))+
      geom_bar(stat = "identity", fill = "orange")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
            axis.title = element_text(size=8),
            plot.title = element_text(size = 10))+
      xlab("States")+
      ylab("Average Value")+
      ggtitle(paste("Histogram Visualization for",input$var_yiran))
     ggplotly(p)
  })
  
  #### Xinxinplot01
  # output$plot02 <- renderPlotly({
  #   datainput <- switch(input$var_xinxin, 
  #                       "Haemophilus Influenzae B" = preventive_df1$sumFluB_Rpt,
  #                       "Hepatitis A" = preventive_df1$sumHepA_Rpt,
  #                       "Hepatitis B" = preventive_df1$sumHepB_Rpt,
  #                       "Measles" = preventive_df1$sumMeas_Rpt,
  #                       "Pertussis"=preventive_df1$sumPert_Rpt,
  #                       "Congenital Rubella Syndrome"=preventive_df1$sumCRS_Rpt,
  #                       "Syphilis"=preventive_df1$sumSyphilis_Rpt)
  #   l <- list(color = toRGB("white"), width = 2)
  #   g <- list(
  #     scope = 'usa',
  #     projection = list(type = 'albers usa'),
  #     showlakes = TRUE,
  #     lakecolor = toRGB('white')
  #   )
  #   plot_geo(preventive_df1, locationmode = 'USA-states') %>%
  #     add_trace(
  #       z = ~datainput, text = ~datainput, locations = ~CHSI_State_Abbr,
  #       color = ~datainput, colors = 'Oranges'
  #     ) %>%
  #     colorbar(title = input$var) %>%
  #     layout(
  #       title = '1994-2003 US Preventive Diseases Summary by State',
  #       geo = g
  #     )
  #   
  # })
  
  ### Xinxinplot02
  output$plot03 <- renderPlotly({
     l<- list(color = toRGB("white"), width = 2)
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    plot_geo(measurebirth, locationmode = 'USA-states') %>%
      add_trace(
        z = ~measurebirth$sum_birth, text = ~measurebirth$sum_birth, locations = ~CHSI_State_Abbr,
        color = ~measurebirth$sum_birth, colors = 'Oranges'
      ) %>%
      colorbar(title = 'Total Birth') %>%
      layout(
        title = '1994-2003 US Total Birth Summary by State',
        geo = g
      )
  })
  
  ### Scatterplot
  output$plot04 <- renderPlotly({
    datainput<-switch(input$var_xinxin02, 
                      "Low birth weight.(<2500 g)"= measurebirth$new_LBW,
                      "Very low birth weight.(<1500 g)"= measurebirth$new_VLBW,
                      "Premature births"= measurebirth$new_Premature,
                      "Infant mortality"= measurebirth$new_Infant_Mortality
                      )
    datainput2 <- switch(input$var_xinxin03,
                         "Births to women under 18" = measurebirth$new_Under_18,
                         "Births to women over 40" = measurebirth$new_Over_40,
                         "Births to unmarried women" = measurebirth$new_Unmarried,
                         "No care in first trimester" = measurebirth$new_Late_Care
                         )
    
    p <- plot_ly(data = measurebirth, x = ~datainput2, y = ~datainput,
                 marker = list(size = 10,
                               color = 'rgba(255, 182, 193, .9)',
                               line = list(color = 'rgba(152, 0, 0, .8)',
                                           width = 2))) %>%
      layout(title = 'Mother Situation VS. Infant Mortality',
             yaxis = list(zeroline = FALSE, title=input$var_xinxin02),
             xaxis = list(zeroline = FALSE,title=input$var_xinxin03))
    
    # Create a shareable link to your chart
    # Set up API credentials: https://plot.ly/r/getting-started
    #chart_link = api_create(p, filename="scatter-styled")
    #chart_link
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

