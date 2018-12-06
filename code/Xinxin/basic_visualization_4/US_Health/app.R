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
library(maps)
library(dplyr)
library(ggplot2)
library(tibble)
library(tidyverse)

summary_measure_state <- read.csv("/Users/yiranwang/US-Health-Visualization/basic_visualization_4/US_Health/summary_measure_state.csv")
preventive_df1 <- read.csv("/Users/yiranwang/US-Health-Visualization/data/Clean data/preventive_df1.csv")
df2 = read.csv('/Users/yiranwang/US-Health-Visualization/data/risk_factors_and_access_to_care.csv')

no_ex = df2[df2$No_Exercise>0,]
risk = df2[df2$Obesity>0,]
few_fruit = df2[df2$Few_Fruit_Veg>0,]
High_Blood_Pres = df2[df2$High_Blood_Pres>0,]
diabete = df2[df2$Diabetes>0,]

few_fruit = aggregate(few_fruit[, 10], list(few_fruit$CHSI_State_Abbr), median)
High_Blood_Pres = aggregate(High_Blood_Pres[, 16], list(High_Blood_Pres$CHSI_State_Abbr), median)
diabete = aggregate(diabete[, 22], list(diabete$CHSI_State_Abbr), median)
no_ex = aggregate(no_ex[, 7], list(no_ex$CHSI_State_Abbr), median)

risk = aggregate(risk[, 13], list(risk$CHSI_State_Abbr), median)
risk$no_ex = no_ex$x
risk$diabete = diabete$x
risk$few_fruit = few_fruit$x
risk$High_Blood_Pres = High_Blood_Pres$x

risk$Abbr = levels(risk$Group.1)[as.numeric(risk$Group.1)]

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
                )),
     tabPanel("Risk Factors",
              titlePanel("US Health Risk Factors"),
              verbatimTextOutput("click_risk"),
              sidebarLayout(
                sidebarPanel(
                  helpText("Create demographic maps with 
                           information from the US Preventive Diseases Summary from 1994-2003."),
                  
                  selectInput("var_risk", 
                              label = "Choose a variable to display",
                              choices = c("Obesity",
                                          "No exercise",
                                          "Few fruits/vegetables",
                                          "High blood pressure",
                                          "Diabetes"),
                              selected = "Obesity")
                ),
                
                mainPanel(
                  fixedRow(
                    column(8, plotlyOutput("plot_risk")),
                    fixedRow(
                      column(8, plotlyOutput("plot_risk_cleve", height = "800px")))
                  )
                )
              ))
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
  
  #### Xinxin
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
  
  ## Risk Factor and access to health care
  output$plot_risk <- renderPlotly({
    datainput <- switch(input$var_risk, 
                        "Obesity" = risk$x,
                        "No exercise" = risk$no_ex,
                        "Few fruits/vegetables" = risk$few_fruit,
                        "High blood pressure" = risk$High_Blood_Pres,
                        "Diabetes" = risk$diabete)
    l <- list(color = toRGB("white"), width = 2)
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    plot_geo(risk, locationmode = 'USA-states') %>%
      add_trace(
        z = ~datainput, text = ~datainput, locations = ~Abbr,
        color = ~datainput, colors = 'Oranges'
      ) %>%
      colorbar(title = input$var) %>%
      layout(
        title = '1994-2003 US Risk Factors Summary by State<br>(Hover for breakdown)',
        geo = g
      )
    
  })
  output$plot_risk_cleve <- renderPlotly({
    datainput <- switch(input$var_risk, 
                        "Obesity" = risk$x,
                        "No exercise" = risk$no_ex,
                        "Few fruits/vegetables" = risk$few_fruit,
                        "High blood pressure" = risk$High_Blood_Pres,
                        "Diabetes" = risk$diabete)
    theme_dotplot <- theme_bw(18) +
      theme(axis.text.y = element_text(size = rel(.75)),
            axis.ticks.y = element_blank(),
            axis.title.x = element_text(size = rel(.75)),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(size = 0.5),
            panel.grid.minor.x = element_blank())
    p = ggplot() + geom_point(aes(x = datainput, y = fct_reorder(risk$Abbr, datainput)), color = "green") +
      ylab(input$var_risk) + xlab("Index") + theme_dotplot
    
    print(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

