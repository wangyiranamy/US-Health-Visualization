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
options(scipen=5)

summary_measure_state <-
  read.csv(
    "basic_visualization_4/US_Health/summary_measure_state.csv"
  )
preventive_df1 <-
  read.csv(
    "data/Clean data/preventive_df1.csv"
  )
measurebirth <-
  read.csv(
    "data/Clean data/measureBirth_clean.csv"
  )

df2 = read.csv(
  'data/risk_factors_and_access_to_care.csv'
)

# yiming data
death_causes = read.csv(
  'data/rates_causes_of_death_bystate.csv'
)
death_mosaic = read.csv(
  'data/disease_mosaic.csv'
)

no_ex = df2[df2$No_Exercise > 0, ]
risk = df2[df2$Obesity > 0, ]
few_fruit = df2[df2$Few_Fruit_Veg > 0, ]
High_Blood_Pres = df2[df2$High_Blood_Pres > 0, ]
diabete = df2[df2$Diabetes > 0, ]

few_fruit = aggregate(few_fruit[, 10], list(few_fruit$CHSI_State_Abbr), median)
High_Blood_Pres = aggregate(High_Blood_Pres[, 16],
                            list(High_Blood_Pres$CHSI_State_Abbr),
                            median)
diabete = aggregate(diabete[, 22], list(diabete$CHSI_State_Abbr), median)
no_ex = aggregate(no_ex[, 7], list(no_ex$CHSI_State_Abbr), median)

risk = aggregate(risk[, 13], list(risk$CHSI_State_Abbr), median)
risk$no_ex = no_ex$x
risk$diabete = diabete$x
risk$few_fruit = few_fruit$x
risk$High_Blood_Pres = High_Blood_Pres$x

risk$Abbr = levels(risk$Group.1)[as.numeric(risk$Group.1)]

# Define UI for application that draws a histogram
ui <- navbarPage(
  "US Health Status",
  id = "ush",
  tabPanel(
    "US Health Summary",
    titlePanel("US Health Status Summary"),
    # tags$ol(
    #   tags$li(
    #     "Create demographic maps with
    #     information from the US Health Status Summary."
    #   )
    #   ),
    fixedRow(column(
      5,
      selectInput(
        inputId = "var_yiran",
        label = "Choose a variable to display",
        choices = c(
          "Average Life Expectancy",
          "Unhealthy Days",
          "Health Status",
          "All Death"
        ),
        selected = "Average Life Expectancy"
      )
    )),
    tags$hr(),
    fixedRow(column(8, plotlyOutput("Plot1")),
             column(4, plotlyOutput("Plot2", height = "300px"))),
    fixedRow(column(8, verbatimTextOutput("info")))
    ),
  tabPanel(
    "Measure of Birth",
    titlePanel("US Birth Measure"),
    verbatimTextOutput("click"),
    # tags$ol(
    #   tags$li("Get a geographical summary of Total Birth of US.")
    # ),
    # 
    fixedRow(column(12, plotlyOutput("plot03"))),
    fixedRow(column(
      12,
      helpText(
        "Relationship between Unhealthy Birth with situations of Mothers."
      )
    )),
    
    fixedRow(column(
      6,
      selectInput(
        "var_xinxin03",
        label = "Choose the X variable.",
        choices = c(
          "Births to women under 18",
          "Births to women over 40",
          "Births to unmarried women",
          "No care in first trimester"
        )
      )
    ), column(
      6,
      selectInput(
        "var_xinxin02",
        label = "Choose the Y variable.",
        choices = c(
          "Low birth weight.(<2500 g)",
          "Very low birth weight.(<1500 g)",
          "Premature births",
          "Infant mortality"
        )
      )
    )),
    fixedRow(column(12, plotlyOutput("plot04")))
  ),
  tabPanel(
    "Risk Factors",
    titlePanel("US Health Risk Factors"),
    verbatimTextOutput("click_risk"),
    fixedRow(column(
      7,
      helpText(
        "The General Health Status of US"
      )
    )),
    fixedRow(column(
      10,
      selectInput(
        "var_risk",
        label = "Choose a variable to display",
        choices = c(
          "Obesity",
          "No exercise",
          "Few fruits/vegetables",
          "High blood pressure",
          "Diabetes"
        ),
        selected = "Obesity"
      )),
      column(10, plotlyOutput("plot_risk"))),

      fixedRow(column(
        8, plotlyOutput("plot_risk_cleve", height = "800px", width = "1600px")
      )
    )
  ),
  # yiming tab
  tabPanel(
    "Death Causes",
    titlePanel("US Unnatural Death Causes"),
    fixedRow(column(
      7,
      helpText(
        "The summary on leading unnatural causes of deaths of US"
      )
    )),
    fixedRow(column(
      10,
      selectInput(
        "var_yiming",
        label = "Choose a variable to display",
        choices = c(
          "HIV",
          "Pregnancy & Birth Issues",
          "Injury",
          "Homicide",
          "Suicide", 
          "Heart Disease",
          "Breast Cancer",
          "Colon Cancer",
          "Lung Cancer"
        ),
        selected = "HIV"
      )),
      column(10, plotlyOutput("plot_death_causes"))),
    
    fixedRow(column(
      8, plotlyOutput("plot_summary_death_causes")
    )
    )
  )
  
  
  )




# Define server logic required to draw a histogram
server <- function(input,
                   output,
                   var_yiran,
                   var_xinxin,
                   var_yiming, # added by yiming
                   session) {
  output$Plot1 <- renderPlotly({
    data <- switch(
      input$var_yiran,
      "Average Life Expectancy" = summary_measure_state$ALE,
      "Unhealthy Days" = summary_measure_state$Unhealthy_Days,
      "Health Status" = summary_measure_state$Health_Status,
      "All Death" = summary_measure_state$All_Death
    )
    summary_measure_state$hover <-
      with(
        summary_measure_state,
        paste(
          State_Name,
          '<br>',
          "ALE",
          ALE,
          "Unhealthy_Days",
          Unhealthy_Days,
          "<br>",
          "Healthy_Status",
          Health_Status
        )
      )
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
        z = ~ data,
        text = ~ hover,
        locations = ~ CHSI_State_Abbr,
        color = ~ data,
        colors = 'Oranges'
      ) %>%
      colorbar(title = input$var) %>%
      layout(title = 'US Health Summary by State',
             geo = g)
  })
  
  ### Yiran Bar
  
  output$info <- renderPrint({
    data <- switch(
      input$var_yiran,
      "Average Life Expectancy" = summary_measure_state$ALE,
      "Unhealthy Days" = summary_measure_state$Unhealthy_Days,
      "Health Status" = summary_measure_state$Health_Status,
      "All Death" = summary_measure_state$All_Death
    )
    idx = which(data == event_data("plotly_hover")$z)
    as.character(summary_measure_state$State_Name[idx])
    # as.character(summary_measure_state[event_data("plotly_hover")$pointNumber,"State_Name"])
  })
  output$Plot2 <- renderPlotly({
    data <- switch(
      input$var_yiran,
      "Average Life Expectancy" = summary_measure_state$ALE,
      "Unhealthy Days" = summary_measure_state$Unhealthy_Days,
      "Health Status" = summary_measure_state$Health_Status,
      "All Death" = summary_measure_state$All_Death
    )
    idx = which(data == event_data("plotly_hover")$z)
    fill_color <-
      c(
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange",
        "orange"
      )
    
    idx = which(data == event_data("plotly_hover")$z)
    fill_color[idx] <- "brown"
    summary_measure_state$fillcolor <- fill_color
    summary_measure_state <- summary_measure_state[order(data), ]
    # fill_color[event_data("plotly_hover")$pointNumber] <- "yellow"
    # p <- plot_ly(summary_measure_state, x = ~reorder(CHSI_State_Abbr,data), y = ~data, type = 'bar',
    #              marker = list(color = summary_measure_state$fillcolor)) %>%
    #   layout(title = paste("Histogram Visualization for",input$var_yiran),
    #          xaxis = "States",
    #          yaxis = "Average Value")
    # p
    #
    p <-
      ggplot(summary_measure_state, aes(reorder(CHSI_State_Abbr, data), data)) +
      geom_bar(stat = "identity", fill = summary_measure_state$fillcolor) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          size = 6
        ),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 18)
      ) +
      xlab("States") +
      ylab("Average Value") +
      ggtitle(paste("Histogram Visualization for", input$var_yiran))
    ggplotly(p)
  })
  
  #### Xinxinplot01
  output$plot01 <- renderPlotly({
    datainput <- switch(
      input$var_xinxin,
      "Haemophilus Influenzae B" = preventive_df1$sumFluB_Rpt,
      "Hepatitis A" = preventive_df1$sumHepA_Rpt,
      "Hepatitis B" = preventive_df1$sumHepB_Rpt,
      "Measles" = preventive_df1$sumMeas_Rpt,
      "Pertussis" = preventive_df1$sumPert_Rpt,
      "Congenital Rubella Syndrome" = preventive_df1$sumCRS_Rpt,
      "Syphilis" = preventive_df1$sumSyphilis_Rpt
    )
    l <- list(color = toRGB("white"), width = 2)
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    plot_geo(preventive_df1, locationmode = 'USA-states') %>%
      add_trace(
        z = ~ datainput,
        text = ~ datainput,
        locations = ~ CHSI_State_Abbr,
        color = ~ datainput,
        colors = 'Oranges'
      ) %>%
      colorbar(title = input$var) %>%
      layout(title = 'US Preventive Diseases Summary by State',
             geo = g)
    
  })
  
  ### Xinxinplot02
  output$plot03 <- renderPlotly({
    l <- list(color = toRGB("white"), width = 2)
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    plot_geo(measurebirth, locationmode = 'USA-states') %>%
      add_trace(
        z = ~ measurebirth$sum_birth,
        text = ~ measurebirth$sum_birth,
        locations = ~ CHSI_State_Abbr,
        color = ~ measurebirth$sum_birth,
        colors = 'Oranges'
      ) %>%
      colorbar(title = 'Total Birth') %>%
      layout(title = 'US Total Birth Summary 1994-2003',
             geo = g)
  })
  ### Scatterplot
  output$plot04 <- renderPlotly({
    datainput <- switch(
      input$var_xinxin02,
      "Low birth weight.(<2500 g)" = measurebirth$new_LBW,
      "Very low birth weight.(<1500 g)" = measurebirth$new_VLBW,
      "Premature births" = measurebirth$new_Premature,
      "Infant mortality" = measurebirth$new_Infant_Mortality
    )
    datainput2 <- switch(
      input$var_xinxin03,
      "Births to women under 18" = measurebirth$new_Under_18,
      "Births to women over 40" = measurebirth$new_Over_40,
      "Births to unmarried women" = measurebirth$new_Unmarried,
      "No care in first trimester" = measurebirth$new_Late_Care
    )
    
    p <-
      plot_ly(
        data = measurebirth,
        x = ~ datainput2,
        y = ~ datainput,
        marker = list(
          size = 10,
          color = 'rgba(255, 182, 193, .9)',
          line = list(color = 'rgba(152, 0, 0, .8)',
                      width = 2)
        )
      ) %>%
      layout(
        title = 'Mother Situation VS. Birth Situation',
        yaxis = list(zeroline = FALSE, title = input$var_xinxin02),
        xaxis = list(zeroline = FALSE, title = input$var_xinxin03)
      )
    
    # Create a shareable link to your chart
    # Set up API credentials: https://plot.ly/r/getting-started
    #chart_link = api_create(p, filename="scatter-styled")
    #chart_link
  })
  
  ### Scatterplot
  output$plot04 <- renderPlotly({
    datainput <- switch(
      input$var_xinxin02,
      "Low birth weight.(<2500 g)" = measurebirth$new_LBW,
      "Very low birth weight.(<1500 g)" = measurebirth$new_VLBW,
      "Premature births" = measurebirth$new_Premature,
      "Infant mortality" = measurebirth$new_Infant_Mortality
    )
    datainput2 <- switch(
      input$var_xinxin03,
      "Births to women under 18" = measurebirth$new_Under_18,
      "Births to women over 40" = measurebirth$new_Over_40,
      "Births to unmarried women" = measurebirth$new_Unmarried,
      "No care in first trimester" = measurebirth$new_Late_Care
    )
    
    p <-
      plot_ly(
        data = measurebirth,
        x = ~ datainput2,
        y = ~ datainput,
        marker = list(
          size = 10,
          color = 'rgba(255, 182, 193, .9)',
          line = list(color = 'rgba(152, 0, 0, .8)',
                      width = 2)
        )
      ) %>%
      layout(
        title = 'Mother Situation VS. Infant Mortality',
        yaxis = list(zeroline = FALSE, title = input$var_xinxin02),
        xaxis = list(zeroline = FALSE, title = input$var_xinxin03)
      )
    
    # Create a shareable link to your chart
    # Set up API credentials: https://plot.ly/r/getting-started
    #chart_link = api_create(p, filename="scatter-styled")
    #chart_link
  })
  
  ## Risk Factor and access to health care
  output$plot_risk <- renderPlotly({
    datainput <- switch(
      input$var_risk,
      "Obesity" = risk$x,
      "No exercise" = risk$no_ex,
      "Few fruits/vegetables" = risk$few_fruit,
      "High blood pressure" = risk$High_Blood_Pres,
      "Diabetes" = risk$diabete
    )
    l <- list(color = toRGB("white"), width = 2)
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    plot_geo(risk, locationmode = 'USA-states') %>%
      add_trace(
        z = ~ datainput,
        text = ~ datainput,
        locations = ~ Abbr,
        color = ~ datainput,
        colors = 'Oranges'
      ) %>%
      colorbar(title = input$var) %>%
      layout(title = 'US Risk Factors Summary by State<br>',
             geo = g)
    
  })
  output$plot_risk_cleve <- renderPlotly({
    datainput <- switch(
      input$var_risk,
      "Obesity" = risk$x,
      "No exercise" = risk$no_ex,
      "Few fruits/vegetables" = risk$few_fruit,
      "High blood pressure" = risk$High_Blood_Pres,
      "Diabetes" = risk$diabete
    )
    theme_dotplot <- theme_bw(18) +
      theme(
        axis.text.y = element_text(size = rel(.75)),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(.75)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5),
        panel.grid.minor.x = element_blank()
      )
    p = ggplot() + geom_point(aes(x = datainput, y = forcats::fct_reorder(risk$Abbr, datainput)), color = "orange") +
      ylab(input$var_risk) + xlab("Index") + theme_dotplot
    
    print(p)
  })
  # yiming geo plot
  output$plot_death_causes <- renderPlotly({
    datainput <- switch(
      input$var_yiming,
      "HIV" = death_causes$hiv,
      "Pregnancy & Birth Issues" = death_causes$complication_of_pregnancy_birth,
      "Injury" = death_causes$injury,
      "Homicide" = death_causes$homicide,
      "Suicide" = death_causes$suicide, 
      "Heart Disease" = death_causes$heart_disease,
      "Breast Cancer" = death_causes$breast_cancer,
      "Colon Cancer" = death_causes$colon_cancer,
      "Lung Cancer" = death_causes$lung_cancer
    )
    l <- list(color = toRGB("white"), width = 2)
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    plot_geo(death_causes, locationmode = 'USA-states') %>%
      add_trace(
        z = ~ datainput,
        text = ~ datainput,
        locations = ~ state,
        color = ~ datainput,
        colors = 'Oranges'
      ) %>%
      colorbar(title = input$var) %>%
      layout(title = 'US Leading Unnatural Causes of Deaths by State<br>',
             geo = g)
    
  })
  # yiming bar plot
  output$plot_summary_death_causes <- renderPlotly({
    p <-
      death_mosaic  %>% 
      group_by(disease) %>%
      summarise(deaths = sum(deaths)) %>%
      ggplot() + geom_col(aes(x = forcats::fct_reorder(disease, deaths, .desc = TRUE), y = deaths), fill = "orange") +
      theme(
        axis.text.x = element_text(
          size = 13
        ),
        axis.text.y = element_text(
          size = 13
        ),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 20)
      ) +
      xlab("Death Cause") +
      ylab("Number of Deaths") +
      ggtitle("Number of Deaths Caused by Each Factor")
    ggplotly(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
