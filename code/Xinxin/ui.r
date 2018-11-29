library(shiny)

#shinyUI(pageWithSidebar(
  #headerPanel("header"),
  #sidebarPanel(
    #selectInput("Birth Measure1", "Choose one:", choices = c("Late_care","Premature", "Unmarried"),
  #selectInput("Birth Measure1", "Choose one:", choices = c("Late_care","Premature", "Unmarried"))
  #),
  #mainPanel (
    #tabsetPanel(
      #tabPanel("scatter plot", plotOutput("scatterplot")),
      #tabPanel("Dot plot", plotOutput("Dotplot"))
    #)
    
  #)
#)
  
#)
#)


ui <- fluidPage(
  titlePanel("US Health preventive diseases Status"),
  verbatimTextOutput("click"),
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with 
               information from the US Preventive Diseases Summary from 1994-2003."),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Haemophilus Influenzae B", "Hepatitis A",
                              "Hepatitis B", "Measles", "Pertussis","Congenital Rubella Syndrome","Syphilis"),
                  selected = "Haemophilus Influenzae B")
      ),
    
    mainPanel(
              tabsetPanel(
              tabPanel("Map", plotlyOutput("plot")))
              ))
  )
  