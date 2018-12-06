#server <- function(input,output, session){
#}
#output$mymap <- renderLeaflet({
#  m <- leaflet() %>%
#    addTiles() %>%
#    setView(lng=-73.935242, lat=40.730610 , zoom=10)
#  m
#})
library(shiny)
library(shiny)
library(ggplot2)# load ggplot
library(plyr)

scatterplot<-function(dataset,x,y,alpha=0.6){
  p<-ggplot(dataset, aes(x=x, y=y))+geom_point(aes(col="skyblue"), alpha=alpha, size =2,stroke = 0)
  +theme_classic(14)+geom_density_2d(color = 'rosybrown',bins = 20)
    
  p
}

measureBirth <- read.csv("data/Clean data/measureBirth_clean.csv")
# Define server logic required to draw a map
server <- function(input, output, var, session) {
  
  output$plot <- renderPlotly({
    datainput <- switch(input$var, 
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
        color = ~datainput, colors = 'Purples'
      ) %>%
      colorbar(title = input$var) %>%
      layout(
        title = '1994-2003 US Preventive Diseases Summary by State<br>(Hover for breakdown)',
        geo = g
      )
    
  })
  

  
  
}
  

