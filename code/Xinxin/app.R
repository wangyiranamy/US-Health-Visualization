#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#ui <- fluidPage()
#server <- function(input, output, session) {}
#shinyApp(ui = ui, server = server)
preventive_df1 <- read.csv("/Users/yiranwang/US-Health-Visualization/data/Clean data/preventive_df1.csv")




# Run the application 
shinyApp(ui = ui, server = server)
