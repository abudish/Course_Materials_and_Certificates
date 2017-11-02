#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Tabs!"),
  
  sidebarLayout(
    sidebarPanel(
       textInput("box1", "Enter tab 1 text:", value = "Tab 1!"),
       textInput("box2", "Enter tab 2 text:", value = "Tab 2!"),
       textInput("box3", "Enter tab 3 text:", value = "Tab 3!")
    ),
    
    # Create tabs panel
    mainPanel(
       tabsetPanel(type = "tabs",
                   tabPanel("Tab 1", br(), textOutput("out1")),
                   tabPanel("Tab 2", br(), textOutput("out2")),
                   tabPanel("Tab 3", br(), textOutput("out3"))
        )
    )
  )
))
