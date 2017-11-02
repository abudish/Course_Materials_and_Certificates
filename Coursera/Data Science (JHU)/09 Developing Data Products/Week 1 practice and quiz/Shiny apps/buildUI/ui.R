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
        titlePanel(h1("My Shiny App")),
       sidebarLayout(
               sidebarPanel(
                       h2("Installation"),
                       p("Shiny is available on CRAN, so you can install it in the usual way from your R console:"),
                       code('install.packages("shiny")'),
                       
                       br(),
                       br(),
                       br(),
                       br(),
                       
                       p(
                               img(src="bigorb.png", height = 40, width = 45),
                               "shiny is product of ",
                               a(href = "https://www.rstudio.com/", "RStudio", style="color:blue")    
                       )
                       
               ),
               mainPanel(
                       h1("Introducing Shiny"),
                       p(
                               "Shiny is a new package from RStudio that makes it ",
                               em("incredibly easy"),
                               "to build interactive web applications in R"
                       ),
                       
                       br(),
                       
                       p(
                               "For an introduction and live examples, visit the ",
                               a(href = "https://shiny.rstudio.com/", "Shiny homepage")
                       ),
                       
                       br(),
                       br(),
                       
                       h1("Features"),
                       
                       tags$ul(
                               tags$li("Build useful web applications with only a few lines of code - no JavaScript required."),
                               tags$li("Shiny applications are automatically \"live\" in the same way that ", strong("spreadsheets"), 
                                       "are live. Outputs change instatntly as users modify inputs, without requiring a reload of the browser")
                       )
                       
                       
                       
               )
       )
))
