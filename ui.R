
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Type a phrase in the input box and 
               press the predict button [1 minute]"),
    
    # Text input
    textInput("phrase", "Phrase", ""),
    #
    actionButton("predButton", "Predict!"),
    #
    verbatimTextOutput("value")
    
))
