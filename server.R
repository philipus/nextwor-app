
library(shiny)
library(dplyr)
library(R.utils)
library(tidyr)
library(rlist)
library(stringr)
library(stringi)
library(udpipe)

if( 'english-ewt-ud-2.5-191206.udpipe' %in% list.files("data")) {
    dl.model.file <- "data/english-ewt-ud-2.5-191206.udpipe"
} else {
    dl <- udpipe_download_model(language = "english")
    dl.model.file <- dl$file_model
}

trigram_df <- readRDS("data/trigram_df.rds")
bigram_df <- readRDS("data/bigram_df.rds")
unigram_df <- readRDS("data/unigram_df.rds")

source(file="Rfunction.R", local  = TRUE)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    complWord <- eventReactive(input$predButton, {
        withProgress(message = 'Predicting next word', value = 0, {
            if(length(input$phrase) > 0) {
                nextWord <- get_out_pred(input$phrase)
                paste0(input$phrase, " ", nextWord$pred)
            } else {
                input$phrase
            }
        }
    )})
    
        output$value <- renderText({ 
            complWord()  
        })

})
