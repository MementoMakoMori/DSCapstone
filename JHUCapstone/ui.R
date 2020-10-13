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
    titlePanel("Computer-Speak"),
    
    # Intro panel
    wellPanel(
      p("Welcome to R. Holley's Text Prediction Project! The program was created as the capstone project for Johns Hopkins University/Coursera's Data Science Certificate. *SwiftKey* provided the initial text data. The most recent algorithm update was September 23, 2020.", align="center")),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          sliderInput(inputId="length", label="How many words to predict?", min=1, max=10, value = 1),
          "Start typing in the box below.",
            textAreaInput(inputId = "words", label="Input Box",
                          placeholder = "As the great poet Bo Burnham said, 'Words, words, words.'",
                          resize="vertical")
        ),

        # Show a panel of the text + prediction
        mainPanel(
            h3("And the prediction is..."),
            textOutput(outputId = "predict")
        )
    ),
    fluidRow(
      column(12,
        h4("Information", align="center"),
        p("Details about project blah blah blah", align="center"),
        p("Github info go here", align="center")
      )
    )
    )
)
