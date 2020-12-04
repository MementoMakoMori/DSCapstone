## R. Holley
## text-prediction shiny app, ui file
## Data Science Certificate, JHU/Coursera

library(shiny)

# Define UI
shinyUI(fluidPage(

    # Application title
    titlePanel("Text Prediction"),
    
    # Intro panel
    wellPanel(
      p("Welcome to R. Holley's Text Prediction Project! The most recent update was November 24, 2020.", align="center")),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
          sliderInput(inputId="length", label="How many possible words to return?", min=1, max=15, value = 1),
          "Start typing in the box below.",
            textAreaInput(inputId = "words", label="Input Box",
                          placeholder = "As the great poet Bo Burnham said, 'Words, words, words.'",
                          resize="vertical")
        ),

        # Show a panel of the text + prediction
        mainPanel(
            h3("And the prediction is..."),
            tableOutput(outputId = "predict")
        )
    ),
    # Footer with extra details
    fluidRow(
      column(12,
        h4("Information", align="center"),
        p("This program was created as the capstone project for Johns Hopkins University/Coursera's Data Science Certificate. SwiftKey provided the text data.", align="center"),
        p("The model for prediction is a simple Markov-Chain: given the current state, WordA, what are the probable next states? The 'next states' are WordB, WordC, WordX,etc, with probabilities given as the sum of WordA and WordX ordered co-occurances over the total number of WordA occurances. For a more detailed report on the building, cross-validation, and testing of this model, ", tags$a(href="https://github.com/MementoMakoMori/DSCapstone","click here"), "or go to ",tags$a(href="https://github.com/MementoMakoMori/DSCapstone","github.com/MementoMakoMori/DSCapstone"), align="center")
      )
    )
    )
)
