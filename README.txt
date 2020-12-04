# R. Holley Text Prediction Project
## Johns Hopkins University/Coursera Data Science Certifiate
## Capstone Project

### Summary
This little app takes text input of any length >=1 word and output one or more possible next words, along with their given probabilities. The bigram table of probabilities was built with text data provided by Swiftkey. The [app is hosted here](https://rholley.shinyapps.io/JHUCapstone/), and there is also a [small presentation here](https://rpubs.com/MementoMakoMori/695528).

The ui and server files for the shiny app are in the JHUCapstone folder.

The initial text-processing and modelbuilding is in the modelbuilding.R file. CVTests.R and testScript.R contain the cross-validation and test set scripts. The Swiftkey data was randomly divided by lines into three sets: trainCorp, cvCorp, and testCorp. These files were compressed with the `saveRDS()` function in R and should be loaded with the `readRDS()` function. appdict is the final model loaded into the web app.

Milestone report files (.Rmd and .html) have little to do with the final product, but are saved because my Coursera account links to them as a past assignment.

### Total Files
* JHUCapstone folder
  * ui.R
  * server.R
* appdict
* trainCorp
* cvCorp
* testCorp
* modelbuilding.R
* CVTests.R
* testScript.R
* MilestoneReport.html
* MilestoneReport.Rmd

### R & Packages for these scripts
* R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
Copyright (C) 2020 The R Foundation for Statistical Computing
Platform: x86_64-pc-linux-gnu (64-bit)
* quanteda Version: 2.1.1 "Quantitative Analysis of Textual Data"
* future.apply Version: 1.6.0 "Apply Function to Elements in Parallel using Futures"
* shiny "Web Application Framework for R" Version: 1.5.0

