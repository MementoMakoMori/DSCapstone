## R. Holley
## text-prediction shiny app, server file
## Data Science Certificate, JHU/Coursera

library(shiny)
library(quanteda)

wordList <- readRDS("./appdict")

## functions for fetching predictions
predFun <- function(words, lg){
  words <- words %>% tokens(what="word", remove_punct=TRUE, remove_symbols=TRUE, remove_numbers=TRUE, remove_url=TRUE, remove_separators=TRUE, padding=TRUE) %>% tolower()
  if(length(words)==0){return("Please enter text on the left.")}
  if(is.known(words)==FALSE){
    pred <- wordList$the
    return(pred[1:lg])
    }
  if(length(words)==1){
    pred <- oneword(words, lg)
    return(pred)
  }else{
    i <- length(words)
    if(is.known(words[i])==FALSE){
      while(is.known(words[i])==FALSE){
        i <- i-1
      }
    }
    pred <- oneword(words[i], lg)
    return(pred)
  }
}

is.known <- function(words){
  known <- sum(words %in% names(wordList))
  if(known==0){return(FALSE)
  }else{(return(known))}
}


oneword <- function(words, lg){
  pred <- wordList[[words]]
  if(lg > length(wordList[[words]])){lg<-length(wordList[[words]])}
  return(round(pred[1:lg], 3))
}

## Server logic starts here
shinyServer(function(input, output) {
    output$predict <- renderTable({
      tok <- as.character(input$words) %>% corpus() %>% corpus_reshape(to="sentences") %>% tolower() %>% tokens(what="word", remove_punct=TRUE, remove_symbols=TRUE, remove_numbers=TRUE, remove_url=TRUE, remove_separators=TRUE, padding=TRUE)
      pred <- predFun(tok[length(tok)], input$length)
      return(cbind("Word"=names(pred),"Probability"=pred))
    })
})