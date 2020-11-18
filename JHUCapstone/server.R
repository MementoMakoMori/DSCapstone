#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(quanteda)
library(data.table)

# collTable <- readRDS()
# uniq <- readRDS()

## functions for fetching predictions
predFun <- function(words, lg){
  words <- words %>% tokens(what="word", remove_punct=TRUE, remove_symbols=TRUE, remove_numbers=TRUE, remove_url=TRUE, remove_separators=TRUE, padding=TRUE) %>% tolower()
  if(is.known(words)==FALSE){return("Error: No matching predictors")}
  if(length(words)==1){
    pred<-oneword(words, lg)
    return(pred)
  } else if(is.known(words)==1){
    pred<-oneword(words, lg)
  } else if(length(words)>1){
    pred<-multiword(words, length(words), lg)
    return(pred)
  }
}

is.known <- function(words){
  known <- sum(words %in% uniq)
  if(known==0){return(FALSE)
  }else{(return(known))}
}

multiword <- function(words, ntok, lg){
  w2ind<-ntok
  for(i in 1:ntok){
    if(is.known(words[w2ind])==1){
      break()
    }else{w2ind <- ntok-i}
  }
  w1ind<-w2ind-1
  for(j in 1:w2ind){
    if(is.known(words[w1ind])==1){
      break()
    }else{w1ind<-w2ind-j}
  }
  predInd<-which(fullTable$n1==words[w1ind]&fullTable$n2==words[w2ind]&!is.na(fullTable$n3))
  if(length(predInd)==0){
    predInd <- which(fullTable$n1==words[w2ind]|fullTable$n1==words[w1ind]&is.na(fullTable$n3))
    predCol <- "n2"
  } else {predCol <- "n3"}
  if(length(predInd)<lg){lg<-length(predInd)}
  pred <- setorder(fullTable[predInd,c(predCol, "count")], -count)
  return(pred[1:lg,])
}

oneword <- function(words, lg){
  predInd <- which(fullTable$n1==words)
  pred <- fullTable[predInd, c("n2", "count", "z")]
  if(length(predInd)<lg){lg<-length(predInd)}
  return(setorder(pred, -count)[1:lg,])
}

## Server logic starts here
shinyServer(function(input, output) {
    output$predict <- renderText({
      if(length(input$words)==0){return("")}
      tok <- as.character(input$words) %>% corpus() %>% corpus_reshape(to="sentences") %>% tolower() %>% tokens(what="word", remove_punct=TRUE, remove_symbols=TRUE, remove_numbers=TRUE, remove_url=TRUE, remove_separators=TRUE, padding=TRUE)
      pred <- predFun(tok[length(tok)], input$length)
      return(unlist(pred[,1]))
    })
})
