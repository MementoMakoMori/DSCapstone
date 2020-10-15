### Johns Hopkins University/Coursera
### Data Science Certificate: Capstone Project
### Text Prediction App 
### R. Holley, September 2020

library(stringr)

predWord <- function(chars){

  spls <- str_split(chars, pattern=" ")
  lg <- length(spls[[1]])
  ## if user enters 2 or more words
  if(lg >= 2) {results <- multiword(spls, lg)}
  ## user enters 1 word
  if (lg == 1) {results <- oneword(spls)}

  if(nrow(results)>10){return(as.character(results$collocation[1:10]))} 
  else return(as.character(results$collocation))
}

multiword <- function(spls, lg){
  colSearch <- paste(spls[[1]][lg-1], spls[[1]][lg], sep=" ")
  pred <- grep(paste(colSearch, " ", sep=""), trigrams$collocation, ignore.case=TRUE)
  return(setorder(trigrams[pred,], -count))
}

oneword <- function(spls){
  pred <- grep(paste(spls, " ", sep=""), bigrams$collocation, ignore.case=TRUE)
  return(setorder(bigrams[pred,], -count))
}


####
####
## Test Functions
## TO DO: add skip-gram for ignoring unknown words

testCondSmall <- function(chars){
  accuracy <- NULL
  if(length(str_split(chars$collocation[1], pattern=" ")[[1]])>2){
    foreach(i=1:length(chars$collocation))%do%{
      spls <- str_split(chars$collocation[i], pattern=" ")
      colSearch <- paste(spls[[1]][1], spls[[1]][2], sep=" ")
      pred <- grep(paste(colSearch, " ", sep=""), triSmall$collocation, ignore.case=TRUE)
      results <- setorder(triSmall[pred,], -count)
      
      if(nrow(results)>10){matches <- as.character(results$collocation[1:10])} 
      else {matches <- as.character(results$collocation)}
      
      if(sum(matches==chars$collocation[i])==1){accuracy <- c(accuracy, TRUE)}else{accuracy <- c(accuracy, FALSE)}
    }
  }
  if(length(str_split(chars$collocation[1], pattern=" ")[[1]])==2){
    foreach(i=1:length(chars$collocation))%do%{
      spls <- str_split(chars$collocation[i], pattern=" ")
      pred <- grep(paste(spls[[1]][1], " ", sep=""), biSmall$collocation, ignore.case = TRUE)
      results <- setorder(biSmall[pred,], -count)
      
      if(nrow(results)>10){matches <- as.character(results$collocation[1:10])} 
      else {matches <- as.character(results$collocation)}
      
      if(sum(matches==chars$collocation[i])==1){accuracy <- c(accuracy, TRUE)}else{accuracy <- c(accuracy, FALSE)}
    }
  }
  return((sum(accuracy)/length(chars$collocation)))
}
  
testCondFull <- function(chars){
  accuracy <- NULL
  if(length(str_split(chars$collocation[1], pattern=" ")[[1]])>2){
    foreach(i=1:length(chars$collocation))%do%{
      spls <- str_split(chars$collocation[i], pattern=" ")
      colSearch <- paste(spls[[1]][1], spls[[1]][2], sep=" ")
      pred <- grep(paste(colSearch, " ", sep=""), trigrams$collocation, ignore.case=TRUE)
      results <- setorder(trigrams[pred,], -count)
      
      if(nrow(results)>10){matches <- as.character(results$collocation[1:10])} 
      else {matches <- as.character(results$collocation)}
      
      if(sum(matches==chars$collocation[i])==1){accuracy <- c(accuracy, TRUE)}else{accuracy <- c(accuracy, FALSE)}
    }
  }
  if(length(str_split(chars$collocation[1], pattern=" ")[[1]])==2){
    foreach(i=1:length(chars$collocation))%do%{
      spls <- str_split(chars$collocation[i], pattern=" ")
      pred <- grep(paste(spls[[1]][1], " ", sep=""), bigrams$collocation, ignore.case = TRUE)
      results <- setorder(bigrams[pred,], -count)
      
      if(nrow(results)>10){matches <- as.character(results$collocation[1:10])} 
      else {matches <- as.character(results$collocation)}
      
      if(sum(matches==chars$collocation[i])==1){accuracy <- c(accuracy, TRUE)}else{accuracy <- c(accuracy, FALSE)}
    }
  }
  return((sum(accuracy)/length(chars$collocation)))
}