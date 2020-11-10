# fullTable <- readRDS("./fullTable")
# uniq <- readRDS("./uniq")
library(quanteda)
library(dplyr)
library(data.table)

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
  predInd<-which(fullTable$n1==words[w1ind]&fullTable$n2==words[w2ind])
  if(length(predInd)==0){
    predInd <- which(fullTable$n1==words[w2ind]|fullTable$n1==words[w1ind])
    predCol <- c("n1", "n2")
  } else {predCol<-c("n1", "n2", "n3")}
  pred <- setorder(fullTable[predInd,c(predCol, "count", "z")], -z)
  if(length(predInd)<lg){lg<-length(predInd)}
  return(pred[1:lg,])
}

oneword <- function(words, lg){
  predInd <- which(fullTable$n1==words)
  pred <- fullTable[predInd, c("n2", "count", "z")]
  if(length(predInd)<lg){lg<-predInd}
  return(setorder(pred, -count)[1:lg,])
}