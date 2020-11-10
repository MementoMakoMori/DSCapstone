## R. Holley
## Cross-validation testing
## Data Science Certificate, JHU/Coursera

cvCorp <- readRDS("cvCorp")
fullTable <- readRDS("combTable")
source("./practiceSort.R")

uniq <- unique(unlist(c(trainTable$n1, trainTable$n2, trainTable$n3)))

library(doFuture)
library(foreach)

smaller <- cvCorp[1:1000]

clus <- makeCluster(3)
registerDoParallel(clus)

n<-0
accSMALL <- foreach(i=1:length(smaller), .combine = c, .packages = "foreach")%:%
  foreach(j=1:(length(smaller[[i]])-2))%dopar%{
    pred <- predFun(smaller[[i]][j:(j+1)], 1)
    n <- n+1
    if(smaller[[i]][j+2] %in% pred){
      return(TRUE)
    } else {return(FALSE)}
  }
}



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
    predInd <- which(fullTable$n1==words[w2ind]|fullTable$n1==words[w1ind])
    pred <- setorder(fullTable[predInd, c("n2", "count")], -count)
    if(length(predInd)<lg){lg<-length(predInd)}
    return(pred$n2[1:lg])
  } else {
  pred <- setorder(fullTable[predInd,c("n3", "count")], -count)
  if(length(predInd)<lg){lg<-length(predInd)}
  return(pred$n3[1:lg])
}

oneword <- function(words, lg){
  predInd <- which(fullTable$n1==words)
  pred <- setorder(fullTable[predInd, c("n2", "count")], -count)
  if(length(predInd)<lg){lg<-predInd}
  return(pred$n2[1:lg])
}


