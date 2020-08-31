myFilter <- function(chtext){
  
  ntext <- iconv(chtext, "", to="ASCII", sub="")
  
  short=c("can't", "won't", "shouldn't", "wouldn't", "couldn't", "could've", "should've", "would've", 
          "I'm", "they're", "you're", "it's", "I've", "you've", "they've", "I'd", "you'd", "they'd", "y'all", 
          "wasn't", "weren't", "'s")
  long=c("cannot", "will not", "should not", "would not", "could not", "could have", "should have", "would have",
         "I am", "they are", "you are", "it is", "I have", "you have", "they have", "I would", "you would", "they would", "you all", 
         "was not", "were not", "")
  
  clus <- makeCluster(detectCores()-1)
  registerDoParallel(clus)
  
  foreach (i=1:length(short)) %do% {
    ntext <- gsub(short[i], long[i], ntext, ignore.case=TRUE)
  }
  ntext <- gsub("(http[s]?://)?[^([\"<,>]*[.][[\",><]*", "", ntext, ignore.case=TRUE)
  
  chunklen <- round(length(ntext)/5)
  
  c1 <- ntext[1:chunklen]
  c2 <- ntext[chunklen:(2*chunklen)]
  c3 <- ntext[(2*chunklen):(3*chunklen)]
  c4 <- ntext[(3*chunklen):(4*chunklen)]
  c5 <- ntext[(4*chunklen):length(ntext)]
  
  ntext <- parLapply(cl=clus, X=list(c1,c2,c3,c4,c5), fun=corpus_reshape, to="sentences")
  #ntext <- parLapply(cl=clus, X=ntext, fun=tokens, what="word", remove_punct=TRUE, remove_symbols=TRUE, remove_numbers=TRUE)
  
  stopCluster(clus)
  registerDoSEQ()
  return(ntext)
}