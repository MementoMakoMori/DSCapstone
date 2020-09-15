## JHU/Coursera Data Science Certificate
## Capstone Project
## R. Holley, September 2020

myFilter <- function(chtext){
  
  ntext <- iconv(chtext, "", to="ASCII", sub="")
  
  short=c("can't", "won't", "shouldn't", "wouldn't", "couldn't", "could've", "should've", "would've", 
          "I'm", "we're", "they're", "you're", "it's", "I'll", "we'll", "you'll", "they'll", "she'll", "he'll", "I've", "we've", "you've", 
          "they've", "she'd", "he'd", "I'd", "we'd", "you'd", "they'd","y'all", "wasn't", "weren't", "don't", "haven't", "hasn't", "hadn't", "'s")
  long=c("cannot", "will not", "should not", "would not", "could not", "could have", "should have", "would have",
         "I am", "we are", "they are", "you are", "it is", "I will", "we will", "you will", "they will", "she will", "he will", "I have", "we have", "you have", 
         "they have", "she had", "he had", "I had", "we had", "you had", "they had", "you all", "was not", "were not", "do not", "have not", "has not", "had not", "")
  
  clus <- makeCluster(detectCores()-1)
  registerDoParallel(clus)
  
  foreach (i=1:length(short)) %do% {
    ntext <- gsub(short[i], long[i], ntext, ignore.case=TRUE)
  }

  chunklen <- round(length(ntext)/5)
  
  c1 <- ntext[1:chunklen]
  c2 <- ntext[chunklen:(2*chunklen)]
  c3 <- ntext[(2*chunklen):(3*chunklen)]
  c4 <- ntext[(3*chunklen):(4*chunklen)]
  c5 <- ntext[(4*chunklen):length(ntext)]
  
  ntext <- parLapply(cl=clus, X=list(c1,c2,c3,c4,c5), fun=corpus_reshape, to="sentences")
  ## the next line is optional depending on how you want to get n-gram info
  #ntext <- parLapply(cl=clus, X=ntext, fun=tokens, what="word", remove_punct=TRUE, remove_symbols=TRUE, remove_numbers=TRUE)
  
  stopCluster(clus)
  registerDoSEQ()
  return(ntext)
}