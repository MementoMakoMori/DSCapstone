##to do: 
## DONE remove non ASCII chars
## DONE replace contractions
## DONE reshape by sentence
## DONE remove punctuation (with collocation function)
## DONE collocate bigrams and trigrams into tables

## first filter for text that was loaded with corpus(readLines())
myFilter <- function(chtext){
   
  clus <- makeCluster(detectCores()-1)
  registerDoParallel(clus)
  
  ## ensure all text in the corpii is ASCII
  ntext <- iconv(chtext, "", to="ASCII", sub="")
  
  ## removing contractions is a personal preference
  ## however I did not remove profanity
  ## that probably says something about me as a person
  short=c("can't", "won't", "shouldn't", "wouldn't", "couldn't", "could've", "should've", "would've", 
          "I'm", "we're", "they're", "you're", "it's", "I'll", "we'll", "you'll", "they'll", "she'll", "he'll", "I've", "we've", "you've", 
          "they've", "she'd", "he'd", "I'd", "we'd", "you'd", "they'd","y'all", "wasn't", "weren't", "don't", "haven't", "hasn't", "hadn't", "'s")
  long=c("cannot", "will not", "should not", "would not", "could not", "could have", "should have", "would have",
         "I am", "we are", "they are", "you are", "it is", "I will", "we will", "you will", "they will", "she will", "he will", "I have", "we have", "you have", 
         "they have", "she had", "he had", "I had", "we had", "you had", "they had", "you all", "was not", "were not", "do not", "have not", "has not", "had not", "")
 
  foreach (i=1:length(short)) %do% {
    ntext <- gsub(short[i], long[i], ntext, ignore.case=TRUE)
  }
  
  ## this line was supposed to remove urls, but actually removed entire entries! oops
  #ntext <- gsub("(http[s]?://)?[^([\"<,>]*[.][[\",><]*", "", ntext, ignore.case=TRUE)
  
  ## instead of one giant corpus, 5 smaller corpii in a list is much nicer for processing
  chunklen <- round(length(ntext)/5)
  c1 <- ntext[1:chunklen]
  c2 <- ntext[(chunklen+1):(2*chunklen)]
  c3 <- ntext[(2*chunklen+1):(3*chunklen)]
  c4 <- ntext[(3*chunklen+1):(4*chunklen)]
  c5 <- ntext[(4*chunklen+1):length(ntext)]

  ## NOTE: parLapply uses X and fun args instead of x and FUN like other cluster functions
  ## this parLapply plays nice and only uses the 5 cores I asked it too
  ntext <- parLapply(cl=clus, X=list(c1,c2,c3,c4,c5), fun=corpus_reshape, to="sentences", c(remove_numbers = TRUE, remove_symbols=TRUE))
  stopCluster(clus)
  registerDoSEQ()
  return(ntext)
}

## still haven't figured out why textstat_collocations with parLapply uses ALL my cores
## I specifically tell it not to do that
colloc <- function(chtext, n){
  
  ## me: plz don't use all my core
  clus <- makeCluster(detectCores()-1)
  registerDoParallel(clus)

  ## parLapply: use ALL the cores!
  ntext <- parLapply(cl=clus, X=chtext[1:length(chtext)], fun=textstat_collocations, size=n, remove_symbols=TRUE, remove_numbers=TRUE, remove_punc=TRUE, verbose=TRUE)
  
  ## the combining code here was moved to colloc_join()
  stopCluster(clus)
  registerDoSEQ()

  return(ntext)
}

colloc_join <- function(chtext){
  
  ##start with the first colloc table
  combs <- chtext[[1]][,1:2]
  
  ## combine collocations: add for identical pairs, 0 for no match
  ## and remove extraneous columns
  foreach(i=2:length(chtext))%do%{
    combs <- full_join(combs, chtext[[i]][,1:2], by="collocation")
    combs[,2:3] <- nafill(combs[,2:3], fill=0)
    combs <- mutate(combs, count=(count.x+count.y), count.x=NULL, count.y=NULL)
  }
  
  #TA DA!
  return(combs)
}

## tokenization is included with textstat_collocation
## so this function became unnecessary
nTok <- function(chtext){
  clus <- makeCluster(detectCores()-1)
  registerDoParallel(clus)
  ntext <- parLapply(cl=clus, fun=tokens, X=chtext, what="word", remove_punct=TRUE, remove_symbols=TRUE, remove_numbers=TRUE)
  stopCluster(clus)
  registerDoSEQ()
  return(ntext)
}

## why did I give up on this function? it took ~12 hours to 1/20 of the data
## my old processor can only handle so much
nGrams <- function(chtext){
  clus<-makeCluster(detectCores()-1)
  registerDoParallel(clus)
  ntext <- parLapply(cl=clus, X=chtext, fun=tokens_ngrams, n=2:3)
  stopCluster(clus)
  registerDoSEQ()
  return(ntext)
}

## test functions etc below
colloc_merge <- function(chtext){
  combs <- chtext[[1]]
  foreach(i=1:length(chtext))%do%{
    combs <- merge(combs, chtext[[i]], by="collocation", all=TRUE)
  }
  return(combs)
}