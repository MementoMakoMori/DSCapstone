##to do: 
## DONE remove non ASCII chars
## DONE replace contractions
## DONE reshape by sentence
## DONE remove punctuation/#s etc w/tokenization

library(quanteda)
library(quanteda.textmodels)
library(tokenizers)
library(parallel)
library(doParallel)
library(stopwords)

## set cluster cores so it's not re-evaluated for each function
cores <- detectCores()-1

## first function for text that was loaded with corpus(readLines())
myFilter <- function(chtext){
   
  clus <- makeCluster(cores)
  registerDoParallel(clus)
  
  ## ensure all text in the corpii is ASCII
  ntext <- iconv(chtext, "", to="ASCII", sub="")
  
  ## removing contractions is a personal preference
  ## however I did not remove profanity
  ## that probably says something about me as a person
  # short=c("can't", "won't", "shouldn't", "wouldn't", "couldn't", "could've", "should've", "would've", 
  #         "I'm", "we're", "they're", "you're", "it's", "I'll", "we'll", "you'll", "they'll", "she'll", "he'll", "I've", "we've", "you've", 
  #         "they've", "she'd", "he'd", "I'd", "we'd", "you'd", "they'd","y'all", "wasn't", "weren't", "don't", "haven't", "hasn't", "hadn't", "'s")
  # long=c("cannot", "will not", "should not", "would not", "could not", "could have", "should have", "would have",
  #        "I am", "we are", "they are", "you are", "it is", "I will", "we will", "you will", "they will", "she will", "he will", "I have", "we have", "you have", 
  #        "they have", "she had", "he had", "I had", "we had", "you had", "they had", "you all", "was not", "were not", "do not", "have not", "has not", "had not", "")
  # 
  # foreach (i=1:length(short)) %do% {
  #   ntext <- gsub(short[i], long[i], ntext, ignore.case=TRUE)
  # }
  
  ## instead of one giant corpus, 5 smaller corpii in a list is much nicer for processing
  chunklen <- round(length(ntext)/5)
  c1 <- ntext[1:chunklen]
  c2 <- ntext[(chunklen+1):(2*chunklen)]
  c3 <- ntext[(2*chunklen+1):(3*chunklen)]
  c4 <- ntext[(3*chunklen+1):(4*chunklen)]
  c5 <- ntext[(4*chunklen+1):length(ntext)]
  
  rm_string <- "[[:digit:]]|w{3}."
  ## I know this parLapply section doesn't look pretty, but it is efficient for time and RAM
  ntext <- parLapply(cl=clus, X=list(c1,c2,c3,c4,c5), fun=corpus_reshape, to="sentences")
  ntext <-parLapply(cl=clus, X=ntext, fun=tokenize_words, lowercase=TRUE, strip_punct=TRUE, strip_numeric=TRUE)
  ntext <- parLapply(cl=clus, X=ntext, fun=tokens, remove_url=TRUE, preserve_tags=FALSE)
  ntext <-  parLapply(cl=clus, X=ntext, fun=tokens_remove, pattern=rm_string, valuetype="regex")
  ## tokens_remove is necessary bc tokens() "remove_url" only removes URLs that start with http/https
  ## it also doesn't remove number-hybrid words like '20th' and '1960s'
  stopCluster(clus)
  registerDoSEQ()
  ## return the chunks concatenated, so object is tokens and not list of tokens
  #print(c(address(ntext), refs(ntext)))
  return(c(ntext[[1]],ntext[[2]],ntext[[3]],ntext[[4]],ntext[[5]]))
  
}

## this function was for time and RAM tests
# miniFilt <- function(chtext){
#   clus <- makeCluster(cores)
#   registerDoParallel(clus)
#   
#   chunklen <- round(length(chtext)/5)
#   c1 <- chtext[1:chunklen]
#   c2 <- chtext[(chunklen+1):(2*chunklen)]
#   c3 <- chtext[(2*chunklen+1):(3*chunklen)]
#   c4 <- chtext[(3*chunklen+1):(4*chunklen)]
#   c5 <- chtext[(4*chunklen+1):length(chtext)]
#   rm_string <- "[[:digit:]]|w{3}."
#   
#   t<-Sys.time()
#   print(mem_change(n1 <- parLapply(cl=clus, X=list(c1,c2,c3,c4,c5), fun=corpus_reshape, to="sentences") %>%
#     parLapply(cl=clus, fun=tokenize_words, lowercase=TRUE, strip_punct=TRUE, strip_numeric=TRUE) %>%
#     parLapply(cl=clus, fun=tokens, remove_url=TRUE, preserve_tags=FALSE) %>%
#     parLapply(cl=clus, fun=tokens_remove, pattern=rm_string, valuetype="regex")))
#   print(Sys.time()-t)
#    t<-Sys.time()
#   print(mem_change({
#     n2 <- parLapply(cl=clus, X=list(c1,c2,c3,c4,c5), fun=corpus_reshape, to="sentences")
#     n2 <- parLapply(cl=clus, X=n2, fun=tokenize_words, lowercase=TRUE, strip_punct=TRUE, strip_numeric=TRUE)
#     n2 <- parLapply(cl=clus, X=n2, fun=tokens, remove_url=TRUE, preserve_tags=FALSE)
#     n2 <- parLapply(cl=clus, X=n2, fun=tokens_remove, pattern=rm_string, valuetype="regex")
#   }))
#   print(Sys.time()-t)
#   stopCluster(clus)
#   registerDoSEQ()
# }

## tokenization is the part that takes a long time
# toks <- function(chtext){
#   clus <- makeCluster(cores)
#   registerDoParallel(clus)
#   ntext <- parLapply(cl=clus, X=chtext, fun=tokens, what="word", remove_punct=TRUE, remove_symbols=TRUE, remove_numbers=TRUE, remove_url=TRUE, preserve_tags=FALSE)
#   ## tokens option "remove_url" only removes URLs that start with http/https
#   ## it also doesn't remove number-hybrid words like '20th' and '1960s'
#   rm_string <- "[[:digit:]]|w{3}.[[:alnum:]]+"
#   ntext <- parLapply(cl=clus, X=ntext, fun=tokens_remove, pattern=rm_string, valuetype="regex")
#   stopCluster(clus)
#   registerDoSEQ()
#   ## return the chunks concatenated, so object is tokens and not list of tokens
#   return(c(ntext[[1]],ntext[[2]],ntext[[3]],ntext[[4]],ntext[[5]]))
# }

## so these functions *worked*, when it's all said and done
## but on 6 cores finding collocations took ~ 30 hours
## and ~10 hours on 16 cores D:

## still haven't figured out why textstat_collocations with parLapply uses all my cores
## I specifically tell it not to do that
# colloc <- function(chtext, n, m){
#   
#   ## me: plz don't use all my core
#   clus <- makeCluster(cores)
#   registerDoParallel(clus)
# 
#   ## parLapply: use ALL the cores!
#   ntext <- parLapply(cl=clus, X=chtext, fun=textstat_collocations, size=n, min_count=m, remove_symbols=TRUE, remove_numbers=TRUE, remove_punc=TRUE, verbose=TRUE)
#   
#   ## the combining code previously here was moved to colloc_join()
#   stopCluster(clus)
#   registerDoSEQ()
# 
#   return(ntext)
# }
# 
# colloc_join <- function(chtext){
#   
#   ##start with the first colloc table
#   combs <- chtext[[1]][,1:2]
#   
#   ## combine collocations: add for identical pairs, 0 for no match
#   ## and remove extraneous columns
#   foreach(i=2:length(chtext))%do%{
#     combs <- full_join(combs, chtext[[i]][,1:2], by="collocation")
#     combs[,2:3] <- nafill(combs[,2:3], fill=0)
#     combs <- mutate(combs, count=(count.x+count.y), count.x=NULL, count.y=NULL)
#   }
#   
#   #TA DA!
#   return(combs)
# }

## why did I give up on this function? it took ~12 hours to 1/20 of the data
## my old processor can only handle so much
# nGrams <- function(chtext){
#   clus<-makeCluster(cores)
#   registerDoParallel(clus)
#   ntext <- parLapply(cl=clus, X=chtext, fun=tokens_ngrams, n=2:3)
#   stopCluster(clus)
#   registerDoSEQ()
#   return(ntext)
# }