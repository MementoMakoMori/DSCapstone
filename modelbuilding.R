## text sampling and model building
## R. Holley

library(quanteda)
library(dplyr)
library(doParallel)

#clus <- makeCluster(detectCores()-1)

con1 <- file("./en_US/en_US.news.txt")
con2 <- file("./en_US/en_US.blogs.txt")
con3 <- file("./en_US/en_US.twitter.txt")

fullCorp <- corpus(c(readLines(con1, skipNul = TRUE),readLines(con2, skipNul = TRUE),readLines(con3, skipNul=TRUE)))
close(con1); close(con2); close(con3)

set.seed(777)
train <- sample(1:length(fullCorp), size=(length(fullCorp)*0.8), replace=FALSE)
trainCorp <- fullCorp[train]
remain <- fullCorp[-train]
cv <- sample(1:length(remain), size=length(fullCorp)*0.15, replace=FALSE)
cvCorp <- remain[cv]
testCorp <- remain[-cv]

## model building process:
## 1. shape corpus to sentences -> tokenize to words
## 2. create a DFM to identify and remove words with frequency < 5
## 3. create collocations table

trainCorp <- corpus_reshape(trainCorp, to="sentences") %>% tolower() %>% tokens(what = "word", remove_punct=TRUE, remove_symbols=TRUE, remove_numbers=TRUE, remove_url=TRUE, padding=TRUE)
fewtoks <- dfm(trainCorp) %>% dfm_trim(max_termfreq = 9) %>% colnames()
filtCorp <- tokens_remove(trainCorp, fewtoks)

chunklen <- round(length(filtCorp)/3)
splitCorp <- list(filtCorp[1:chunklen], filtCorp[(chunklen+1):(2*chunklen)], filtCorp[((2*chunklen)+1):length(filtCorp)])
rm(filtCorp, chunklen)
gc()
clus <- makeCluster(3)
registerDoParallel(clus)
system.time(collTab <- parLapply(cl=clus, X=splitCorp, fun=textstat_collocations, size=c(2,3), min_count=3))
stopCluster(clus)
## combining tokenization in the collocations function ran for 4 days without finishing so I decided to stop it
## and separated the two functions

cluster <- makeCluster(6)
registerDoParallel(cluster)
combs <- trainTable[[1]]

foreach(i=2:3)%do%{
  combs <- full_join(x=combs, y=trainTable[[i]], by="collocation")
  combs[,2:length(colnames(combs))] <- parCapply(cl=cluster, x=combs[,2:length(colnames(combs))], FUN=nafill, fill=0)
  combs <- mutate(combs, count=(count.x+count.y), count.x=NULL, count.y=NULL)
  combs <- mutate(combs, count_nested=(count_nested.x+count_nested.y), count_nested.x=NULL, count_nested.y=NULL)
  }
## YAY
stopCluster(cluster)
registerDoSEQ()

words <- tokens(collocTrim$collocation, what="word")

library(future.apply)
plan(multicore)
n1 <- future_lapply(words, function(i){i[1]})
n2 <- future_lapply(words, function(i){i[2]})
n3 <- future_lapply(words, function(i){i[3]})
plan(sequential)
trainTable <- mutate(trainTable, "n1" = n1, "n2" = n2, "n3" = n3)
uniq <- unique(unlist(words))

## cross-validation processing and testing
cvCorp <- corpus_reshape(cvCorp, to="sentences") %>% tolower() %>% tokens(what = "word", remove_punct=TRUE, remove_symbols=TRUE, remove_numbers=TRUE, remove_url=TRUE, padding=TRUE)









## test on a fraction of the train data

smallSet <- tokens(smallSet, what="word", remove_punct = TRUE, remove_symbols=TRUE, remove_numbers = TRUE, remove_url = TRUE, padding=TRUE, verbose=quanteda_options("verbose"))
## that tokenization only took a few seconds. the collocation part is what takes a long time. hypothetically
system.time(smallCol <- textstat_collocations(smallSet, size=c(2,3), min_count = 2))

# user  system elapsed 
# 218.868   0.067 109.655 
# 
# > (length(chunks[[1]])/10000)*(109.655/60)
# [1] 62.42549
## so it should hypothetically take 1 hours to colloc each of the 10 chunks when run sesquentially
## now try doing the same section with a parallel processor
## min_count must be 1 instead of 2
clus <- makeCluster(2)
registerDoParallel(clus)
parSet <- list(smallSet[1:5000],smallSet[5001:10000])
system.time(parCol <- parLapply(cl=clus, X=parSet, fun=textstat_collocations, size=c(2,3), min_count=1))
stopCluster(clus)
# user  system elapsed 
# 0.371   0.313 156.347 
##using parallelization took more time!
## but that was with the min_count=1 requirement
## last one, try doing sequential but with min_count=1
system.time(smallCol2 <- textstat_collocations(smallSet, size=c(2,3), min_count = 1))
# user   system  elapsed 
# 2235.836    0.580 1118.977 
# > (length(chunks[[1]])/10000)*(1118.7/60)
# [1] 636.8647
## yikes, min_count=1 is what's really slowing down the colloc function
## what if I removed low-frquency tokens before calculating cooccurance?
smalltok <- smallSet
smalldfm <- dfm(smallSet)
leasttok <- colnames(dfm_trim(smalldfm, max_termfreq = 9))
smalltok <- tokens_remove(smalltok, leasttok)
system.time(smallCol3 <- textstat_collocations(smalltok, size=c(2,3), min_count = 1))
# user  system elapsed 
# 976.988   0.444 489.094 
# 1-(489.094/1118.977)
# 0.5629097
## 56% decrease in time!

## now try parallelizing the filtered tokens!
clus <- makeCluster(2)
registerDoParallel(clus)
splittok <- list(smalltok[1:5000], smalltok[5001:10000])
system.time(smallCol4 <- parLapply(cl=clus, X=splittok, fun=textstat_collocations, size=c(2,3), min_count=1))
stopCluster(clus)
# user  system elapsed 
# 0.195   0.144  70.153
## this is the fastest solution! FIRST remove infrequent tokens then parallel collocations
