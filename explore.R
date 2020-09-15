## Exploratory Analysis
## Filtering & Processing
## Johns Hopkins University/Coursera Data Science Specialization
## Capstone Project
## R. Holley

library(quanteda)
library(parallel)
library(doParallel)
library(dplyr)
library(data.table)

## connections for each English file
con1 <- file("./final/en_US/en_US.news.txt")
con2 <- file("./final/en_US/en_US.blogs.txt")
con3 <- file("./final/en_US/en_US.twitter.txt")

## read in files, close connections, use filter, reshape by sentences
news <- corpus(readLines(con1, skipNul = TRUE))
close(con1)
nSent <- myFilter(news)
rm(news)

blogs <- corpus(readLines(con2, skipNul = TRUE))
close(con2)
bSent <- myFilter(blogs)
rm(blogs)

tweets <- corpus(readLines(con3, skipNul=TRUE))
close(con3)
twSent <- myFilter(tweets)
rm(tweets)

## all three batches of data cleaned & reshaped to sentences
## each a list of 5 corpii for processing
## now for collocations, bigrams and trigrams (n=2 and n=3)
## separately to test if my computer will die

nColl <- colloc(nSent, 2)
bColl <- colloc(bSent, 2)
twColl <- colloc(twSent, 2)

bigrams <- colloc_join(list(nColl, bColl, twColl))

## UPDATE: bigrams took ~30 hours on all 6 cores
## I borrowed a family member's nuch newer 16-thread desktop for a few days
## which took ~10 hours to process the same bigrams

## I don't think any programming class I have ever taken has specified if reusing
## ...variable names is some kind of sacrilege
## it feels wrong but no one has ever told me not to
nColl <- colloc(nSent, 3)
bColl <- colloc(bSent, 3)
twColl <- colloc(twSent, 3)

trigrams <- colloc_join(list(nColl, bColl, twColl))

## now the text data is fulling processing into 2 tables: bigrams and trigrams
## the web app will search through these tables to give the use predictions

##=========================================================
## BELOW IS PRACTICE/BRAINSTORMING CODE
## ENTER AT YOUR OWN RISK
##=========================================================

## #justNLPthings
cluster <- makeCluster(detectCores()-1)
registerDoParallel(cluster)

ntokens <- nTok(ntot) 

nloc1 <- textstat_collocations(ntokens[[1]], size=2)
nloc2 <- textstat_collocations(ntokens[[2]], size=2)
nloc3 <- textstat_collocations(ntokens[[3]], size=2)
nloc4 <- textstat_collocations(ntokens[[4]], size=2)
nloc5 <- textstat_collocations(ntokens[[5]], size=2)

stopCluster(cluster)
registerDoSEQ()


## this should probably be a function but I will do that later
cluster <- makeCluster(detectCores()-1)
registerDoParallel(cluster)

locs <- list(nloc1[,1:2], nloc2[,1:2], nloc3[,1:2], nloc4[,1:2], nloc5[,1:2])

combs <- locs[[1]]
foreach(i=2:5)%do%{
  combs <- full_join(combs, locs[[i]], by="collocation")
  combs[,2:3] <- nafill(combs[,2:3], fill=0)
  combs <- mutate(combs, count=(count.x+count.y), count.x=NULL, count.y=NULL)
}
## YAY
stopCluster(cluster)
registerDoSEQ()

## more testing stuff
system.time(shrtFilter(b1))

bit <- nSent[[1]][1:50]

tokens(bit, what="word", remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE) %>% tokens_ngrams(n=c(2,3))
more <- nSent[[1]][1:500]

bigMat <- nMat(nSent[[1]])

samp <- nSent[[1]]
length(samp)
ls <- round(length(samp)/3)
s1 <- samp[1:ls]; s2 <- samp[(ls+1):(2*ls)]; s3 <- samp[(2*ls+1):length(samp)]
chunks <- list(s1, s2, s3)

clus <- makeCluster(3)
registerDoParallel(clus)
trigrams <- parLapply(cl=clus, X=chunks, fun=textstat_collocations, size=3)
stopCluster(clus)
registerDoSEQ()

tri <- trigrams[[1]]
## combine collocations: add for identical pairs, 0 for no match, remove extraneous columns
foreach(i=2:3)%do%{
  tri <- full_join(tri[,1:2], trigrams[[i]][,1:2], by="collocation")
  tri[,2:3] <- nafill(tri[,2:3], fill=0)
  tri <- mutate(tri, count=(count.x+count.y), count.x=NULL, count.y=NULL)
}

### okay so... do trigrams for everything? start with just nSent (news)
nTri <- colloc3(nSent)

## testing data.table's merge vs. dplyr's full_join
small <- list(nSent[[1]][1:2000], nSent[[1]][2001:4000], nSent[[1]][4001:6000], nSent[[1]][6001:8000], nSent[[1]][8001:10000])
smColl <- colloc(small, n=2)
## colloc_join with full_join and mutate
dplyrTime <- system.time(colloc_join(smColl))
tableTime <- system.time(colloc_merge(smColl))
ts <- colloc_merge(smColl)

