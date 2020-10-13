## Exploratory Analysis
## Filtering & Processing
## Johns Hopkins University/Coursera Data Science Specialization
## Capstone Project
## R. Holley

library(quanteda)
library(quanteda.textmodels)
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

nColl <- colloc(nSent, 2, 4)
bColl <- colloc(bSent, 2, 4)
twColl <- colloc(twSent, 2, 4)

bigrams <- colloc_join(list(nColl, bColl, twColl))

## UPDATE: bigrams took ~30 hours on all 6 cores
## I borrowed a family member's nuch newer 16-thread desktop for a few days
## which took ~10 hours to process the same bigrams

## I don't think any programming class I have ever taken has specified if reusing
## ...variable names is some kind of sacrilege
## it feels wrong but no one has ever told me not to
nColl <- colloc(nSent, 3, 4)
bColl <- colloc(bSent, 3, 4)
twColl <- colloc(twSent, 3, 4)

trigrams <- colloc_join(list(nColl, bColl, twColl))

## now the text data is fully processed into 2 tables: bigrams and trigrams
## the web app will search through these tables to give the user predictions

## resizing data for useability
print(object.size(bigrams), units="auto")
# 68.6 Mb
print(object.size(trigrams), units="auto")
# 97 Mb

## the default minimum count for colloc data was 2
## I should change it to 3? 4? higher?
## % of bigram/trigram data is collocs of count 3 or fewer
(sum(bigrams$count<=3))/length(bigrams$count)
# 0.4759471
(sum(trigrams$count<=3))/length(trigrams$count)
# 0.5937799

## aka: a lot, so I'll subset
biSmall <- bigrams[which(bigrams$count>=4),]
triSmall <- trigrams[which(trigrams$count>=4),]

## check new sizes
print(object.size(biSmall), units="auto")
# 35.7 Mb
print(object.size(triSmall), units="auto")
# 40.6 Mb

##===================================================
## ACCURACY TESTING
##===================================================

## test text: 2013 US Presidential Inaugural Address
testFilt <- myFilter(data_corpus_inaugural[57])

## make bigram table
testCol2 <- colloc(testFilt, 2, 1)
testCol2 <- colloc_join(testCol2)

## make trigram table
testCol3 <- colloc(testFilt, 3, 1)
testCol3 <- colloc_join(testCol3)

## check test functions with tiny subset
## testCondFull checks against the (original, default) min_count=2 n-gram tables
## testCondSmall checks against the min_count=4 n-gram tables
small <- testCol3[1:10,]
start<-Sys.time()
testCondFull(small)
# 0.4
start - Sys.time()
# Time difference of -6.045904 secs

start2 <- Sys.time()
testCondSmall(small)
# 0.4
start2 - Sys.time()
# Time difference of -2.624081 secs


## Full test sets (testCol2 is bigrams, testCol3 is trigrams), on full and small bigram/trigram data
## this could take a while so I'll start with the faster ones
start0 <- Sys.time()
testCondSmall(testCol2)
# 0.2747642
start0 - Sys.time()
# Time difference of -5.377376 mins
## my poor slow processor

start1 <- Sys.time()
testCondSmall(testCol3)
# 0.1946855
start1 - Sys.time()
# Time difference of -8.106881 mins

start2 <- Sys.time()
testCondFull(testCol2)
# 0.2824292
start2 - Sys.time()
# Time difference of -9.763398 mins

start3 <- Sys.time()
testCondFull(testCol3)
# 0.2261388
start3 - Sys.time()
# Time difference of -19.0014 mins

## my best prediction acuracy rate is 28.2%
## that sucks!
## I'm going to try adding a co-occurence matrix, first with a small sample size
con1 <- file("./final/en_US/en_US.news.txt")
samp <- corpus(readLines(con1, n=5000, skipNul=TRUE)) %>% myFilter()
close(con1)
toks <- nTok(samp)
test <- dfm(as.tokens(toks)) #%>% textstat_simil(margin = "feature", min_simil = 0)
cooc <- fcm(test, context="document", count = "frequency")
## hold up, that fcm was CRAZY fast
## ... I need to use that more
bigrams <- fcm(as.tokens(toks), context = "window", count="frequency", window=1, ordered=TRUE)
bi <- convert(bigrams, to="data.frame")

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

## matching tests
inds <- grep("do you ", trigrams$collocation, ignore.case=TRUE)
head(setorder(trigrams[inds,], -count))


trigrams[grep("i would eat", trigrams$collocation),]


