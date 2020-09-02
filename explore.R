##Exploratory Analysis
##Johns Hopkins University/Coursera Data Science Specialization
##Capstone Project
##R. Holley

library(tm)
library(quanteda)
library(parallel)
library(doParallel)
library(dplyr)
library(ngram)
library(data.table)


##read in massive data
##plz don't crash my PC
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

##connections for each English file
con1 <- file("./final/en_US/en_US.news.txt")
con2 <- file("./final/en_US/en_US.blogs.txt")
con3 <- file("./final/en_US/en_US.twitter.txt")

#blogs <- corpus(readLines(con2, skipNul=TRUE))

news <- corpus(readLines(con1, skipNul = TRUE))
nwc <- wordcount(readLines(con1, skipNul = TRUE))

close(con1)
ntot <- myFilter(news)
rm(news)

#tweets <- corpus(readLines(con3, skipNul=TRUE))

##INSERT HERE calls to protoFilter
##shrtFilter will do the following
## -remove non-ACSII characters
## -replace contractions with full words
## -break corpii into chunks for parallel processing
## -tokenize by sentences -> remove punctuation -> tokenize by word
## -create ngrams!!??


# btot <- myFilter(blogs)
# rm(blogs)
# twtot <- myFilter(tweets)
# rm(tweets)

##WEEK 2
# Questions to consider:
# Some words are more frequent than others - what are the distributions of word frequencies?
#   
# What are the frequencies of 2-grams and 3-grams in the dataset?
#   
# How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?
#   
# How do you evaluate how many of the words come from foreign languages?
#   
# Can you think of a way to increase the coverage -- identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases?
stopCluster(cluster)
registerDoSEQ()


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


##this should probably be a function but w/e
cluster <- makeCluster(detectCores()-1)
registerDoParallel(cluster)

locs <- list(nloc1[,1:2], nloc2[,1:2], nloc3[,1:2], nloc4[,1:2], nloc5[,1:2])
foreach(i=1:5)%do%{
  locs[[i]][,2] <- nafill(locs[[i]][,2], fill=0)
}

combs <- locs[[1]]
foreach(i=2:5)%do%{
  combs <- full_join(combs, locs[[i]], by="collocation")
  combs[,2:3] <- nafill(combs[,2:3], fill=0)
  combs <- mutate(combs, count=(count.x+count.y), count.x=NULL, count.y=NULL)
}
##YAY
stopCluster(cluster)
registerDoSEQ()

#testing stuff
system.time(shrtFilter(b1))





