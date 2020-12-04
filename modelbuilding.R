## R. Holley
## Organizing Data, Building a Model
## Data Science Certificate, JHU/Coursera

library(quanteda)

## read in massive text data
con1 <- file("./en_US/en_US.news.txt")
con2 <- file("./en_US/en_US.blogs.txt")
con3 <- file("./en_US/en_US.twitter.txt")

## combine text to one corpus
fullCorp <- corpus(c(readLines(con1, skipNul = TRUE),readLines(con2, skipNul = TRUE),readLines(con3, skipNul=TRUE)))
close(con1); close(con2); close(con3)

fullCorp <- readRDS("./fullCorp")

## randomly subset data by elements: 80% for training, 15% for cross-validation, 5% for testing
set.seed(777)
train <- sample(1:length(fullCorp), size=(length(fullCorp)*0.8), replace=FALSE)
trainCorp <- fullCorp[train]
remain <- fullCorp[-train]
cv <- sample(1:length(remain), size=length(fullCorp)*0.15, replace=FALSE)
cvCorp <- remain[cv]
testCorp <- remain[-cv]
## saveRDS(cvCorp, "./cvCorp")
## saveRDS(testCorp, "./testCorp")

## reshape training data so that each element ('document') is one sentence
trainCorp <- corpus_reshape(trainCorp, to="sentences") %>% tolower() %>% tokens(what = "word", remove_punct=TRUE, remove_symbols=TRUE, remove_numbers=TRUE, remove_url=TRUE, padding=TRUE)
## remove word tokens that appear <10 times
fewtoks <- dfm(trainCorp) %>% dfm_trim(max_termfreq = 9) %>% colnames()
filtCorp <- tokens_remove(trainCorp, fewtoks)

## create feature co-occurance matrix, counting how many times word B follows word A
fmat <- fcm(filtCorp, context="window", count="frequency", window=1, ordered=TRUE)
## saveRDS(fmat, "./trainFCM")   
## fmat <- readRDS("./trainFCM")

##the fcm is too big to process as one unit - it will use all the RAM and crash
## here I am splitting it into 10 chunks 
#### IMPORTANT ####
## the chunking will need to vary based on your available RAM!
begin <- 1
chunklen <- round(nrow(fmat)/10)
end <- chunklen
splitInd <- NULL
for(i in 1:10){
  splitInd[[i]] <- c(begin:end)
  begin <- begin + chunklen
  end <- end + chunklen
  if(i==9){end<-nrow(fmat)}
}

## probMat function takes lines from the fcm and converts the cell values to probabilities
probMat <- function(x){
  fullTab <- data.frame()
  line <- apply(x, 1, function(x){
    x/sum(x)
  })
  line <- as.data.frame(t(line))
  fullTab <- rbind(fullTab, line)
  return(fullTab)
}

## run probMat on each chunk and save the output
fooTable <- data.frame()
for(i in 1:10){
  fooTable <- rbind(fooTable, probMat(fmat[splitInd[[i]],]))
  gc()
  print(c("Completed chunk ", i))
  saveRDS(fooTable, paste0("./fooTable", i))
  write.table(fooTable, "./objTable", col.names = FALSE, append=TRUE)
}


## convert each line of fcm probabilities from a matrix/table format into a list
## the list of words is 'dict' short for dictionary
## not to be confused with the python structure 'dictionary'

for(i in 1:10){
  chunk <- readRDS(paste0("./fooTable", i))
  newObj <- NULL
  for(j in 1:nrow(chunk)){
    newObj[[j]] <- as.list(chunk[j,])
    newObj[[j]] <- newObj[[j]][-which(newObj[[j]]==0)]
  }
  names(newObj) <- rownames(chunk)
  saveRDS(newObj, file=paste0("dict", i))
  rm(chunk, newObj)
}

## use readRDS to load each dicti file into the environment
## dicti <- readRDS("./dicti") etc etc

dictBig <- c(dict1, dict2, dict3, dict4, dict5, dict6, dict7, dict8, dict9, dict10)

## the web app only displays up to 15 results, so organize results by highest probability
## and remove results past 15
trimDict <- function(l){
  ln <- lapply(l, length)
  l <- l[-which(ln<1)]
  x<-NULL
  for(i in 1:length(l)){
    x[[i]] <- sort(unlist(l[[i]]), decreasing=TRUE)
    if(length(x[[i]]>15)){
      x[[i]] <- x[[i]][1:15]
    }
  }
  names(x) <- names(l)
  return(x)
}

dictSmall <- trimDict(dictBig)

## save the final result, and you're done model building! 
##The next step is cross-validation, in the file CVTest.R and testScript.R
saveRDS(dictSmall, "./appdict")