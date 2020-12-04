## R. Holley
## Cross-validation testing
## Data Science Certificate, JHU/Coursera

## What is cross-validation? 
## Checking that the patterns identified and modeled in the training set hold true in a different sample of the population
## If the patterns fit within an acceptable range of error, the trained model is good!
## If the patterns do not fit:
## * Check for outliers affecting data
## * Consider different model weights
## * Consider different method of model-building

## Because my current test method only includes bigrams, I am expecting a fairly low accuracy rate.
## My goal is at least 25% accuracy for all words in the cross-validation set
## and at least 35% accuracy for just words in my dictionary

source('testScript.R')

library(quanteda)
library(future.apply)

cvCorp <- readRDS("cvCorp")
dict <- readRDS("appdict")

cvCorp <- corpus_reshape(cvCorp, to="sentences") %>% tolower() %>% tokens(what = "word", remove_punct=TRUE, remove_symbols=TRUE, remove_numbers=TRUE, remove_url=TRUE, padding=TRUE)

## For cross-validation, I first want to check the accuracy of *words in my dictionary*
## giving myself the generous margin of the entire dictionary entry (up to 15 results) to check for correct prediction

library(future.apply)
cvTest <- tokens_keep(cvCorp, names(dict), padding=TRUE)
cvTest <- cvTest[-which(lapply(cvCorp, length)<=1)]

plan(multisession, workers=10)
system.time(acc1 <- future_lapply(X=cvTest, FUN=eval, future.seed=2020))
plan(sequential)
full <- unlist(acc1)
print(paste0("Percentage correct: ", sum(full)/length(full)))
# [1] "Percentage correct: 0.438926414103778
## that is honestly much higher than I was expecting!

## I'll run the test again without unknown words removed

cvTest <- cvCorp[-which(lapply(cvCorp, length)<=1)]
plan(multisession, workers=10)
system.time(acc2 <- future_lapply(X=cvTest, FUN=eval, future.seed=2020))
plan(sequential)
full <- unlist(acc2)
print(paste0("Percentage correct: ", sum(full)/length(full)))
# [1] "Percentage correct: 0.427101329048248"
## that's still way better than I was expecting, so I will not make major changes to my model-building


## I'd like to know how many words in the cross-validation are missing from my dictionary
cvuniq <- unique(unlist(cvCorp)) ## length: 286764
trainuniq <- unique(names(dict)) ## length: 96719
dif <- cvuniq[-which(cvuniq %in% trainuniq)] ##length: 194739!

## eval function returns FALSE if an input word is unknown
## can I improve accuracy by providing a result for unknown words?
## function eval2 contains an if statement for unknown words - the last known word is used as input instead
## however if the unknown input word is the first word in the sentence, the result is still FALSE
plan(multisession, workers=10)
system.time(acc3 <- future_lapply(X=cvTest, FUN=eval2, future.seed=2020))
plan(sequential)
full <- unlist(acc3)
print(paste0("Percentage correct: ", sum(full)/length(full)))
# [1] "Percentage correct: 0.428328152510526"
## 1/10 of a percentage point improvement! It's small, but I'll take it.

## Moving on to test set
## The function test is identical to eval2 except it includes a required argument lg
## lg is how many predictions to return
## if the length of prediction is shorter than lg (e.g. lg=10, but length(pred)=8) then lg is reassigned to length(pred)
## in eval and eval2, prediction returned ALL possible words up to the max dicionary entry length 15

testCorp <- readRDS("./testCorp")
testCorp <- corpus_reshape(testCorp, to="sentences") %>% tolower() %>% tokens(what = "word", remove_punct=TRUE, remove_symbols=TRUE, remove_numbers=TRUE, remove_url=TRUE, padding=TRUE)
testSet <- testCorp[-which(lapply(testCorp, length)<=1)]

## first running the test with lg=15 (the max length) which should yield accuracy similar to CV test of 42.7%
plan(multisession, workers=10)
system.time(testAcc <- future_lapply(X=testSet, FUN=test, lg=15, future.seed=2020, future.chunk.size=(round(length(testSet)/10))))
plan(sequential)
full <- unlist(testAcc)
print(paste0("Percentage correct: ", sum(full)/length(full)))
# [1] "Percentage correct: 0.424370588018412"
## disappointing to see a drop in accuracy of ~0.4 points, but it is well within the expected range of the CV result
## the next test with lg=5 should yield a much lower percentage
plan(multisession, workers=10)
system.time(testAcc2 <- future_lapply(X=testSet, FUN=test, lg=5, future.seed=2020, future.chunk.size=(round(length(testSet)/10))))
plan(sequential)
full <- unlist(testAcc2)
print(paste0("Percentage correct: ", sum(full)/length(full)))
# [1] "Percentage correct: 0.29517580804199"
## this number is more inline with what I was initially expecting