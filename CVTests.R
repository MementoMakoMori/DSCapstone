## R. Holley
## Cross-validation testing
## Data Science Certificate, JHU/Coursera

cvCorp <- readRDS("cvCorp")
fullTable <- readRDS("combTable")
uniq <- readRDS("uniq")

#uniq <- unique(unlist(c(trainTable$n1, trainTable$n2)))
# cvCorp <- corpus_reshape(cvCorp, to="sentences") %>% tolower() %>% tokens(what = "word", remove_punct=TRUE, remove_symbols=TRUE, remove_numbers=TRUE, remove_url=TRUE, padding=TRUE)

system.time(cvModel <- textstat_collocations(ex, size=c(2,3), min_count=3))
cvModel <- textstat_collocations(cvCorp, size=c(2,3), min_count=3)
