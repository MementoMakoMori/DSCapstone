## parallelization testing

library(future)
library(future.apply)
library(furrr)
library(doParallel)
library(quanteda)

ex <- cvCorp[1:1000]

## parallelization tests
## NOTE: textstat_collocations uses 2 threads, so workers are effectively doubled
plan(multisession, workers=6)
t1 <- system.time(fut.lapp <- future_lapply())
## with this type of backend, changing plan is necessary to close workers (and reallocate their memory)
plan(sequential)
plan(multisession, workers=6)
t2 <- system.time(fut.mapp <- future_mapply())
plan(sequential)
plan(multisession, workers=6)
t3 <- system.time(fur.mapp <- future_map())
plan(sequential)
clus <- makeCluster(6)
registerDoParallel(clus)
t4 <- system.time(par.l <- parLapply())
stopCluster(clus)
registerDoSEQ()
registerDoParallel(clus)
t5 <- system.time(par.m <- clusterMap())