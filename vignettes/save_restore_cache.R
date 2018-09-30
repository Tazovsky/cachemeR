## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = ">"
)

## ------------------------------------------------------------------------
library(cachemeR)

# create cache location
dir.create(cache.dir <- tempfile())
config.file <- file.path(cache.dir, "config.yaml")
print(config.file)
cache <- cachemer$new(path = config.file)

cache$setLogger(TRUE)

start.t <- Sys.time()
res1 %c-% testFun(1:23, b = 1, list(d = 2, e = 3))
res2 %c-% testFun(1:50, b = 2, list(d = 4, e = 5))
fib %c-% calculateFibonacci(50000)
t1 <- Sys.time() - start.t
t1

# list cached objects:
list.files(cache.dir)

cache$clear()


## ------------------------------------------------------------------------
library(cachemeR)

print(config.file)

# list cached objects:
list.files(cache.dir)

cache <- cachemer$new(path = config.file)

start.t <- Sys.time()
res1 %c-% testFun(1:23, b = 1, list(d = 2, e = 3))
res2 %c-% testFun(1:50, b = 2, list(d = 4, e = 5))
fib %c-% calculateFibonacci(50000)
t2 <- Sys.time() - start.t
t2



