cachemeR
========

[![Build
Status](https://travis-ci.org/Tazovsky/cachemeR.svg?branch=devel)](https://travis-ci.org/Tazovsky/cachemeR)
[![codecov](https://codecov.io/gh/Tazovsky/cachemeR/branch/devel/graph/badge.svg)](https://codecov.io/gh/Tazovsky/cachemeR)
[![Coverage
Status](https://coveralls.io/repos/github/Tazovsky/cachemeR/badge.svg?branch=devel)](https://coveralls.io/github/Tazovsky/cachemeR?branch=devel)

Overview
--------

`cachemeR` is a convenient way of functions in R. From the beginning the
purpose of this package is to make caching as easy as possible and to
put as less effort as possible to implement it in existsing projects :)

Installation
------------

``` r
if (!require("devtools")) 
  install.packages("devtools")

devtools::install_github("Tazovsky/cachemeR@devel")
```

Usage - `%c-%` operator
-----------------------

Cache has to be initialized. It requires to run `cachemer$new` with path
to `config.yaml`. Then all you need to cache is to use pipe **`%c-%`**
instead of assignment operator `<-`. In following example function
`doLm` fits linear model:

``` r
library(cachemeR)
> Loading required package: R6
> Loading required package: futile.logger

doLm <- function(rows, cols) {
  print("Function is run")
  set.seed(1234)
  X <- matrix(rnorm(rows*cols), rows, cols)
  b <- sample(1:cols, cols)
  y <- runif(1) + X %*% b + rnorm(rows)
  model <- lm(y ~ X)
}

# create dir where cache will be stored
dir.create(tmp.dir <- tempfile())

# create path to config.yaml - yaml fild must be in that dir
config.file <- file.path(tmp.dir, "config.yaml")

# initialize cachemeR
cache <- cachemer$new(path = config.file)

cache$setLogger(TRUE)
> INFO [2020-11-10 20:47:14] Logger is on

# cache function
result1 %c-% doLm(5, 5)
> INFO [2020-11-10 20:47:14] Caching 'doLm' for first time...
> [1] "Function is run"
result1
> 
> Call:
> lm(formula = y ~ X)
> 
> Coefficients:
> (Intercept)           X1           X2           X3           X4           X5  
>      -1.632        2.595        5.338       -1.247        2.907           NA

# function is cached now so if you re-run function then 
# output will be retrieved from cache instead of executing 'doLm' function again
result2 %c-% doLm(5, 5)
> INFO [2020-11-10 20:47:19] 'doLm' is already cached...
result2
> 
> Call:
> lm(formula = y ~ X)
> 
> Coefficients:
> (Intercept)           X1           X2           X3           X4           X5  
>      -1.632        2.595        5.338       -1.247        2.907           NA
```

Operator `%c-%` is sesitive to function name, function body, argument
(whether argument is named or not, or is list or not, or is declared in
parent environment, etc.):

``` r
library(cachemeR)

dir.create(tmp.dir <- tempfile())
config.file <- file.path(tmp.dir, "config.yaml")
cache <- cachemer$new(path = config.file)
> INFO [2020-11-10 20:47:19] Clearing leftovers in cache

testFun <- function(a, b) {
  (a+b) ^ (a*b)
}

cache$setLogger(TRUE)
> INFO [2020-11-10 20:47:19] Logger is on

result1 %c-% testFun(a = 2, b = 3)
> INFO [2020-11-10 20:47:19] Caching 'testFun' for first time...

testFun <- function(a, b) {
  (a+b) / (a*b)
}

# function name didn't change, but function body did so it will be cached:
result2 %c-% testFun(a = 2, b = 3)
> INFO [2020-11-10 20:47:24] Caching 'testFun' for first time...

result1
> [1] 15625
result2
> [1] 0.8333333
```

But it also **has some [limitations](#limitations)**.

Use cases
---------

1.  shiny app example
2.  calculate Fibonacci
3.  ?

Limitations
-----------

Generally `cachemeR` is designed to cache R functions only. And it won’t
work if:

-   Cached function’s argument value contains `$` or `[[]]`:

``` r
args = list(a = 1, b = 2)
res %c-% fun(a = args$a, b = args[["b"]])
```

-   You want to cache something else than function only:

``` r

res %c-% fun(a = 1, b = 2) + 1

# or expression in parenthesis:
res %c-% (fun(a = 1, b = 2) + 1)
res %c-% {fun(a = 1, b =2) + 1}

# or simple value/variable:
arg <- 1
res %c-% arg
```

-   You want to use it with `magrittr` pipes, for example with `%>%`:

``` r
getDF <- function(nm) {
  switch(nm,
         iris = iris,
         mtcars = mtcars)
}

library(dplyr)
res %c-% getDF("iris") %>% summary()
```

Microbenchmark
--------------

``` r
# microbenchmark
cache <- cachemer$new(path = config.file)
> INFO [2020-11-10 20:47:28] Clearing leftovers in cache
cache$setLogger(FALSE)

test_no_cache <- function(n) {
  result_no_cache <- doLm(n, n)
}

test_cache <- function(n) {
  result_no_cache %c-%  doLm(n, n)
}

res <- microbenchmark::microbenchmark(
  test_no_cache(400),
  test_cache(400)
)
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"
> [1] "Function is run"

res
> Unit: milliseconds
>                expr       min        lq     mean    median        uq        max
>  test_no_cache(400) 46.524587 49.168289 55.71481 52.074472 59.824559   98.21499
>     test_cache(400)  1.799063  2.184755 52.62374  2.437088  2.825046 4998.70218
>  neval
>    100
>    100
```

Dev environment
===============

Package is developed in RStudio run in container:

``` bash
docker build -t cachemer .
docker run --rm -v $(PWD):/mnt/vol -d -p 8787:8787 -it cachemer:latest
```
