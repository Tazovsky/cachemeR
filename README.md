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
> INFO [2018-10-05 23:08:56] Logger is on

# cache function
result1 %c-% doLm(5, 5)
> INFO [2018-10-05 23:08:56] Caching 'doLm' for first time...
> [1] "Function is run"
result1
> 
> Call:
> lm(formula = y ~ X)
> 
> Coefficients:
> (Intercept)           X1           X2           X3           X4  
>       1.598        1.130        4.896        7.321        1.145  
>          X5  
>          NA

# function is cached now so if you re-run function then 
# output will be retrieved from cache instead of executing 'doLm' function again
result2 %c-% doLm(5, 5)
> INFO [2018-10-05 23:08:56] 'doLm' is already cached...
result2
> 
> Call:
> lm(formula = y ~ X)
> 
> Coefficients:
> (Intercept)           X1           X2           X3           X4  
>       1.598        1.130        4.896        7.321        1.145  
>          X5  
>          NA
```

Operator `%c-%` is sesitive to function name, function body, argument
(whether argument is named or not, or is list or not, or is declared in
parent environment, etc.). But it also **has some
[limitations](#limitations)**.

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

calc fibonacci

``` r

# fib5 <- calculateFibonacci(5)
# fib5 %c-% calculateFibonacci(5)
```
