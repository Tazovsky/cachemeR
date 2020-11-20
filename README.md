
# cachemeR

[![Build
Status](https://travis-ci.org/Tazovsky/cachemeR.svg?branch=devel)](https://travis-ci.org/Tazovsky/cachemeR)
[![codecov](https://codecov.io/gh/Tazovsky/cachemeR/branch/devel/graph/badge.svg)](https://codecov.io/gh/Tazovsky/cachemeR)
[![Coverage
Status](https://coveralls.io/repos/github/Tazovsky/cachemeR/badge.svg?branch=devel)](https://coveralls.io/github/Tazovsky/cachemeR?branch=devel)

## Overview

`cachemeR` is a convenient way of caching functions in R. From the
beginning the purpose of this package is to make caching as easy as
possible and to put as less effort as possible to implement it in
existsing projects :)

## Installation

``` r
if (!require("devtools")) 
  install.packages("devtools")

devtools::install_github("Tazovsky/cachemeR@devel")
```

## Usage - `%c-%` operator

Cache has to be initialized. It requires to run `cachemer$new` with path
to `config.yaml`. Then all you need to cache is to use pipe **`%c-%`**
instead of assignment operator `<-`. In following example function
`doLm` fits linear model:

``` r
library(cachemeR)
> Loading required package: R6
> Loading required package: futile.logger

doLm <- function(rows, cols, verbose = TRUE) {
  if (verbose)
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
> INFO [2020-11-11 15:53:56] Logger is on

# cache function
result1 %c-% doLm(5, 5)
> INFO [2020-11-11 15:53:56] Caching 'doLm' for first time...
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
> INFO [2020-11-11 15:53:58] 'doLm' is already cached...
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
> INFO [2020-11-11 15:53:58] Clearing leftovers in cache

testFun <- function(a, b) {
  (a+b) ^ (a*b)
}

cache$setLogger(TRUE)
> INFO [2020-11-11 15:53:58] Logger is on

result1 %c-% testFun(a = 2, b = 3)
> INFO [2020-11-11 15:53:58] Caching 'testFun' for first time...

testFun <- function(a, b) {
  (a+b) / (a*b)
}

# function name didn't change, but function body did so it will be cached:
result2 %c-% testFun(a = 2, b = 3)
> INFO [2020-11-11 15:54:01] Caching 'testFun' for first time...

result1
> [1] 15625
result2
> [1] 0.8333333
```

## Share elements across all instances of a class

``` r
library(cachemeR)

dir.create(tmp.dir <- tempfile())
config.file <- file.path(tmp.dir, "config.yaml")
on.exit(unlink(tmp.dir, TRUE, TRUE), add = TRUE)
cache <- cachemer$new(path = config.file)
> INFO [2020-11-11 15:54:04] Clearing leftovers in cache

cache$setLogger(TRUE)
> INFO [2020-11-11 15:54:04] Logger is on

lm_fit <- function(rows, cols) {
  print("Fitting linear model...")
  set.seed(1234)
  X <- matrix(rnorm(rows*cols), rows, cols)
  b <- sample(1:cols, cols)
  y <- runif(1) + X %*% b + rnorm(rows)
  model <- lm(y ~ X)
}

fun3 <- function() {
  print("> fun3")
  y %c-% lm_fit(123, 123)
  return(y)
}
fun2 <- function() {
  print("> fun2")
  fun3()
}
fun1 <- function() {
  print("> fun1")
  fun2()
}

x %c-% lm_fit(123, 123) # cache function on the toppest level
> INFO [2020-11-11 15:54:04] Caching 'lm_fit' for first time...
> [1] "Fitting linear model..."
x2 %c-% fun1()
> INFO [2020-11-11 15:54:06] Caching 'fun1' for first time...
> [1] "> fun1"
> [1] "> fun2"
> [1] "> fun3"
> INFO [2020-11-11 15:54:06] 'lm_fit' is already cached...
identical(x, x2)
> [1] TRUE
```

But it also **has some [limitations](#limitations)**.

## Use cases

1.  shiny app example
2.  calculate Fibonacci
3.  ?

## Limitations

Generally `cachemeR` is designed to cache R functions only. And it won’t
work if:

  - Cached function’s argument value contains `$` or `[[]]`:

<!-- end list -->

``` r
args = list(a = 1, b = 2)
res %c-% fun(a = args$a, b = args[["b"]])
```

  - You want to cache something else than function only:

<!-- end list -->

``` r

res %c-% fun(a = 1, b = 2) + 1

# or expression in parenthesis:
res %c-% (fun(a = 1, b = 2) + 1)
res %c-% {fun(a = 1, b =2) + 1}

# or simple value/variable:
arg <- 1
res %c-% arg
```

  - You want to use it with `magrittr` pipes, for example with `%>%`:

<!-- end list -->

``` r
getDF <- function(nm) {
  switch(nm,
         iris = iris,
         mtcars = mtcars)
}

library(dplyr)
res %c-% getDF("iris") %>% summary()
```

## Microbenchmark

``` r
# microbenchmark
cache <- cachemer$new(path = config.file)
> INFO [2020-11-11 15:54:12] Restored cache (2) elements
cache$setLogger(FALSE)

test_no_cache <- function(n) {
  result_no_cache <- doLm(n, n, verbose = FALSE)
}

test_cache <- function(n) {
  result_no_cache %c-%  doLm(n, n, verbose = FALSE)
}

res1 <- microbenchmark::microbenchmark(
  test_no_cache(400),
  test_cache(400)
)

res1
> Unit: milliseconds
>                expr      min        lq     mean    median        uq        max
>  test_no_cache(400) 28.56172 31.592854 34.91320 33.789533 37.368221   47.71753
>     test_cache(400)  3.59404  3.890445 35.32929  4.392523  5.054555 3067.23224
>  neval
>    100
>    100

# but now just try it assuming the calculation has been already cached
res2 <- microbenchmark::microbenchmark(
  test_no_cache(400),
  test_cache(400)
)

res2
> Unit: milliseconds
>                expr       min        lq      mean   median        uq       max
>  test_no_cache(400) 27.729712 30.510635 33.580129 32.26039 35.025328 85.789598
>     test_cache(400)  3.583999  4.072419  4.730182  4.56728  5.067277  7.930751
>  neval
>    100
>    100
```

# Dev environment

Package is developed in RStudio run in container:

``` bash
# R 3.6.1
docker build -f Dockerfile-R3.6.1 -t kfoltynski/cachemer:3.6.1 .
# or R 4.0.0
docker build -f Dockerfile-R4.0.0 -t kfoltynski/cachemer:4.0.0 .

# run container with RStudio listening on 8790

# R 3.6.1
docker run -d --name cachemer --rm -v $(PWD):/mnt/vol -w /mnt/vol -p 8790:8787 kfoltynski/cachemer:3.6.1
# R 4.0.0
docker run -d --name cachemer --rm -v $(PWD):/mnt/vol -w /mnt/vol -p 8790:8787 kfoltynski/cachemer:4.0.0
```
