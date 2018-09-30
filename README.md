# cachemeR

[![Build Status](https://travis-ci.org/Tazovsky/cachemeR.svg?branch=devel)](https://travis-ci.org/Tazovsky/cachemeR)
[![codecov](https://codecov.io/gh/Tazovsky/cachemeR/branch/devel/graph/badge.svg)](https://codecov.io/gh/Tazovsky/cachemeR)
[![Coverage Status](https://coveralls.io/repos/github/Tazovsky/cachemeR/badge.svg?branch=devel)](https://coveralls.io/github/Tazovsky/cachemeR?branch=devel)

## Overview

`cachemeR` is a convenient way of caching objects in R. 
From the beginning the purpose of this package is to make caching as easy as possible 
and to put as less effort as possible to implement it in existsing projects :)

## Installation


``` r
if (!require("devtools")) 
  install.packages("devtools")

devtools::install_github("Tazovsky/cachemeR@devel")
```

## Usage

Cache has to be initialized. It requires to run `cachemer$new` with path to `config.yaml`:

``` r
dir.create(tmp.dir <- tempfile())
on.exit(unlink(tmp.dir, TRUE, TRUE))
config.file <- file.path(tmp.dir, "config.yaml")
cache <- cachemer$new(path = config.file)
```


``` r
doLm <- function(rows, cols)

```

## Use cases

## Microbenchmark
