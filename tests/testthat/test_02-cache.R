testthat::context("pipe output")

testthat::test_that("init cache object", {
  testthat::expect_true ({
    dir.create(tmp.dir <- tempfile())
    on.exit(unlink(tmp.dir, TRUE, TRUE))
    config.file <- file.path(tmp.dir, "config.yaml")
    cache <- cachemer$new(path = config.file)
    TRUE
  })
})

testthat::test_that("test output: all args are named", {
  dir.create(tmp.dir <- tempfile())
  on.exit(unlink(tmp.dir, TRUE, TRUE))
  config.file <- file.path(tmp.dir, "config.yaml")
  cache <- cachemer$new(path = config.file)
  
  ref.res1 <- testFun(a = 1:20, b = 0, c = list(d = 3, e = 5))
  res1 %c-% testFun(a = 1:20, b = 0, c = list(d = 3, e = 5))
  
  testthat::expect_equal(ref.res1, res1)
})

testthat::test_that("test output: named and unnamed args", {
  dir.create(tmp.dir <- tempfile())
  on.exit(unlink(tmp.dir, TRUE, TRUE))
  config.file <- file.path(tmp.dir, "config.yaml")
  cache <- cachemer$new(path = config.file)
  
  ref.res2 <- testFun(1:13, b = 2, list(d = 3, e = 5))
  res2 %c-% testFun(1:13, b = 2, list(d = 3, e = 5))
  
  testthat::expect_equal(ref.res2, res2)
})

testthat::test_that("test output: all args are default", {
  dir.create(tmp.dir <- tempfile())
  on.exit(unlink(tmp.dir, TRUE, TRUE))
  config.file <- file.path(tmp.dir, "config.yaml")
  cache <- cachemer$new(path = config.file)
  
  ref.res3 <- testFun()
  res3 %c-% testFun()
  
  testthat::expect_equal(ref.res3, res3)
})

testthat::test_that("test output: all args are unnamed", {
  dir.create(tmp.dir <- tempfile())
  on.exit(unlink(tmp.dir, TRUE, TRUE))
  config.file <- file.path(tmp.dir, "config.yaml")
  cache <- cachemer$new(path = config.file)
  
  ref.res4 <- testFun(1:77, 3, list(d = 4, e = 2))
  res4 %c-% testFun(1:77, 3, list(d = 4, e = 2))
  
  testthat::expect_equal(ref.res4, res4)
})

testthat::context("test R6 object and methods")

testthat::test_that("method: lastCache", {
  
  testthat::skip_if(Sys.getenv("TRAVIS") == TRUE)

  dir.create(tmp.dir <- tempfile())
  on.exit(unlink(tmp.dir, TRUE, TRUE))
  config.file <- file.path(tmp.dir, "config.yaml")
  cache <- cachemer$new(path = config.file)
  
  ref.res <- testFun(a = 1:100, 1, list(d = 2, e = 3))
  res %c-% testFun(a = 1:100, 1, list(d = 2, e = 3))
  
  cache <- cachemerRef$new()
  
  testthat::expect_equal(ref.res, cache$lastCache$output)
  testthat::expect_equal(list(a = 1:100, b = 1, c =list(d = 2, e = 3)),
                         cache$lastCache$arguments)
  
  testthat::expect_equal("6909e86c41e10a11431fb13433767de4",
                         cache$lastCache$hashes$fun.name)
  
  testthat::expect_equal("c3783d8b264c9c5addfb001af37976f8",
                         cache$lastCache$hashes$fun.body)
  
  testthat::expect_equal("f5de5e744f53b90b28a3a3cce3e23114",
                         cache$lastCache$hashes$arguments)
  
  testthat::expect_equal("6446ef4dc350c0f6418be1b91f6b2d9f",
                         cache$lastCache$hash)
})


testthat::test_that("argument value is named variable", {
  dir.create(tmp.dir <- tempfile())
  on.exit(unlink(tmp.dir, TRUE, TRUE))
  config.file <- file.path(tmp.dir, "config.yaml")
  cache <- cachemer$new(path = config.file)
  
  x <- 1:123
  y <- 13
  z <- list(d = 11, e = 22)
  
  ref.res <- testFun(a = x, 1, z)
  res %c-% testFun(a = x, 1, z)
  
  testthat::expect_equal(ref.res, res)
  
  ref.res <- testFun(a = x, 7, z)
  res %c-% testFun(a = x, 7, z)
  
  testthat::expect_equal(ref.res, res)
  
})

testthat::test_that("argument value is named LIST variable", {
  dir.create(tmp.dir <- tempfile())
  on.exit(unlink(tmp.dir, TRUE, TRUE))
  config.file <- file.path(tmp.dir, "config.yaml")
  cache <- cachemer$new(path = config.file)
  
  x <- 1:123
  y <- 13
  z <- list(x = list(d = 11, e = 22))
  
  ref.res <- testFun(a = x, 3, z$x)
  res %c-% testFun(a = x, 3, z$x)
  
  testthat::expect_equal(ref.res, res)
  
  # change value in list to be sure it will react to this change
  z <- list(x = list(d = 3, e = 2))
  ref.res <- testFun(a = x, 3, z$x)
  res %c-% testFun(a = x, 3, z$x)
  testthat::expect_equal(ref.res, res)
  
  x <- 1:12
  y <- list(value = list(finally = 4))
  z <- list(x = list(d = 11, e = 22))
  
  ref.res <- testFun(a = x, y$value$finally, z$x)
  res %c-% testFun(a = x, y$value$finally, z$x)
  testthat::expect_equal(ref.res, res)
})

testthat::test_that("argument value is named NESTED LIST variable", {
  dir.create(tmp.dir <- tempfile())
  on.exit(unlink(tmp.dir, TRUE, TRUE))
  config.file <- file.path(tmp.dir, "config.yaml")
  cache <- cachemer$new(path = config.file)
  
  x <- 1:12
  y <- list(value = list(finally = 4))
  z <- list(x = list(d = 11, e = 22))
  
  ref.res <- testFun(a = x, y$value$finally, z$x)
  res %c-% testFun(a = x, y$value$finally, z$x)
  testthat::expect_equal(ref.res, res)
})


testthat::test_that("dollar '$' support for data frames", {
  dir.create(tmp.dir <- tempfile())
  on.exit(unlink(tmp.dir, TRUE, TRUE))
  config.file <- file.path(tmp.dir, "config.yaml")
  cache <- cachemer$new(path = config.file)
  
  df <- iris
  df$Sepal.Length2 <- df$Sepal.Length + 1
  
  testthat::expect_error(df$Sepal.Length2 %c-% df$Sepal.Length, 
                         regexp = "`\\$` is not supported")
})

testthat::test_that("dollar '$' support for LHS", {
  dir.create(tmp.dir <- tempfile())
  on.exit(unlink(tmp.dir, TRUE, TRUE))
  config.file <- file.path(tmp.dir, "config.yaml")
  cache <- cachemer$new(path = config.file)
  
  res.ref <- list(value = NULL)	
  res <- list(value = NULL)	
  
  x <- 1:12	
  y <- list(value = list(finally = 1))	
  z <- list(x = list(d = 2, e = 3))	
  
  res.ref$value <- testFun(a = x, y$value$finally, z$x)	
  res$value %c-% testFun(a = x, y$value$finally, z$x)	
  
  testthat::expect_equal(res.ref$value, res$value)	
})

testthat::test_that("simple value assignment", {
  
  testthat::expect_error(res %c-% 1, regexp = "not a function")
  
  var <- 2
  testthat::expect_error(res %c-% var, regexp = "not a function")
})

testthat::test_that("clear method", {
  dir.create(tmp.dir <- tempfile())
  on.exit(unlink(tmp.dir, TRUE, TRUE))
  
  cache <- cachemer$new(file.path(tmp.dir, "config.yaml"))
  
  res1 %c-% testFun(a = 1:77, b = 7, c = list(d = 7, e = 7))
  
  hash <- cache$lastCache$hash
  
  cache$clear()
  
  testthat::expect_null(cache$lastCache)
  
  x <- cachemerRef$new()
  
  res2 %c-% testFun(a = 1:77, b = 7, c = list(d = 7, e = 7))
  
  testthat::expect_equal(hash, x$lastCache$hash)
  
  cache$clear(all = TRUE)
  
  testthat::expect_error(cachemerRef$new(), regexp = "Missing 'path' argument")
})

testthat::test_that("setLogger method", {
  dir.create(tmp.dir <- tempfile())
  on.exit(unlink(tmp.dir, TRUE, TRUE))
  cache <- cachemer$new(file.path(tmp.dir, "config.yaml"))
  
  cache$setLogger(TRUE)
  
  testthat::expect_output(cache$setLogger(TRUE), "Logger is on")
  
  testthat::expect_output(
    res1 %c-% testFun(a = 1:77, b = 7, c = list(d = 7, e = 7)),
    "Caching 'testFun' for first time")
  
  cache$setLogger(FALSE)
  
  testthat::expect_output(
    res1 %c-% testFun(a = 1:77, b = 7, c = list(d = 7, e = 7)),
    NA)
  
})


testthat::test_that("summary method", {
  
  dir.create(tmp.dir <- tempfile())
  on.exit(unlink(tmp.dir, TRUE, TRUE))
  
  cache <- cachemer$new(file.path(tmp.dir, "config.yaml"))
  
  cache$clear()
  
  for (i in 1:22)
    res %c-% testFun(a = 1:22, b = i, c = list(d = i / 2, e = i/3))
  
  testthat::expect_equal(
    cache$summary(),
    readRDS(system.file("testdata", "summary_tbl.RDS", package = "cachemeR")))
  
  testthat::expect_equal(
    cache$summary("data.table"),
    readRDS(system.file("testdata", "summary_dt.RDS", package = "cachemeR")))
  
  # recreate reference objects
  if (FALSE) {
    saveRDS(cache$summary(), file = "inst/testdata/summary_tbl.RDS")
    saveRDS(cache$summary("data.table"), file = "inst/testdata/summary_dt.RDS")
  }
  
})