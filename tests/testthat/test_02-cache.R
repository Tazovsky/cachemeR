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

testthat::test_that("test output: different ways to pass variables", {
  dir.create(tmp.dir <- tempfile())
  on.exit(unlink(tmp.dir, TRUE, TRUE))
  config.file <- file.path(tmp.dir, "config.yaml")
  cache <- cachemer$new(path = config.file)
  
  arg <- list(d = 3, e = 5)
  
  testthat::expect_error(x %c-% testFun(a = 1:20, b = 0, c = arg[["e"]]),
                         "\\$ operator is invalid for atomic vectors")
  
  arg <- list(d = 3, e = 55)
  z.ref <- testFun(a = 1:20, b = 0, c = arg)
  z %c-% testFun(a = 1:20, b = 0, c = arg)
  testthat::expect_equal(z, z.ref)
  
  e <- 56
  arg <- list(d = 3, e = e)
  y.ref <- testFun(a = 1:20, b = 0, c = arg)
  y %c-% testFun(a = 1:20, b = 0, c = arg)
  testthat::expect_equal(y.ref, y)
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

testthat::test_that("file is not yaml, does not exist, etc", {
  
  tmp.dir <- tempfile()
  
  cache <- cachemer$new(file.path(tmp.dir, "config.yaml"))
  
  testthat::expect_true(file.exists(file.path(tmp.dir, "config.yaml")))
  
  testthat::expect_error(
    cachemer$new(file.path(tmp.dir, "config.blabla")),
    "no 'yml' or 'yaml' extension"
  )
  
  testthat::expect_error(
    cachemer$new(file.path(tmp.dir, "config.yaml"), overwrite = FALSE),
    "File already exists"
  )
  
  res %c-% testFun()
  testthat::expect_equal(res, cache$getEnv$last.cache$output)
  
  testthat::expect_error(cache$setLogger(),
                         "'is\\.on' argument is missing")
})

testthat::test_that("yaml does not exist", {
  dir.create(tmp.dir <- tempfile())
  on.exit(unlink(tmp.dir, TRUE, TRUE))
  yaml.path <- file.path(tmp.dir, "config.yaml")
  cache <- cachemer$new(yaml.path)
  file.remove(yaml.path)
  testthat::expect_error(cachemer$new(), "Yaml file does not exist")
})
testthat::test_that("summary method: empty cache", {
  dir.create(tmp.dir <- tempfile())
  on.exit(unlink(tmp.dir, TRUE, TRUE))
  
  cache <- cachemer$new(file.path(tmp.dir, "config.yaml"))
  cache$clear()
  
  testthat::expect_is(cache$summary(), c("tbl_df", "tbl", "data.frame"))
  testthat::expect_equal(nrow(cache$summary()), 0)
  testthat::expect_is(cache$summary("data.table"), "data.table")
  testthat::expect_equal(nrow(cache$summary("data.table")), 0)
})

testthat::test_that("summary method", {
  
  testthat::skip_if(Sys.getenv("TRAVIS") == TRUE)
  
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

testthat::test_that("share method", {
  dir.create(tmp.dir <- tempfile())
  on.exit(unlink(tmp.dir, TRUE, TRUE))
  cache <- cachemer$new(file.path(tmp.dir, "config.yaml"))
  cache$clear()
  
  cache$share("iris", iris)
  cache2 <- cachemer$new(file.path(tmp.dir, "config.yaml"))
  
  testthat::expect_identical(cache2$getShared("iris"), iris)
  
  cache2$clear()
  
  testthat::expect_null(cache2$getShared("iris"))
})


testthat::test_that("all arguments are unnamed", {
  dir.create(tmp.dir <- tempfile())
  on.exit(unlink(tmp.dir, TRUE, TRUE))
  config.file <- file.path(tmp.dir, "config.yaml")
  cache <- cachemer$new(path = config.file)
  
  calcFun <- function(a, b) {
    (a+b) ^ (a*b)
  }
  
  cache$setLogger(TRUE)
  
  res %c-% calcFun(2, 3)
  
  testthat::expect_equal(res, 15625)
  
})