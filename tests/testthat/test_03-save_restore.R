testthat::context("saveCache")

testthat::test_that("saveCache: arguments", {
  dir.create(tmp.dir <- tempfile())
  on.exit(unlink(tmp.dir, TRUE, TRUE))
  var <- list(a = 1, b = 2, c = list(e = 2, 241234))
  
  testthat::expect_error(
    saveCache(var, path = "dev/", sufix = "sufix"),
    "\\!missing\\(promises\\.env\\) is not TRUE"
  )
  
  env <- new.env()
  
  testthat::expect_error(
    saveCache(path = tmp.dir, sufix = "sufix", promises.env = env),
    "\\!missing\\(x\\) is not TRUE"
  )
  
  testthat::expect_error(
    saveCache(path = tmp.dir, sufix = "sufix", promises.env = env),
    "\\!missing\\(x\\) is not TRUE"
  )
  
  testthat::expect_error(
    saveCache(var, sufix = "sufix", promises.env = env),
    "\\!missing\\(path\\) is not TRUE"
  )
  
  saveCache(var, path = tmp.dir, sufix = "sufix", promises.env = env)
  
  # 2nd save is to force evaluate promise
  saveCache(var, path = tmp.dir, sufix = "sufix", promises.env = env)
  
  # 3rd is to catch error because we are sure file already exists after 2nd step
  testthat::expect_error(
    saveCache(var, path = tmp.dir, sufix = "sufix", promises.env = env),
    "!file.exists\\(file.path\\(path, fname\\)\\) is not TRUE"
  )
  
  testthat::expect_true(
    file.exists(file.path(tmp.dir, paste0("cachemer", "_", "sufix", ".rds")))
  )
  
})

testthat::context("restoreCache")

testthat::test_that("restoreCache: returned object", {
  dir.create(tmp.dir <- tempfile())
  on.exit(unlink(tmp.dir, TRUE, TRUE))
  config.file <- file.path(tmp.dir, "config.yaml")
  cache <- cachemer$new(path = config.file)
  
  res1 %c-% testFun(1:23, b = 1, list(d = 2, e = 3))
  res2 %c-% testFun(1:23, b = 2, list(d = 2, e = 3))
  res3 %c-% testFun(1:23, b = 3, list(d = 2, e = 3))
  
  restored <- restoreCache(tmp.dir)
  
  testthat::expect_equal(
    names(restored), 
    as.vector(sapply(restored, function(x) x$hash))
  )
})

testthat::test_that("saveCache: restore session", {
  dir.create(tmp.dir <- tempfile())
  on.exit(unlink(tmp.dir, TRUE, TRUE))
  config.file <- file.path(tmp.dir, "config.yaml")
  cache <- cachemer$new(path = config.file)
  
  cache$setLogger(TRUE)
  
  # when saving in separate process sometimes rds files may be saved 
  # with delay - I want to avoid it in tests
  cache$setForceEval(TRUE)
  
  res1 %c-% testFun(1:23, b = 1, list(d = 2, e = 3))
  res2 %c-% testFun(1:23, b = 2, list(d = 2, e = 3))
  res3 %c-% testFun(1:23, b = 3, list(d = 2, e = 3))
  
  smry <- cache$summary()
  
  cache$clear()
  testthat::expect_length(list.files(tmp.dir), 4)
  testthat::expect_null(cache$lastCache)
  
  # restore session
  cache <- cachemer$new(path = config.file)
  
  testthat::expect_equal(cache$summary(), smry)
  
})

testthat::test_that("saveCache: new cached elements after clear", {
  dir.create(tmp.dir <- tempfile())
  on.exit(unlink(tmp.dir, TRUE, TRUE))
  config.file <- file.path(tmp.dir, "config.yaml")
  cache <- cachemer$new(path = config.file)
  
  cache$setLogger(TRUE)
  # when saving in separate process sometimes rds files may be saved 
  # with delay - I want to avoid it in tests
  cache$setForceEval(TRUE)
  
  res1 %c-% testFun(1:23, b = 1, list(d = 2, e = 3))
  res2 %c-% testFun(1:23, b = 2, list(d = 2, e = 3))
  
  testthat::expect_length(list.files(tmp.dir), 3)
  
  smry <- cache$summary()
  
  cache$clear()
  
  res1 %c-% testFun(1:23, b = 1, list(d = 2, e = 3))
  res2 %c-% testFun(1:23, b = 2, list(d = 2, e = 3))
  
  testthat::expect_length(list.files(tmp.dir), 5)
  
  # restore session
  cache <- cachemer$new(path = config.file)
  
  testthat::expect_equal(cache$summary(), smry)
  
})