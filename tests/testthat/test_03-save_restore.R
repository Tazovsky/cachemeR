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