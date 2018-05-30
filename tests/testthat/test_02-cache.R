testthat::context("pipe output")

dir.create(tmp.dir <- tempfile())
on.exit(unlink(tmp.dir, TRUE, TRUE))

testthat::test_that("init cache object", {
  testthat::expect_true ({
    config.file <- file.path(tmp.dir, "config.yaml")
    cache <- cachemer$new(path = config.file)
    TRUE
  })
})

testthat::test_that("test output: all args are named", {

  ref.res1 <- testFun(a = 1:20, b = 0, c = list(d = 3, e = 5))
  res1 %c-% testFun(a = 1:20, b = 0, c = list(d = 3, e = 5))

  testthat::expect_equal(ref.res1, res1)
})

testthat::test_that("test output: named and unnamed args", {

  ref.res2 <- testFun(1:13, b = 2, list(d = 3, e = 5))
  res2 %c-% testFun(1:13, b = 2, list(d = 3, e = 5))

  testthat::expect_equal(ref.res2, res2)
})

testthat::test_that("test output: all args are default", {
  ref.res3 <- testFun()
  res3 %c-% testFun()

  testthat::expect_equal(ref.res3, res3)
})

testthat::test_that("test output: all args are unnamed", {
  ref.res4 <- testFun(1:77, 3, list(d = 4, e = 2))
  res4 %c-% testFun(1:77, 3, list(d = 4, e = 2))

  cache <- cachemerRef$new()

  testthat::expect_equal(ref.res4, res4)
})

testthat::context("test R6 object and methods")

testthat::test_that("method: lastCache", {

  ref.res <- testFun(a = 1:100, 1, list(d = 2, e = 3))
  res %c-% testFun(a = 1:100, 1, list(d = 2, e = 3))

  cache <- cachemer$new()

  testthat::expect_equal(ref.res, cache$lastCache$output)
  testthat::expect_equal(list(a = 1:100, b = 1, c =list(d = 2, e = 3)),
                         cache$lastCache$arguments)
  testthat::expect_equal("5cb5a1cb70981907b91d2de4b127b762",
                         cache$lastCache$hash)
})
