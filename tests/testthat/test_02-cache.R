testthat::context("pipe output")

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

  testthat::expect_equal(ref.res4, res4)
})
