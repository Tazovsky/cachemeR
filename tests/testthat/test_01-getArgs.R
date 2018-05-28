testthat::context("getArgs()")

testthat::test_that("get default arguments", {

  testthat::expect_equal(getArgs(testFun(), eval.calls = TRUE),
                         list(
                           a = 1:20,
                           b = 0,
                           c = list(d = 3, e = 5)
                         ))
})

testthat::test_that("get custom arguments", {
  testthat::expect_equal(
    getArgs(testFun(
      a = 1:10,
      b = 3,
      c = list(d = 21, e = 12),
      eval.calls = TRUE
    )),
    list(
      a = 1:10,
      b = 3,
      c = list(d = 21, e = 12)
    ))
})

testthat::test_that("get mixed arguments: custom and default", {
  testthat::expect_equal(
    names(getArgs(testFun(
      a = 1:13,
      b = 4,
      eval.calls = TRUE
    ))),
    c("a", "b", "c"))

  testthat::expect_equal(
    getArgs(testFun(
      a = 1:13,
      b = 4,
      eval.calls = TRUE
    )),
    list(
      a = 1:13,
      b = 4,
      c = list(d = 3, e = 5)
    ))
})

testthat::test_that("function with no argument", {
  testthat::expect_equal(getArgs(testFun2()), list())
})

