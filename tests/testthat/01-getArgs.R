library(dplyr)

testFun <- function(a = 1:20,
                    b = 0,
                    c = list(d = 3, e = 5)) {
  print(">> run")
  x %>% sin %>% sum
}

testthat::context("getArgs()")

test_that("get default arguments", {

  testthat::expect_equal(getArgs(testFun(), eval.calls = TRUE),
                         list(
                           a = 1:20,
                           b = 0,
                           c = list(d = 3, e = 5)
                         ))
})

test_that("get custom arguments", {
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
