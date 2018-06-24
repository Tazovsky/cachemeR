testthat::context("test `%c%`")

testthat::test_that("split_chain(), getChainArgs()", {
  
  res <- split_chain(quote(3 %>% fun1() %>% fun2()), env = parent.frame(1))
  args <- getChainArgs(res)
  
  # saveRDS(res, "inst/testdata/split_chain.RDS")
  # saveRDS(args, "inst/testdata/getChainArgs.RDS")
  testthat::expect_equal(readRDS(system.file("testdata", "split_chain.RDS", package = "cachemeR")), res)
  testthat::expect_equal(readRDS(system.file("testdata", "getChainArgs.RDS", package = "cachemeR")), args)
})

