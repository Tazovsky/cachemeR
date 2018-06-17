
# flog.threshold(DEBUG, name = "mylog")
# flog.debug("sadasads", name = "mylog")
# 
# flog.threshold(WARN, name = "mylog")
# 
# flog.remove("mylog")


fun1 <- function(x, multiplier = 3) {
  print("fun1")
  x * multiplier
}
fun2 <- function(x) {
  print("fun2")
  x
}
fun3 <- function(x, multiplier = 2) {
  print("fun3")
  x * 2
}


is_pipe <- function(x) {
  grepl("%.*%", x)
}

is_function <- function(x, env) {
  is.call(x) && is.function(get(as.character(x)[1], env)) ||
  is.name(x) && is.function(get(as.character(x), env))
}

`%c%` <- function(x, value = NULL) {
  target <- substitute(x)
  expr <- substitute(value)
  envir <- parent.frame(1)
  fun.name <- as.character(expr)[1]
  chain <- match.call()
  env = new.env(parent = envir)
  lhs <- chain[[2]]
  
  split_chain <- function(x, env) {
    
    if (missing(env))
      stop("Missing argument 'env'")
    
    if (missing(x))
      stop("Missing argument 'x'")
    
    res <- list(pipes = list(), functions = list(), values = list(), others = list())
    
    for (i in 1:length(x)) {
      
      el <- x[[i]]
      
      if (length(el) > 1) {
        
        splitted <- split_chain(el, env)
        
        for (nm in names(res)) {
          if (!is.null(splitted[[nm]])) {
            idx <- length(res[[nm]]) + 1
            
            if (length(splitted[[nm]]) > 0 )
              res[[nm]][[idx]] <- splitted[[nm]][[1]]
            
          }
        }
        
      } else if (is_pipe(el)) {
        idx <- length(res[["pipes"]]) + 1
        res[["pipes"]][[idx]] <- el
      } else if (is_function(el, env)) {
        idx <- length(res[["functions"]]) + 1L
        res[["functions"]][[idx]] <- list(name = el)
      } else if (is.numeric(el)) {
        idx <- length(res[["values"]]) + 1L
        res[["values"]][[idx]] <- el
      } else {
        idx <- length(res[["others"]]) + 1L
        res[["others"]][[idx]] <- el
      }
      
    }
    res
  }
  
  chain_parts <- split_chain(lhs, env)
  
  getChainArgs <- function(chain_parts) {
  # get functions arguments
  for (i in 1:length(chain_parts$functions)) {
    
    fun2check <- chain_parts$functions[[i]]$name
    
    if (!is.null(fun2check)) {
      cl <-
        call(
          "getArgs",
          # if arg is not declared then "getArgs()" fails and should be wrapped in "substitute"
          call("substitute", fun2check),
          eval.calls = TRUE,
          allow.non.eval = TRUE
        )
      
      args <- eval(cl)
      
      # assign arguments
      chain_parts$functions[[i]]$args <- args
    }
    
  }
    chain_parts 
  }
  
  
  # TODO#1: cache objects here
  # TODO#2: if object exists in cache then return its output value, else cache new object
  browser()
  
  return(chain_parts)
}

testthat::test_that("split_chain(), getChainArgs()", {
  
  res <- split_chain(quote(3 %>% fun1() %>% fun2()), env = parent.frame(1))
  args <- getChainArgs(res)
  
  # saveRDS(res, "inst/testdata/split_chain.RDS")
  # saveRDS(args, "inst/testdata/getChainArgs.RDS")
  
  testthat::expect_equal(readRDS("inst/testdata/split_chain.RDS"), res)
  testthat::expect_equal(readRDS("inst/testdata/getChainArgs.RDS"), args)
})

testthat::test_that("`%c%`: one LHS function output", {
  
  res.ref <- list(x = "", multiplier = 3)
  res <- fun1(3) %c% .
  
  testthat::expect_true(res.ref$x == res$x)
  testthat::expect_equal(res.ref$multiplier, res$multiplier)
})