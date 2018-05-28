

# init
init <- cacher$new()


library(listenv)
library(dplyr)
testFun <- function(x = 1:20,
                    b = 0,
                    c = list(a=2, b=3)) {
  print(">> run")
  x %>% sin %>% sum
}

`%c-%` <- function(x, value) {
  target <- substitute(x)
  # expr <- lazyeval::lazy(value)
  expr <- substitute(value)
  envir <- parent.frame(1)

  fun.name <- gsub("\\(|\\)", "", deparse(substitute(testFun())))
  args <- formalArgs(get(fun.name, envir = envir))
  browser()




  print("before evaluating")
  result <- eval(expr)

  # cache <- cacherRef$new()

  # cache$share(deparse(target), result)

  assign(deparse(target), result, envir = envir)
}


az %c-% testFun()


getArgNames <- function(value) formalArgs(deparse(substitute(value)[[1]]))

res <- getArgNames(testFun())


cache <- cacherRef$new()

dp <- deparse(testFun)

sliced <- gsub("\\s|^function.\\(|\\).$", "", dp[1])
paste0(sliced, ",")
testFun

