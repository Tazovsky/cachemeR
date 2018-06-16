
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
  is.call(x) && is.function(get(as.character(x)[1], env))
}

`%c%` <- function(x, value = NULL) {
  target <- substitute(x)
  expr <- substitute(value)
  envir <- parent.frame(1)
  fun.name <- as.character(expr)[1]
  chain <- match.call()
  env = new.env(parent = envir)
  lhs <- chain[[2]]
  
  split_chain_ <- function(el, env) {
    
    res <- list(pipes = NULL, functions = NULL, values = NULL, others = NULL)
    
    if (is_pipe(el))
      res[["pipes"]][[i]] <- c(res[["pipes"]][[i]], el)
    else if (is_function(el, env))
      res[["functions"]][[i]] <- c(res[["functions"]][[i]], el)
    else
      res[["others"]][[i]] <- c(res[["others"]][[i]], el)
    
    
    res
  }
  
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
        res[["functions"]][[idx]] <- el
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
  
  spr <- split_chain(lhs, env)
  
  fun2check <- spr$functions[[1]][[1]]
  
  browser()
  
  args <- getArgs(fun2check)
  
}







x <- 3 %>% fun1() %>% fun2() %c% .


