#' \%c\%
#'
#' @param x 
#' @param value 
#'
#' @return
#' @export
#' @importFrom utils getFromNamespace
#' @examples
`%c%` <- function(x, value = NULL) {
  target <- substitute(x)
  expr <- substitute(value)
  envir <- parent.frame(1)
  fun.name <- as.character(expr)[1]
  chain <- match.call()
  env = new.env(parent = envir)
  lhs <- chain[[2]]
  
  lhs2splot <- if (is_parenthesized(lhs)) lhs[[-1]] else lhs # rm parenthesis
  
  browser()
  
  chain_parts <- split_chain(lhs2splot, env)
  
  args <- getChainArgs(chain_parts)
  
  # TODO#1: cache objects here
  # TODO#2: if object exists in cache then return its output value, else cache new object
  browser()
  
  return(chain_parts)
}

#' split_chain
#' 
#' Split \code{\link[base]{match.call}} into elements like 
#' \code{functions}, \code{pipes}, \code{values} or \code{others}
#' 
#' @param x 
#' @param env 
#'
#' @return list
#' @export
#'
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

#' getChainArgs
#' 
#' Get arguments of function from \code{\link[cachemeR]{split_chain}} output
#'
#' @param chain_parts \code{\link[cachemeR]{split_chain}} output
#'
#' @return list
#' @export
#'
getChainArgs <- function(chain_parts) {
  # get functions arguments
  for (i in 1:length(chain_parts$functions)) {
    
    fun2check <- chain_parts$functions[[i]]$name
    
    if (!is.null(fun2check)) {
      cl <-
        call(
          "getArgs",
          # if arg is not declared then "getArgs()" fails and should be 
          # wrapped in "substitute" to avoid that
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

## test functions
#' @noRd
fun1 <- function(x, multiplier = 3) {
  print("fun1")
  x * multiplier
}
#' @noRd
fun2 <- function(x) {
  print("fun2")
  x
}
#' @noRd
fun3 <- function(x, multiplier = 2) {
  print("fun3")
  x * 2
}

# temporery checks:
if (FALSE) {
  res <- (fun1(3) + 1) %c% .
  
  # ------------------------- unit tests
  
  testthat::test_that("`%c%`: one LHS function output", {
    
    res.ref <- list(x = "", multiplier = 3)
    res <- fun1(3) %c% .
    
    testthat::expect_true(res.ref$x == res$x)
    testthat::expect_equal(res.ref$multiplier, res$multiplier)
  })
  
}
