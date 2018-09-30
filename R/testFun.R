#' testFun
#'
#' Test function
#'
#' @param a numeric vector
#' @param b numeric value
#' @param c list
#'
#' @return numeric
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom futile.logger flog.debug
#' @rdname test-functions
testFun <- function(a = 1:20,
                    b = 0,
                    c = list(d = 3, e = 5)) {
  flog.debug("Function have just been evaluated")
  res <- a %>% sin %>% sum
  res <- (res + b) ^ c$d / c$e
  res
}

#' @rdname test-functions
#' @export
#' @rdname test-functions
testFun2 <- function() 1 + 3


#' doLm
#'
#' @param rows integer
#' @param cols integer
#'
#' @return model
#' @export
#'
#' @importFrom stats lm rnorm runif
#' @rdname test-functions
doLm <- function(rows, cols) {
  set.seed(1234)
  X <- matrix(rnorm(rows*cols), rows, cols)
  b <- sample(1:cols, cols)
  y <- runif(1) + X %*% b + rnorm(rows)
  model <- lm(y ~ X)
}

#' calculateFibonacci
#'
#' @param nterms integer
#'
#' @return vector
#' @export
#' @rdname test-functions
calculateFibonacci <-  function(nterms) {
  n1 = 0
  n2 = 1
  count = 2
  # check if the number of terms is valid
  res <- c()
  
  if(nterms <= 0) {
    stop("Plese enter a positive integer")
  } else {
    if(nterms == 1) {
      n1
    } else {
      while(count < nterms) {
        nth = n1 + n2
        res <- c(res, nth)
        # update values
        n1 = n2
        n2 = nth
        count = count + 1
      }
    }
  }
  res
}

