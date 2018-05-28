#' testFun
#'
#' Test function
#'
#' @param x vector
#' @param b numeric
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
  res <- (res + 2)^2/5
}

#' @rdname test-functions
#' @export
#' @rdname test-functions
testFun2 <- function() 1 + 3


#' testLm
#'
#' @param rows
#' @param cols
#'
#' @return model
#' @export
#'
#' @rdname test-functions
testLm <- function(rows, cols) {
  set.seed(1234)
  X <- matrix(rnorm(rows*cols), rows, cols)
  b <- sample(1:cols, cols)
  y <- runif(1) + X %*% b + rnorm(rows)
  model <- lm(y ~ X)
}
