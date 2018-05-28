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
#'
#' @rdname test-functions
testFun <- function(a = 1:20,
                    b = 0,
                    c = list(d = 3, e = 5)) {
  print(">> run")
  x %>% sin %>% sum
}

#' @rdname test-functions
#' @export
testFun2 <- function() 1 + 3
