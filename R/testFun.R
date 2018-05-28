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
#' @examples testFun()
testFun <- function(x = 1:20,
                    b = 0,
                    c = list(a = 2, b = 3)) {
  print(">> run")
  x %>% sin %>% sum
}
