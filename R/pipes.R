#'  \%c-\%
#'
#' cacheR pipe operator
#'
#' @param x variable to assign output from `value`
#' @param value to be assigned to `x`
#' @export
#' @rdname pipe
`%c-%` <- function(x, value) {
  target <- substitute(x)
  # expr <- lazyeval::lazy(value)
  expr <- substitute(value)
  envir <- parent.frame(1)

  browser()

  value.args <- getArgs(expr)






  print("before evaluating")
  result <- eval(expr)

  # cache <- cacherRef$new()

  # cache$share(deparse(target), result)

  assign(deparse(target), result, envir = envir)
}


# debugonce(cache)
# res %c-% testFun(a = 1:20, b = 0, c = list(d = 3, e = 5))

if (FALSE) {
  # debugonce(`%c-%`)
  # cacheR::`%c-%`()
  res %c-% testFun(a = 1:13, b = 666, c = list(d = 3, e = 0))




}
