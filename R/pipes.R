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
  expr <- substitute(value)
  envir <- parent.frame(1)
  fun.name <- as.character(expr)[1]

  if (!is.function(get(fun.name, envir = envir)))
    stop("Supports functions caching only.")

  value.args <- getArgs(expr)
  obj <- cacherRef$new()
  obj$cacheme(fun.name, value.args, expr)

  result <- obj$lastCache$output

  assign(deparse(target), result, envir = envir)
}


if (FALSE) {
  # debugonce(`%c-%`)
  # cacheR::`%c-%`()
  res %c-% testFun(a = 1:13, b = 666, c = list(d = 3, e = 0))


}
