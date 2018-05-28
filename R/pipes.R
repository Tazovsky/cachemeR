#'  \%c-\%
#'
#' cacheR pipe operator
#'
#' @param x variable to assign output from `value`
#' @param value to be assigned to `x`
#'
#' @rdname pipe
cache <- function(x, value) {
  target <- substitute(x)
  # expr <- lazyeval::lazy(value)
  expr <- substitute(value)
  envir <- parent.frame(1)

  print("before evaluating")
  result <- eval(expr)

  # cache <- cacherRef$new()

  # cache$share(deparse(target), result)

  assign(deparse(target), result, envir = envir)
}

#' @rdname pipe
#' @export
`%c-%` <- cache
