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
  chain <- match.call()
  
  if (!is.call(chain[[3]]))
    stop("RHS is not a function. If assigning value, please use `<-` operator.")
  
  fun <- get(fun.name, envir = envir)
  if (!is.function(fun))
    stop("Supports functions caching only.")

  if (fun.name == "$")
    stop("Extraction with `$` is not supported on RHS.")
  
  value.args <- getArgs(value = expr, eval.calls = TRUE)
  cache <- cachemerRef$new()
  cache$cacheme(
    fun.name = fun.name,
    fun.body = functionBody(fun),
    arguments = value.args,
    output = expr,
    envir =  envir
  )

  result <- cache$lastCache$output

  eval(call("<-", target, result), envir, envir)
}


if (FALSE) {
  res %c-% testFun(a = 1:13, b = 666, c = list(d = 3, e = 0))
  res %c-% testFun()

  res.old %c-% doLm(rows = 5000, cols = 1000)
  res.new %c-% doLm(rows = 5000, cols = 1000)

  res.new %c-% doLm(rows = 5000, 1000)

  bench <- microbenchmark::microbenchmark(
    res <- doLm(rows = 5000, cols = 1000),
    res.cached %c-% doLm(rows = 5000, cols = 1000),
    times = 30L
  )


  testthat::expect_equal(res, res.cached)
}
