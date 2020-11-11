#'  \%c-\%
#'
#' cacheR pipe operator
#'
#' @param x variable to assign output from `value`
#' @param env environment
#' @param value to be assigned to `x`
#'
#' @export
#' @importFrom methods functionBody
#' @rdname pipe
`%c-%` <- function(x, value, env = parent.frame()) {
  target <- substitute(x)
  expr <- substitute(value)
  fun.name <- as.character(expr)[1]
  chain <- match.call()
  
  if (!is.call(chain[[3]]))
    stop("RHS is not a function. If assigning value, please use `<-` operator.")
  
  fun <- get(fun.name, envir = env)
  if (!is.function(fun))
    stop("Supports functions caching only.")

  if (fun.name == "$")
    stop("Extraction with `$` is not supported on RHS.")
  
  value.args <- getArgs(value = expr, eval.calls = TRUE, env = env)
  cache <- cachemerRef$new()
  cache$cacheme(
    fun.name = fun.name,
    fun.body = methods::functionBody(fun),
    arguments = value.args,
    output = expr,
    envir =  env
  )

  result <- cache$lastCache$output

  eval(call("<-", target, result), env, env)
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
