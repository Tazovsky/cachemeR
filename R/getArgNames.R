
#' getArgs
#'
#' @param value
#' @param eval.calls
#'
#' @return
#' @export
#'
#' @examples
getArgs <- function(value, eval.calls = TRUE) {

  qte <- quote(substitute(value))

  qte <- eval(qte)

  arg.nm <- setdiff(names(qte), c("", "eval.calls"))

  if (!is.null(arg.nm) && length(arg.nm) > 0) {
    res <- lapply(arg.nm, function(arg) qte[[arg]])
    names(res) <- arg.nm
    res
  } else {
    res <- formals(deparse(substitute(value)[[1]]))
  }


  if (eval.calls)
    lapply(res, function(x) if (inherits(x, "call")) eval(x) else x)
  else
    res
}


testFun <- function(x = 1:20,
                    b = 0,
                    c = list(a=2, b=3)) {
  print(">> run")
  x %>% sin %>% sum
}

testFun2 <- function(x = 1:20,
                    b = 0,
                    c = list(a=2, b=3)) x %>% sin %>% sum


getArgNames <- function(value) {
  env <- parent.frame()
  expr <- substitute(value)
  browser()
  # function should return all arguments names - in this case c("x", "z")

}

vec <- 1:50


expr <- substitute(testFun(a = vec))

base::

deparse(expr)

deparse(quote(testFun()))

deparse(testFun)



substitute(expr)


deparse()

f <- formalArgs(testFun)


formalArgs(testFun())

formals(f)
args(testFun)

fun.name <- gsub("\\(|\\)", "", deparse(substitute(testFun())))

formalArgs(get(fun.name, envir = .GlobalEnv))


q <- quote(testFun(a = vec, b = 1, c = list(c=3,d=4)))
# q <- quote(testFun(a = vec))


res <- lapply(names(q)[-1], function(arg) q[[arg]])

deparse(q)
names(q)


getArgNames <- function(value) formalArgs(deparse(substitute(value)[[1]]))

arg.nm <- getArgNames(testFun(x = 1:20, b = 0, c = list(a=2, b=3)))
getArgNames(testFun2())



qte <- quote(testFun(a = vec, b = 1, c = list(c=3,d=4)))

qte2 <- quote(testFun())

arg.nm <- names(qte)[-1]
res <- lapply(arg.nm, function(arg) q[[arg]])
names(res) <- arg.nm

getArgNamesAndValues <- function(value) {

  qte <- quote(substitute(value))

  qte <- eval(qte)

  arg.nm <- names(qte)[-1]

  if (!is.null(arg.nm) && length(arg.nm) > 0) {
    res <- lapply(arg.nm, function(arg) qte[[arg]])
    names(res) <- arg.nm
    res
  } else {
    res <- formals(deparse(substitute(value)[[1]]))
  }

  res
}

x <- getArgNamesAndValues(testFun())
x

debugonce(getArgNamesAndValues)
y <- getArgNamesAndValues(testFun(a = vec, b = 2, c = list(c=3,d=5)))
y


