
#' getArgs
#'
#' @param value function call
#' @param eval.calls logical
#'
#' @return list
#' @export
#'
#' @examples \dontrun{
#' testFun <- function(a = 1:20, b = list(c = 3, d = 5)) {
#'   x %>% sin %>% sum
#' }
#' getArgs(testFun())
#' }
getArgs <- function(value, eval.calls = TRUE) {

  if (inherits(value, "call"))
    qte <- quote(value)
  else
    qte <- quote(substitute(value))

  qte <- eval(qte)

  arg.nm <- setdiff(names(qte), c("", "eval.calls"))

  res.custom.args <- lapply(arg.nm, function(arg) qte[[arg]])
  names(res.custom.args) <- arg.nm

  if (inherits(value, "call"))
    res.default.args <- formals(deparse(value[[1]]))
  else
    res.default.args <- formals(deparse(substitute(value)[[1]]))

  common.args <- intersect(names(res.custom.args), names(res.default.args))

  if (length(common.args) > 0) {
    res <- c(res.custom.args[names(res.custom.args) %in% common.args],
             res.default.args[!names(res.default.args) %in% common.args])
  } else {
    res <- res.default.args

    ## TODO: handle case when function has unnamed arguments
    # res <- if (class(res.default.args) == "pairlist") as.list(res.default.args) else res.default.args
    #
    # is.empty <- lapply(res.default.args, function(x) x == "")
    #
    # if (length(is.empty) > 0) {
    #   not.empty.args <- res[!unlist(is.empty)]
    #   empty.args <- res[unlist(is.empty)]
    #
    #   qte.list <- as.list(qte)
    #
    #   fun.arguments <- lapply(qte.list, eval)
    #   fun.arguments[[1]] <- NULL # remove function
    # }

  }

  if (eval.calls)
    lapply(res, function(x) if (inherits(x, "call")) eval(x) else x)
  else
    res
}
