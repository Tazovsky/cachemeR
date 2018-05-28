
#' getArgs
#'
#' @param value
#' @param eval.calls
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' testFun <- function(a = 1:20, b = list(c = 3, d = 5)) {
#'   x %>% sin %>% sum
#' }
#' getArgs(testFun())
#' }
getArgs <- function(value, eval.calls = TRUE) {

  qte <- quote(substitute(value))

  qte <- eval(qte)

  arg.nm <- setdiff(names(qte), c("", "eval.calls"))

  res.custom.args <- lapply(arg.nm, function(arg) qte[[arg]])
  names(res.custom.args) <- arg.nm

  res.default.args <- formals(deparse(substitute(value)[[1]]))

  common.args <- intersect(names(res.custom.args), names(res.default.args))

  if (length(common.args) > 0)
    res <- c(res.custom.args[names(res.custom.args) %in% common.args],
             res.default.args[!names(res.default.args) %in% common.args])
  else
    res <- res.default.args

  if (eval.calls)
    lapply(res, function(x) if (inherits(x, "call")) eval(x) else x)
  else
    res
}
