
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

  qte.list <- as.list(qte)
  fun.name <- qte.list[[1]]
  qte.list[[1]] <- NULL

  arg.nm <- setdiff(names(qte.list), c("eval.calls"))

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
  }

  if (!is.null(res) &&
      length(res) > 0 &&
      !is.null(qte.list) &&
      length(qte.list) > 0) {

    # may hapen when some arguments are evaluates with their name and some not
    # eg. res %c-% testLm(rows = 5000, 1000)
    mergeNamedAndUnnamed <- function(res, qte.list) {
      for (i in 1:length(res)) {
        if (res[i] == "" && names(qte.list[i]) == "")
          res[[i]] <- qte.list[[i]]
      }
      res
    }

    res <- mergeNamedAndUnnamed(res, qte.list)
  }

  if (eval.calls)
    lapply(res, function(x) if (inherits(x, "call")) eval(x) else x)
  else
    res
}
