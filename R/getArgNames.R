
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

  p.env <- parent.frame(1) # parent envir
  gp.env <- parent.frame(2) # granparent envir
  
  if (inherits(value, "call"))
    qte <- quote(value)
  else
    qte <- quote(substitute(value))

  qte <- eval(qte)

  qte.list <- as.list(qte)

  fun.name <- qte.list[[1]]
  qte.list[[1]] <- NULL
  
  # check if argument values are named variables
  qte.list <- lapply(qte.list, function(x) {
    if (class(x) == "name")
      eval(x, envir = gp.env)
    else
      x
  })

  arg.nm <- setdiff(names(qte.list), c("eval.calls"))

  res.custom.args <- lapply(arg.nm, function(arg) qte[[arg]])
  names(res.custom.args) <- arg.nm

  # have to handle case when custom argument is named variable
  assign.evaluated.arg <- function(res.custom.args, qte.list) {
    for (nm in names(res.custom.args)) {
      el <- res.custom.args[[nm]]
      # if argument is type 'name' it means that this is named argument
      # so evaluated argument (its value) should be assigned to custom args
      if (class(el) == "name" && !is.null(qte.list[[nm]])) {
        res.custom.args[[nm]] <- qte.list[[nm]]
      } 
    }
    return(res.custom.args)
  }

  res.custom.args <- assign.evaluated.arg(res.custom.args, qte.list) 
  
  if (inherits(value, "call"))
    res.default.args <- formals(deparse(value[[1]]))
  else
    res.default.args <- formals(deparse(substitute(value)[[1]]))

  common.args <- intersect(names(res.custom.args), names(res.default.args))

  if (length(common.args) > 0) {
    res <- c(res.custom.args[names(res.custom.args) %in% common.args],
             res.default.args[!names(res.default.args) %in% common.args])

    # correct order of elements
    res <- res[names(res.default.args)]
  } else {
    res <- res.default.args
  }

  if (!is.null(res) &&
      length(res) > 0 &&
      !is.null(qte.list) &&
      length(qte.list) > 0) {

    # may hapen when some arguments are evaluates with their name and some not
    # eg. res %c-% testLm(rows = 5000, 1000)



    mergeNamedAndUnnamed <- function(res, qte.list, common.args) {
      for (i in 1:length(res)) {

        if ((!is.null(res[i]) && res[i] == "" && !is.null(qte.list[i]) && names(qte.list[i]) == "") ||
            # when one common argument and still some unnamed
            (
              !is.null(qte.list[i][[1]]) &&
              !is.null(qte.list[i]) &&
              !is.null(names(qte.list[i])) && names(qte.list[i]) == "" && !names(res[i]) %in% common.args
            )
        )
          res[[i]] <- qte.list[[i]]
      }
      res
    }

    # if all namees of qte.list are NULL then all argument were provided without naming them
    # and point is just to get names from function's default arguments
    if (is.null(names(qte.list)) && !is.null(names(res))) {
      names(qte.list) <- names(res)
      res <- qte.list
    } else {
      res <- mergeNamedAndUnnamed(res, qte.list, common.args)
    }
  }
  
  if (eval.calls)
    res <- lapply(res, function(x) if (inherits(x, "call")) eval(x) else x)
  
  # all argument must be evaluated and be values
  is.non.evaluated <- sapply(res, class) %in% c("name", "call")
  
  if (any(is.non.evaluated)) {
    elem <- sapply(res, class)[is.non.evaluated]
    msg <- sprintf("Non evaluated argument(s): %s ",
                   paste0(names(elem), ": ", elem, collapse = ", "))
    stop(msg)
  }
  
  return(res)
}
