#'  \%keepfds\%
#'
#' Keep LHS attribute "fds" if doesn't exist in RHS
#'
#' @param lhs left hand side of assignment
#' @param rhs right hand side of assignment
#' @export
#' @rdname pipe
`%keepfds%` <- function(lhs, rhs) {
  target <- substitute(lhs)
  val <- substitute(rhs)
  envir <- parent.frame()
  val.eval <- eval(val, envir = envir)
  # target.eval <-  eval(target, envir = envir)
  target.dp <- deparse(target)
  chain <- match.call()
  pipe <- chain[[1]]
  lhs.el <- chain[[2]]
  rhs.el <- chain[[3]]

  browser()

  ## variable does not exist
  if (exists(deparse(target), envir = envir) && isList(lhs, envir)) {
    # parse list to get path to element to be assigned to
    list.parsed <- parseList(lhs, envir)

    # get whole list object
    lhs.obj.nm <- "lhs.obj"
    target.nm <- list.parsed[1]
    assign(lhs.obj.nm, get(target.nm, envir = envir))

    # create expresion to assign value automatically
    expr <- paste0(lhs.obj.nm, paste0("[['", list.parsed[-1], "']]", collapse = ""), " <- ", "val.eval")
    eval(parse(text = expr)) # assign RHS value

    lhs.obj.attr <- attr(get(lhs.obj.nm), "fds")


    assign(target.nm, val.eval, envir = envir)

  } else if (!exists(deparse(target), envir = envir)) {
    assign(deparse(target), val.eval, envir = envir)
  } else if (!is.null(attr(val.eval, "fds"))) {
    assign(deparse(target), val.eval, envir = envir)
  } else {

    target.eval <- eval(target)
    target.attr <- attributes(target.eval)

    if (is.null(target.attr$fds))
      warning("LHS is missing argument 'fds'")
    else
      attr(val.eval, "fds") <- target.attr$fds

    assign(deparse(target), val.eval, envir = envir)
  }
}

isList <- function(lhs, envir) {
  p.envir <- parent.frame()
  sb <- substitute(lhs, env = p.envir)
  if (sb[[1]] == "$") {
    is.list(get(as.character(sb[[2]][[2]]), envir = envir))
  } else {
    FALSE
  }
}

parseList <- function(lhs, envir) {
  p.envir <- parent.frame()
  sb <- substitute(lhs, env = p.envir)

  parsed <- as.character(deparse(sb))

  unlist(strsplit(parsed, "\\$"))
}
