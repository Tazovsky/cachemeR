
#' isListArg
#'
#' @param x obj of class `call` or `name``
#' @param envir environment in wchich
#'
#' @return logical
#' @export
isListArg <- function(x, envir) {
  
  # stopifnot(class(x) %in% c("call", "name"))
  
  p.envir <- parent.frame()
  
  sb <- if (!class(x) == "call") substitute(x, env = p.envir) else x
  
  # handle case when x is nested list, for example: x$y$z
  sb.char <- as.character(sb)
  
  sb.char.unl <- 
    unlist(sapply(sb.char, function(x) if (x == "$") x else strsplit(x, "\\$")))
  
  if (sb.char.unl[[1]] == "$") {
    ## because dollar is first:
    # Browse[4]> sb
    # z$x
    # Browse[4]> as.character(sb)
    # [1] "$" "z" "x"
    is.list(get(sb.char.unl[[2]], envir = envir))
  } else {
    FALSE
  }
}

if (FALSE) {
  debugonce(isArgList)
  isArgList(res[[3]], gp.env)
}



#' parseList
#'
#' @param x object of class `call` or `name`
#' @param envir environment
#'
#' @return vector
#' @export
#'
parseList <- function(x, envir) {
  p.envir <- parent.frame()
  
  sb <- if (!class(x) == "call") substitute(x, env = p.envir) else x
  
  parsed <- as.character(deparse(sb))
  
  unlist(strsplit(parsed, "\\$"))
}

#' getListArg
#'
#' @param x object of class `call`
#' @param envir environment
#'
#' @return list/value
#' @export
#'
getListArg <- function(x, envir) {
  
  stopifnot(isListArg(x, envir = envir))
  
  # parse list to get path to element to be assigned to
  list.parsed <- parseList(x, envir)
  
  # get whole list object
  lhs.obj.nm <- "lhs.obj"
  target.nm <- list.parsed[1]
  assign(lhs.obj.nm, get(target.nm, envir = envir))
  
  # create expresion to assign value automatically
  expr <- paste0(lhs.obj.nm, paste0("[['", list.parsed[-1], "']]", collapse = ""))
  
  res <- eval(parse(text = expr)) # get value of list
  
  return(res)
}

#' evalOutput
#' 
#' Evaluate `call` expression even when some elements are listss
#' 
#' @param output object of class `call`
#' @param envir environment
#'
#' @return value of `call` expression
#' @export
#'
evalOutput <- function(output, envir = parent.frame()) {
  stopifnot(class(output) == "call")
  
  # if any argument is list, then get its value before evaluating function
  for (i in 1:length(output)) {
    if (isListArg(output[[i]], envir = envir)) {
      output[[i]] <- getListArg(output[[i]], envir = envir)
    }
  }
  
  eval(output, envir = envir)
}